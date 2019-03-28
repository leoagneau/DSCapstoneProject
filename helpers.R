## Load libraries
require(readtext)
require(quanteda)
require(data.table)
require(stringr)
require(rdrop2)

## Define constants
k=5

## 1. Load the frequency table of unigrams, bigrams, trigrams
## The setDT method is used to add back pointer to the DT object
## https://stackoverflow.com/questions/31250999/r-readrds-load-fail-to-give-identical-data-tables-as-the-original
## https://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor

UniFreq <- NULL
BiFreq <- NULL
TriFreq <- NULL

Load_data <- function(corpora) {
  c <- paste(corpora, collapse = "")
  UniFile <- paste0("Uni_", c, "_nostop_nostem_cntgt2_all.RDS")
  BiFile <- paste0("Bi_", c, "_nostop_nostem_cntgt2_all.RDS")
  TriFile <- paste0("Tri_", c, "_nostop_nostem_cntgt2_all.RDS")
  if (!file.exists(UniFile))
    drop_download(paste0('RemoteData/', UniFile))
  if (!file.exists(BiFile))
    drop_download(paste0('RemoteData/', BiFile))
  if (!file.exists(TriFile))
    drop_download(paste0('RemoteData/', TriFile))
  
  UniFreq <<- setDT(readRDS(UniFile))
  BiFreq <<- setDT(readRDS(BiFile))
  TriFreq <<- setDT(readRDS(TriFile))
}

Prep_model <- function() {
  ## 2. Perform GT discounting on the counts
  ## 2a. Calculate the "frequency of frequency c" (N_c)
  CountNC <- function(FreqVec) {
    CountTbl <- table(FreqVec[,.(c)])
    return(data.table(cbind(c=as.integer(names(CountTbl)), Nc=as.integer(CountTbl))))
  }
  UniBins <- CountNC(UniFreq)
  BiBins <- CountNC(BiFreq)
  TriBins <- CountNC(TriFreq)
  
  ## 2b. Replace N_c with value computed from a linear regression that is fit to map N_c to c in log space
  ## log(N_c) = a + b*log(c)
  ## QUESTION: Should we use all the c-Nc pairs, or only use c<=k?
  FitLMFrmNc <- function(NcCountTbl) {
    return(lm(log(Nc) ~ log(c), data = NcCountTbl))
  }
  UniLM <- FitLMFrmNc(UniBins)
  BiLM <- FitLMFrmNc(BiBins)
  TriLM <- FitLMFrmNc(TriBins)
  rm(list=c("UniBins", "BiBins", "TriBins"))
  
  ## 2c. Only perform the discounting to small count (c) n-grams, where c <= k, using Katz's formula
  Cal_GTDiscount <- function(cnt, N) {
    if (N==1) {
      model <- UniLM
    } else if (N==2) {
      model <- BiLM
    } else if (N==3) {
      model <- TriLM
    }
    # Common parts
    N1 <- exp(predict(model, newdata=data.frame(c=1)))
    Nc <- exp(predict(model, newdata=data.frame(c=cnt)))
    Ncp1 <- exp(predict(model, newdata=data.frame(c=(cnt+1))))
    Nkp1 <- exp(predict(model, newdata=data.frame(c=(k+1))))
    
    # Common number
    sub <- ((k+1)*Nkp1)/(N1)
    newc <- ((cnt+1)*(Ncp1)/(Nc)-cnt*sub)/(1-sub)
    return(newc)
  }
  
  UpdateCount <- function(FreqTbl, N) {
    FreqTbl[c>k ,cDis:=as.numeric(c)]
    FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, N)]
  }
  UpdateCount(UniFreq, 1)
  UpdateCount(BiFreq, 2)
  UpdateCount(TriFreq, 3)
  setkey(UniFreq, term)
  setkey(BiFreq, term)
  setkey(TriFreq, term)
  rm(list=c("UniLM", "BiLM", "TriLM"))
}

## 3. Compute backoff probability of observed trigrams started with given bigram
## and backoff probability of observed bigrams started with given unigram

## Get the count given a word sequence
GetObsNGramsByPre <- function(wordseq, NgramFreq) {
  PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
  NgramFreq[grep(PreTxt, NgramFreq[,term], perl=T, useBytes=T),]
}

## Compute the probability
Cal_ObsProb <- function(ObsNgrams, Nm1Grams, wordseq) {
  PreCount <- Nm1Grams[wordseq, on=.(term), c]
  ObsNgrams[,Qbo:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}

## 4. Compute probability of unobserved trigrams started with chosen bigram
## 4a. Calculate the total probability mass taken out from observed bigrams, started with w_{i-1} (from 3b)
## 4b. Calculate the total probability mass taken out from observed trigrams, started with (w_{i-2},w_{i-1}) (from 3d)
Cal_RemainProbMass <- function(ObsNGrams, Nm1Grams, wordseq) {
  if (dim(ObsNGrams)[1] != 0) {
    # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
    return(sum(ObsNGrams[,c-cDis]/Nm1Grams[wordseq, c]))
  } else {
    return(1)
  }
}

## Find all the unigrams that end unobserved Ngrams
GetUnObsNgramTails <- function(ObsNgrams, N) {
  ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
  return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}

## 4c. Calculate backoff probabilities of all unobserved bigrams p_bo(w_i|w_i-1)
Cal_BiQbo <- function(UOBT, AlphaBi) {
  UnObsBiCnt <- UniFreq[UOBT, .(cDis), on="term"]
  TotalCntUnObsBi <- sum(UnObsBiCnt)
  return(AlphaBi * UnObsBiCnt / TotalCntUnObsBi)
}

## 4d. Calculate backoff probabilities of all unobserved trigrams p_bo(w_i|w_{i-2},w_{i-1})
Cal_TriQbo <- function(ObsBiG, ObsTriG, UnObsBiTails, AlphaTri) {
  # Qbo of bigrams which in ObsBiG but not in ObsTriG
  ObsBiQbo <- ObsBiG[which(!(ObsBiG[, term] %in% str_remove(ObsTriG[,term], "[^_]+_"))), .(term, Qbo)]
  ObsBiQbo[, term:=str_remove(ObsBiQbo[,term], "[^_]+_")]
  AllUnObsBiQbo <- rbind(UnObsBiTails, ObsBiQbo)
  SumUnObsBiQbo <- AllUnObsBiQbo[,sum(Qbo)]
  return(AllUnObsBiQbo[,Qbo:=AlphaTri*Qbo/SumUnObsBiQbo])
}

UNUSED_Cal_TriQbo <- function(AlphaTri, UnObsBiTails) {
  TotalQBo <- UnObsBiTails[, sum(Qbo)]
  return(AlphaTri *1)
}

## Call all the necessary functions
Preprocess <- function(wordseq) {
  names(wordseq) <- NULL
  quest <- wordseq %>% tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, remove_hyphens=T, remove_twitter=T, remove_url=T) %>% tokens_remove(stopwords("en")) %>% tokens_tolower()
  return(paste(tail(quest$text1, 2), collapse = " "))
}

Find_Next_word <- function(bigr, words_num) {
  bigr <- gsub(" ", "_", bigr)
  ## 3a. Get the discounted count of all observed bigrams started with given unigram w_{i-1}
  w_im1 <- str_split_fixed(bigr,"_", 2)[,2]
  ObsBiG <- GetObsNGramsByPre(w_im1, BiFreq)
  ## 3b. Get the discounted count of all observed trigrams started with given bigram w_{i-2},w_{i-1}
  ObsTriG <- GetObsNGramsByPre(bigr, TriFreq)
  ## 3c. Compute the probability
  Cal_ObsProb(ObsBiG, UniFreq, w_im1)
  Cal_ObsProb(ObsTriG, BiFreq, bigr)
  ## 3d. Calculate the total probability mass taken out from observed bigrams, started with w_{i-1} (from 3a)
  AlphaBi <- Cal_RemainProbMass(ObsBiG, UniFreq, w_im1)
  ## 3e. Calculate the total probability mass taken out from observed trigrams, started with (w_{i-2},w_{i-1}) (from 3b)
  AlphaTri <- Cal_RemainProbMass(ObsTriG, BiFreq, bigr)
  ## 4a. Calculate probabilities of unobserved bigrams started with w_{i-1}
  UnObsBiTails <- GetUnObsNgramTails(ObsBiG, 2)
  ## 4b. Calculate backoff probabilities of all unobserved bigrams p_bo(w_i|w_i-1)
  UnObsBiTails[, Qbo:=Cal_BiQbo(UnObsBiTails, AlphaBi)]
  ## 4c. Calculate backoff probabilities of all unobserved trigrams p_bo(w_i|w_{i-1},w_{i-2})
  UnObsTriGrams <- Cal_TriQbo(ObsBiG, ObsTriG, UnObsBiTails, AlphaTri)
  ObsTriG[, c("c", "cDis"):=NULL]
  ObsTriG[, term:=str_remove(ObsTriG[, term], "([^_]+_)+")]
  AllTriG <- setorder(rbind(ObsTriG, UnObsTriGrams), -Qbo)
  res <- AllTriG[1:words_num]
  return(res[Qbo!=0])
}

Next_word <- function(prephrase, words_num) {
  bigr <- Preprocess(prephrase)
  result <- Find_Next_word(bigr, words_num)
  if (dim(result)[1] == 0) {
    rbind(result, list("<Please input more text>", 1))
  }
  return(result)
}