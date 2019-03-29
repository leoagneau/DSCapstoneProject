# Load text files and convert all the terms to a list of 1-, 2-, 3-grams

# Leo Mak
# 03-2019

# Libraries ----
require(readtext)
require(quanteda)
require(data.table)
require(stringi)

# To control which terms to be ignored with raw count < min_count
min_count = 4

## Define where is the data to be loaded and stored
if (.Platform$OS.type == "unix") {  # Linux or Unix
  path = "~/temp/JHU_DS/SwiftData/final/"
} else {  # Windows
  path = "E:/tmp/DS_jhu/SwiftData/final/"  # Windows
}

## 0. Preprocessing: read text files and create DFM ----
## Returns a Quanteda dfm from a given character vector
##
## txt - Character vector of text, each element in the vector is a document in dfm 
## ng - The 'N' of N-gram
load.file.to.dfm <- function(txt, ng) {
  text.dfm <- txt %>% tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, remove_hyphens=T, remove_twitter=T, remove_url=T) %>%  tokens_remove(stopwords("en")) %>% tokens_ngrams(n=ng) %>% dfm()
  return(text.dfm)
}

## Load the specified files ('blog', 'news', or 'twitter') and return the 1-, 2-, and 3-grams
## contained in the files.  Returns a list of 3 data.tables which are the unigram, bigram, trigram
## data.table with terms and raw counts
##
## ... - single letter either in 'b', 'n', or 't', representing 'blog', 'news', 'twitter'.
##       Multiple letters are accepted in form such as ('b', 't').
## sample_size - a decimal number between 0 and 1, indicating the sampling size from the text file
##               which will be processed as N-gram
Load_Files <- function(..., sample_size = 0.5) {
  types <- list(...)
  news <- NULL
  blogs <- NULL
  twitter <- NULL
  
  t1 <- proc.time()
  for (i in types) {
    if (i == 'n') {
      news <- stri_split_lines1(readtext(paste0(path,"en_US/en_US.news.txt")))
      news <- sample(news, size=length(news)*sample_size)
    }
    if (i == 'b') {
      blogs <- stri_split_lines1(readtext(paste0(path,"en_US/en_US.blogs.txt")))
      blogs <- sample(blogs, size=length(blogs)*sample_size)
    }
    if (i == 't') {
      twitter <- stri_split_lines1(readtext(paste0(path,"en_US/en_US.twitter.txt")))
      twitter <- sample(twitter, size=length(twitter)*sample_size)
    }
  }
  txt <- c(news, blogs, twitter)
  rm(list=c("news", "blogs", "twitter"))
  t2 <- proc.time()
  print(t2-t1)

  t1 <- proc.time()
  UniG <- load.file.to.dfm(txt, 1)
  BiG <- load.file.to.dfm(txt, 2)
  TriG <- load.file.to.dfm(txt, 3)
  t2 <- proc.time()
  print(t2-t1)
  rm(list="txt")
  
  ## 1. Count the raw count of each observed unigrams, bigrams, and trigrams in the corpus
  CountNGramFreq <- function(NGrDfm) {
    FreqV <- colSums(NGrDfm)
    return(data.table(term=names(FreqV), c=FreqV))
  }
  UniFreq <- CountNGramFreq(UniG)
  BiFreq <- CountNGramFreq(BiG)
  TriFreq <- CountNGramFreq(TriG)
  
  UniFreq <- UniFreq[c>min_count,]
  BiFreq <- BiFreq[c>min_count,]
  TriFreq <- TriFreq[c>min_count,]
  rm(list=c("UniG", "BiG", "TriG"))
  return(list(UniFreq, BiFreq, TriFreq))
}
