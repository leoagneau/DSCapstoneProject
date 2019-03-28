## This is the capstone project of Coursera-JHU's Data Science Specialization

# Load packages ----
library(shiny)
library(shinythemes)
require(data.table)
require(ggplot2)

# Initialization
rm(list=ls())
MIN_WORD_NUM = 1
MAX_WORD_NUM = 10

# Source helpers ----
source("helpers.R")

# Load data, default is blog
Load_data("B")

# Prepare models
Prep_model()

# Width of columns in fluidRow
LColW <- 3
MColW <- 3
RColW <- 5

# Corpus name mapping
corp_list <- list("B" = "Blog",
                  "N" = "News", 
                  "T" = "Twitter")

# Description of the app
Usage <- "<p>Type some text in the <strong>Input Area</strong>, and add a space
          or enter a new line after your input to generate the list of predicted words.
          The list of predicted words with corresponding probabilities will be shown
          in the graph of right hand side.</p>
          <p>The <strong>Number of word prediction</strong> can be changed between 1 to 10.</p>"
Intro <- "<p>This program makes use of Katz backoff 3-gram modeling algorithm, which in turn uses
          Good-Turing discounting to calculate the probabilites for the backoff algorithm.</p>
          "
Limitation <- "<p>The program looks for the space or new line at the end of the input to trigger
               the prediction algorithm.  However, since each prediction takes a bit of time, please
               be kind to the program and don't type too fast, especially when deleteing words. 
               If the predictions seem weird, please try to reload the app/webpage.</p>"

ui <- fluidPage(

  titlePanel("Next Word Prediction (NEWPRE)"),

  fluidRow(
    column(LColW,
           h3("Introduction"), hr(),
           h4("Usage"), htmlOutput("usage"), br(),
           h4("About the program"), htmlOutput("intro"), br(),
           h4("Limitation"), htmlOutput("limitation")
    ),
    column(MColW,
           h3("Input Area"),
           textAreaInput("typingArea", NULL, placeholder = "Type your sentence here", width = "110%", resize = "both"),
           br(),
           numericInput("n", "Number of word prediction", min=MIN_WORD_NUM, max=MAX_WORD_NUM, value=5),
           checkboxGroupInput("corpus", "Corpus:",
                              choices = c("Blog" = "B",
                                "News" = "N",
                                "Twitter" = "T"),
                              selected = "B"),
           actionButton("chgCorpus", "Use Corpus")
    ),
    column(RColW,
           plotOutput("wordsProb", width = "120%", height = "500px")
    )
  ),
  fluidRow(
    column(4,
           hr(),
           span(style="color:#C8C8C8;", "Leo Mak @2019"))
  )
)

# Define server logic
server <- function(input, output, session) {

  output$usage <- renderText({HTML(Usage)})
  output$intro <- renderText({HTML(Intro)})
  output$limitation <- renderText({HTML(Limitation)})
  
  observeEvent(input$chgCorpus, {
    corpora <- input$corpus
    if (is.null(input$corpus)) {
      updateCheckboxGroupInput(session, "corpus", selected = "B")
      corpora <- "B"
    }
    loadMsg <- str_glue("Loading corpus: ", paste(mget(corpora, envir = as.environment(corp_list)), collapse = ", "), "...")
    showModal(modalDialog(loadMsg, footer=NULL))
    Load_data(corpora)
    Prep_model()
    removeModal()
  })
  
  Validate_num <- reactive({
    if (is.na(input$n))
      5
    else if (input$n < MIN_WORD_NUM)
      MIN_WORD_NUM
    else if (input$n > MAX_WORD_NUM)
      MAX_WORD_NUM
    else
      input$n
  })
  
  Predict_words1 <- function(textInput, wordnum) {
    Next_word(textInput, wordnum)
  }

  res1 <- data.table(term=character(), Qbo=numeric())
  
  observeEvent(input$typingArea, {
    #browser()
    if (lengths(strsplit(input$typingArea, "\\W+")) > 1) {
      if (grepl("[[:space:]]$", input$typingArea)) {
        res1 <<- Predict_words1(input$typingArea, MAX_WORD_NUM)
        output$dbgMsg1 <- renderPrint({memory.size()})
      }
    }
  })
  
  output$wordsProb <- renderPlot({
    input$typingArea
    ggplot(res1[1:input$n], aes(x=reorder(term, Qbo), y=Qbo, fill=Qbo, position_stack(reverse=T))) +
      coord_flip() + 
      geom_col() + guides(fill=F) + 
      xlab(NULL) + ylab("Probability") + 
      ggtitle("Predicted Next Words") + 
      scale_fill_gradient(low="beige", high="slateblue") + 
      theme(panel.background = element_rect(fill="grey70"), plot.background = element_rect("transparent"), 
            axis.title.x = element_text(size = 14, colour = "orange", margin = margin(t=10)),
            axis.text = element_text(size = 18, colour = "orange"),
            plot.title = element_text(size = 22, hjust = 0.5, margin = margin(t=15, b=15)))
  }, bg="transparent")
  
  session$onSessionEnded(function() {
  })
}

# Run the application 
shinyApp(
  ui = fluidPage(theme = shinytheme("sandstone"), ui),
  server = server)
