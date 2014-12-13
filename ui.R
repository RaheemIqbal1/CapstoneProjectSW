#--------------------------------------------------
# R UI Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(shiny))

shinyUI(fluidPage(
 
    # Application title
    titlePanel("Predict Next Word"),
    
    fluidRow(HTML("<strong>Author: Rumi, RI</strong>") ),
    fluidRow(HTML("<strong>Date: 12-Dec-2014</strong>") ),
    
    fluidRow(
      br(),
      p("This Shiny application uses N-Gram Back Off model to predict next unseen word in the user entered words sequence. In a separate data processing phase twitter, news and blogs data 
         was consumed, and then cleansed to create data frames of four, three, two and one-grams with corresponding sorted cumulative frequencies. 
         The Shiny app loads the four saved n-gram objects and apply a simple Katz's Back-off algorithm to predict the next word after user enters a partial sentence.")),
    br(),
    br(),
  
    fluidRow(HTML("<strong>Enter an incomplete sentence. Press \"Next Word\" button to predict the next word</strong>") ),
    fluidRow( p("\n") ),
    
    # Sidebar layout
    sidebarLayout(
        
        sidebarPanel(
            textInput("inputString", "Enter a partial sentence here",value = ""),
            submitButton("Next Word")
        ),
        
        mainPanel(
            h4("Predicted Next Word"),
            verbatimTextOutput("prediction"),
            textOutput('text1'),
            textOutput('text2')
        )
    )
))