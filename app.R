#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vroom)
library(here)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(Rstem)
library(sentiment)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Analisis Sentimen Film Money Heist"),
  headerPanel("dengan Naive Bayes"),
            mainPanel(
                tabsetPanel(
                    tabPanel("Data Twitter", DT::dataTableOutput('dataTwitter')),
                    tabPanel("Data Cleaned", DT::dataTableOutput('dataCleaned')),
                    tabPanel("Data Sentimen", DT::dataTableOutput('tbl')),
                    tabPanel("Plot Tweet", plotOutput("sent2"))
                )
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataTwitter<- vroom(here("dataTwitter.csv"))
    dataTwitter<- data.frame(dataTwitter)
    # Output Data
    output$dataTwitter = DT::renderDataTable({
      DT::datatable(dataTwitter, options = list(lengthChange = FALSE))
    })
    
    sent_df<- vroom(here("dataSentimen.csv"))
    sent_df <- data.frame(sent_df)
    dataCleaned<- vroom(here("dataCleaned.csv"))
    dataCleaned<- data.frame(dataCleaned)
    # Output Data
    output$dataCleaned = DT::renderDataTable({
      DT::datatable(dataCleaned, options = list(lengthChange = FALSE))
    })
  
    sent_df<- vroom(here("dataSentimen.csv"))
    sent_df <- data.frame(sent_df)
    # Output Data
    output$tbl = DT::renderDataTable({
      DT::datatable(sent_df, options = list(lengthChange = FALSE))
    })
    
    # plot distribution of emotions
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Money Heist",
           plot.title = element_text(size=12))
    plotSentiments1 <- function(sentiment_dataframe, title) 
    {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + 
        geom_bar(aes(y=..count.., fill=emotion)) + 
        scale_fill_brewer(palette="Dark2") + 
        ggtitle(title) + 
        theme(legend.position="right") + 
        ylab("Number of Tweets") + 
        xlab("Emotion Categories")
    }
    #plotting tweets emotions
    output$sent1 <- renderPlot({
      plotSentiments1(sent_df, "Sentiment Analysis of Money Heist")
    })
    
    # plot distribution of polarity
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Money Heist",
           plot.title = element_text(size=12))
    plotSentiments2 <- function(sent_df, title)
    {
      library(ggplot2)
      ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle(title) +
        theme(legend.position="right") +
        ylab("Number of Tweets") +
        xlab("Polarity Categories")
    }
    
    output$sent2 <- renderPlot({
      plotSentiments2(sent_df, "Sentiment Analysis of Money Heist")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
