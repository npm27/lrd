# Shiny application for lrd -----------------------------------------------
# Written by Nicholas P. Maxwell
# Updated to shinydashboard by Erin M. Buchanan

# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(vecsets)
library(Hmisc)
library(rio)
library(DT)
library(lrd)

# Load Pages --------------------------------------------------------------

#####note here if these are functions in the package,
#####we should just load them directly from the package
source("scripts.R")
source("info_tab.R")
source("free_recall.R")
source("cued_recall.R")
source("sentence_recall.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "lrd"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info_tab",
                     icon = icon("question-circle")),
            menuItem("Free Recall", tabName = "free_recall",
                     icon = icon("sd-card")),
            menuItem("Cued Recall", tabName = "cued_recall",
                     icon = icon("memory")),
            menuItem("Sentence Recall", tabName = "sentence_recall",
                     icon = icon("keyboard"))
        )
    ),
    dashboardBody(
        tabItems(
            info_tab,
            free_recall,
            cued_recall,
            sentence_recall
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    values <- reactiveValues()

    # Free Recall Scoring -------------------------------------------------------

    # Get the data
    observeEvent(input$free_data, {
        if (is.null(input$free_data)) return(NULL)
        values$free_data <<- import(input$free_data$datapath)
    })

    observeEvent(input$answer_key_free, {
        if (is.null(input$answer_key_free)) return(NULL)
        values$answer_key_free <<- import(input$answer_key_free$datapath)
    })

    # Output the data
    output$free_recall_data <- renderDT({
        values$free_data
    })

    output$free_recall_answer <- renderDT({
        values$answer_key_free
    })

    # Cued Recall Scoring -------------------------------------------------------

    # Get the data
    observeEvent(input$cued_data, {
        if (is.null(input$cued_data)) return(NULL)
        values$cued_data <<- import(input$cued_data$datapath)
    })

    observeEvent(input$answer_key_cued, {
        if (is.null(input$answer_key_cued)) return(NULL)
        values$answer_key_cued <<- import(input$answer_key_cued$datapath)
    })

    # Output the data
    output$cued_recall_data <- renderDT({
        values$cued_data
    })

    output$cued_recall_answer <- renderDT({
        values$answer_key_cued
    })

    # Sentence Recall Scoring -------------------------------------------------------

    # Get the data
    observeEvent(input$sentence_data, {
        if (is.null(input$sentence_data)) return(NULL)
        values$sentence_data <<- import(input$sentence_data$datapath)
    })

    observeEvent(input$answer_key_sentence, {
        if (is.null(input$answer_key_sentence)) return(NULL)
        values$answer_key_sentence <<- import(input$answer_key_sentence$datapath)
    })

    # Output the data
    output$sentence_recall_data <- renderDT({
        values$sentence_data
    })

    output$sentence_recall_answer <- renderDT({
        values$answer_key_sentence
    })









}

# Run the application
shinyApp(ui = ui, server = server)
