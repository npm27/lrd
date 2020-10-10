# Shiny application for lrd -----------------------------------------------
# Written by Nicholas P. Maxwell
# Updated to shinydashboard by Erin M. Buchanan

# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
# library(vecsets)
# library(Hmisc)
library(rio)
library(DT)
library(lrd)

# Load Pages --------------------------------------------------------------

source("info_tab.R")
source("free_recall.R")
source("wide_tab.R")
#source("cued_recall.R")
#source("sentence_recall.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "lrd"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info_tab",
                     icon = icon("question-circle")),
            menuItem("Arrange Data", tabName = "wide_tab",
                     icon = icon("sort-amount-down")),
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
            wide_tab,
            free_recall
            #,
            # cued_recall,
            # sentence_recall
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    values <- reactiveValues()

    # Wide to long conversion -------------------------------------------------

    # Get the data
    observeEvent(input$wide_input, {
        if(is.null(input$wide_input)) return(NULL)
        values$wide_data <- import(input$wide_input$datapath)
    })

    # Output the data
    output$wide_data <- renderDT({
        values$wide_data
    })

    # Create the answer choices
    output$wide_responsesUI <- renderUI({
        selectizeInput("wide_responses", "Choose the response column:",
                       choices = colnames(values$wide_data),
                       multiple = F)
    })

    output$wide_idUI <- renderUI({
        selectizeInput("wide_id", "Choose the participant ID column:",
                       choices = colnames(values$wide_data),
                       multiple = F)
    })

    # Convert the data
    observeEvent(input$wide_data_go, {

        output$long_data_output <- renderDT(server = F, {
            # do the conversion
            # print(input$wide_responses)
            # print(input$wide_sep)
            # print(input$wide_id)

            long_data <- arrange_data(data = values$wide_data,
                         responses = input$wide_responses,
                         sep = input$wide_sep,
                         id = input$wide_id)

            datatable(long_data,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'long_results',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable

        })
    })

    # Free Recall Scoring -------------------------------------------------------

    # Get the data
    observeEvent(input$free_data, {
        if (is.null(input$free_data)) return(NULL)
        values$free_data <- import(input$free_data$datapath)
    })

    observeEvent(input$answer_key_free, {
        if (is.null(input$answer_key_free)) return(NULL)
        values$answer_key_free <- import(input$answer_key_free$datapath)
    })

    # Output the data
    output$free_recall_data <- renderDT({
        values$free_data
    })

    output$free_recall_answer <- renderDT({
        values$answer_key_free
    })

    # Create the answer choices
    output$free_responsesUI <- renderUI({
        selectizeInput("free_responses", "Choose the response column:",
                       choices = colnames(values$free_data),
                       multiple = F)
    })

    output$free_keyUI <- renderUI({
        selectizeInput("free_key", "Choose the answer key column:",
                       choices = colnames(values$answer_key_free),
                       multiple = F)
    })

    output$free_idUI <- renderUI({
        selectizeInput("free_id", "Choose the participant id column:",
                       choices = colnames(values$free_data),
                       multiple = F)
    })

    output$free_group.byUI <- renderUI({
        selectizeInput("free_group.by", "Choose the group by columns:",
                       choices = colnames(values$free_data),
                       multiple = T)
    })

    # Score the free recall
    observeEvent(input$free_recall_go, {

        # print(input$free_responses)
        # print(input$free_key)
        # print(input$free_id)
         print(input$free_group.by)
        # print(input$free_cutoff)
        # print(input$free_flag)
        #
        # print(class(input$free_responses))
        # print(class(input$free_key))
        # print(class(input$free_id))
        # print(class(input$free_group.by))
        # print(class(input$free_cutoff))
        # print(class(input$free_flag))

        values$free_recall_calculated <- prop_correct_free(
            data = values$free_data,
            responses = input$free_responses,
            key = values$answer_key_free[ , input$free_key],
            id = input$free_id,
            cutoff = input$free_cutoff,
            flag = input$free_flag,
            group.by = c(input$free_group.by))

        output$free_recall_scored <- renderDT(server = F, {
            datatable(values$free_recall_calculated$DF_Scored,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'long_results',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_participant <- renderDT(server = F, {
            datatable(values$free_recall_calculated$DF_Participant,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'long_results',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_group.by <- renderDT(server = F, {
            datatable(values$free_recall_calculated$DF_Group,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'long_results',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_graph <- renderPlot({

            #if there are grouping variables
            if(!is.null(input$free_group.by)){

                if (length(input$free_group.by)==1){
                    ggplot() +
                        stat_summary(data = values$free_recall_calculated$DF_Participant,
                                     aes_string(x = input$free_group.by[1], y = "Proportion.Correct"),
                                     fun = mean,
                                     geom = "bar",
                                     fill = "White",
                                     color = "Black") +
                        stat_summary(data = values$free_recall_calculated$DF_Participant,
                                     aes_string(x = input$free_group.by[1], y = "Proportion.Correct"),
                                     fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }

                if (length(input$free_group.by) == 2){
                    ggplot() +
                        stat_summary(data = values$free_recall_calculated$DF_Participant,
                                     aes_string(x = input$free_group.by[1],
                                                y = "Proportion.Correct",
                                                fill = input$free_group.by[2]),
                                     fun = mean,
                                     geom = "bar") +
                        stat_summary(data = values$free_recall_calculated$DF_Participant,
                                     aes_string(x = input$free_group.by[1],
                                                y = "Proportion.Correct",
                                                fill = input$free_group.by[2]),
                                     fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }


            }
        })

        })



    #output$serial_data_output <- renderDT({})
    #output$serial_graph <- renderPlot({})

    #output$pfr_data_output <- renderDT({})
    #output$pfr_graph <- renderPlot({})

    #output$crp_data_output <- renderDT({})
    #output$crp_graph <- renderPlot({})

    # # Cued Recall Scoring -------------------------------------------------------
    #
    # # Get the data
    # observeEvent(input$cued_data, {
    #     if (is.null(input$cued_data)) return(NULL)
    #     values$cued_data <<- import(input$cued_data$datapath)
    # })
    #
    # observeEvent(input$answer_key_cued, {
    #     if (is.null(input$answer_key_cued)) return(NULL)
    #     values$answer_key_cued <<- import(input$answer_key_cued$datapath)
    # })
    #
    # # Output the data
    # output$cued_recall_data <- renderDT({
    #     values$cued_data
    # })
    #
    # output$cued_recall_answer <- renderDT({
    #     values$answer_key_cued
    # })
    #
    # # Sentence Recall Scoring -------------------------------------------------------
    #
    # # Get the data
    # observeEvent(input$sentence_data, {
    #     if (is.null(input$sentence_data)) return(NULL)
    #     values$sentence_data <<- import(input$sentence_data$datapath)
    # })
    #
    # observeEvent(input$answer_key_sentence, {
    #     if (is.null(input$answer_key_sentence)) return(NULL)
    #     values$answer_key_sentence <<- import(input$answer_key_sentence$datapath)
    # })
    #
    # # Output the data
    # output$sentence_recall_data <- renderDT({
    #     values$sentence_data
    # })
    #
    # output$sentence_recall_answer <- renderDT({
    #     values$answer_key_sentence
    # })
    #

}

# Run the application
shinyApp(ui = ui, server = server)
