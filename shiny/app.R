# Shiny application for lrd -----------------------------------------------
# Written by Nicholas P. Maxwell
# Updated to shinydashboard by Erin M. Buchanan

# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rio)
library(DT)
library(lrd)

# Load Pages --------------------------------------------------------------

source("info_tab.R")
source("free_recall.R")
source("wide_tab.R")
source("cued_recall.R")
source("sentence_recall.R")

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
            free_recall,
            cued_recall,
            sentence_recall
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

    output$free_positionUI <- renderUI({
        selectizeInput("free_position", "Choose the position answered column for
                       serial position curves:",
                       choices = colnames(values$free_data),
                       multiple = T)
    })

    # Score the free recall and do other related calculations
    observeEvent(input$free_recall_go, {

        # free recall section ----
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
                                     filename = 'free_recall_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_participant <- renderDT(server = F, {
            datatable(values$free_recall_calculated$DF_Participant,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'free_participant_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_group.by <- renderDT(server = F, {
            datatable(values$free_recall_calculated$DF_Group,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'free_group_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$free_recall_graph <- renderPlot({

            #if there are grouping variables
            if(!is.null(input$free_group.by)){

                if (length(input$free_group.by) == 1){

                    temp <- values$free_recall_calculated$DF_Participant
                    temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])

                    print(str(temp))
                    print(input$free_group.by[1])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$free_group.by[1],
                                      y = "Proportion.Correct")) +
                        stat_summary(fun = mean,
                                     geom = "bar",
                                     fill = "White",
                                     color = "Black") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }

                if (length(input$free_group.by) > 1){

                    temp <- values$free_recall_calculated$DF_Participant
                    temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                    temp[ , input$free_group.by[2]] <- factor(temp[ , input$free_group.by[2]])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$free_group.by[1],
                                      y = "Proportion.Correct",
                                      fill = input$free_group.by[2])) +
                        stat_summary(fun = mean,
                                     geom = "bar") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }


            } else {

                stupid_graph <- ggplot(values$free_recall_calculated$DF_Participant,
                       aes(x = Proportion.Correct)) +
                    xlab("Proportion Correct") +
                    ylab("Frequency") +
                    geom_histogram() +
                    theme_bw()
            }

            stupid_graph
        })

        # serial curves ----
        if(!is.null(input$free_position)){
            values$serial_calculated <- serial_position(
                data = values$free_recall_calculated$DF_Scored,
                position = input$free_position,
                answer = "Answer",
                key = values$answer_key_free[ , input$free_key],
                scored = "Scored",
                group.by = c(input$free_group.by))

            output$serial_data_output <- renderDT(server = F, {

                datatable(values$serial_calculated,
                          extensions = 'Buttons',
                          options = list(dom = 'BRtp',
                                         filename = 'free_serial_position',
                                         buttons = c('copy', 'csv', 'excel')),
                          rownames = FALSE) #close datatable
                })

            output$serial_graph <- renderPlot({

                #if there are grouping variables
                if(!is.null(input$free_group.by)){

                    if (length(input$free_group.by) == 1){

                        temp <- values$serial_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "Tested.Position",
                                          y = "Proportion.Correct",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            geom_errorbar(aes(ymin = Proportion.Correct - SE, ymax = Proportion.Correct + SE),
                                          width = .2, position = position_dodge()) +
                            xlab("Tested Position") +
                            ylab("Proportion Correct") +
                            theme_bw()
                    }

                    if (length(input$free_group.by) > 1){

                        temp <- values$serial_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                        temp[ , input$free_group.by[2]] <- factor(temp[ , input$free_group.by[2]])

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "Tested.Position",
                                          y = "Proportion.Correct",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            geom_errorbar(aes(ymin = Proportion.Correct - SE, ymax = Proportion.Correct + SE),
                                          width = .2, position = position_dodge()) +
                            xlab("Tested Position") +
                            ylab("Proportion Correct") +
                            facet_wrap(~ input$free_group.by[2]) +
                            theme_bw()
                    }


                } else {

                    temp <- values$serial_calculated
                    #print(head(temp))
                    stupid_graph <- ggplot(data = temp,
                           aes_string(x = "Tested.Position",
                                      y = "Proportion.Correct")) +
                        geom_line() +
                        geom_point() +
                        geom_errorbar(aes(ymin = Proportion.Correct - SE, ymax = Proportion.Correct + SE),
                                      width = .2, position = position_dodge()) +
                        xlab("Tested Position") +
                        ylab("Proportion Correct") +
                        theme_bw()
                }

                stupid_graph

                })

            } #close null free position so serial curves

        # probability of first response ----
        if(!is.null(input$free_position)){
            values$pfr_calculated <- pfr(data = values$free_recall_calculated$DF_Scored,
                                         position = input$free_position,
                                         answer = "Answer",
                                         id = "Sub.ID",
                                         key = values$answer_key_free[ , input$free_key],
                                         scored = "Scored",
                                         group.by = c(input$free_group.by))

            output$pfr_data_output <- renderDT(server = F, {

                datatable(values$pfr_calculated,
                          extensions = 'Buttons',
                          options = list(dom = 'BRtp',
                                         filename = 'free_pfr',
                                         buttons = c('copy', 'csv', 'excel')),
                          rownames = FALSE) #close datatable
            })

            output$pfr_graph <- renderPlot({

                #if there are grouping variables
                if(!is.null(input$free_group.by)){

                    if (length(input$free_group.by) == 1){

                        temp <- values$pfr_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                        temp$Tested.Position <- as.numeric(as.character(temp$Tested.Position))

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "Tested.Position",
                                          y = "pfr",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            xlab("Tested Position") +
                            ylab("Probability of First Response") +
                            theme_bw()
                    }

                    if (length(input$free_group.by) > 1){

                        temp <- values$pfr_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                        temp[ , input$free_group.by[2]] <- factor(temp[ , input$free_group.by[2]])
                        temp$Tested.Position <- as.numeric(as.character(temp$Tested.Position))

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "Tested.Position",
                                          y = "pfr",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            xlab("Tested Position") +
                            ylab("Probability of First Response") +
                            facet_wrap(~ input$free_group.by[2]) +
                            theme_bw()
                    }


                } else {

                    temp <- values$pfr_calculated
                    temp$Tested.Position <- as.numeric(as.character(temp$Tested.Position))
                    #print(str(temp))
                    stupid_graph <- ggplot(data = temp,
                           aes_string(x = "Tested.Position",
                                      y = "pfr")) +
                        geom_line() +
                        geom_point() +
                        xlab("Tested Position") +
                        ylab("Probability of First Response") +
                        theme_bw()
                }

                stupid_graph
            })
        }

        # conditional response probability ----
        if(!is.null(input$free_position)){
            values$crp_calculated <- crp(data = values$free_recall_calculated$DF_Scored,
                                         position = input$free_position,
                                         answer = "Answer",
                                         id = "Sub.ID",
                                         key = values$answer_key_free[ , input$free_key],
                                         scored = "Scored")


            output$crp_data_output <- renderDT(server = F, {

                datatable(values$crp_calculated,
                          extensions = 'Buttons',
                          options = list(dom = 'BRtp',
                                         filename = 'free_crp',
                                         buttons = c('copy', 'csv', 'excel')),
                          rownames = FALSE) #close datatable
            })

            output$crp_graph <- renderPlot({

                #if there are grouping variables
                if(!is.null(input$free_group.by)){

                    if (length(input$free_group.by)==1){

                        temp <- values$crp_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                        temp$participant_lags <- as.numeric(as.character(temp$participant_lags))

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "participant_lags",
                                          y = "CRP",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            xlab("Lag Distance") +
                            ylab("Conditional Response Probability") +
                            theme_bw()
                    }

                    if (length(input$free_group.by) == 2){

                        temp <- values$crp_calculated
                        temp[ , input$free_group.by[1]] <- factor(temp[ , input$free_group.by[1]])
                        temp[ , input$free_group.by[2]] <- factor(temp[ , input$free_group.by[2]])
                        temp$participant_lags <- as.numeric(as.character(temp$participant_lags))

                        stupid_graph <- ggplot(data = temp,
                               aes_string(x = "participant_lags",
                                          y = "CRP",
                                          color = input$free_group.by[1])) +
                            geom_line() +
                            geom_point() +
                            xlab("Lag Distance") +
                            ylab("Conditional Response Probability") +
                            facet_wrap(~ input$free_group.by[2]) +
                            theme_bw()
                    }


                } else {

                    temp <- values$crp_calculated
                    temp$participant_lags <- as.numeric(as.character(temp$participant_lags))
                    stupid_graph <- ggplot(data = temp,
                           aes_string(x = "participant_lags",
                                      y = "CRP")) +
                        geom_line() +
                        geom_point() +
                        xlab("Lag Distance") +
                        ylab("Probability of First Response") +
                        theme_bw()
                }

                stupid_graph
            })

        } #close null free position

        }) #close observe event

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

    # Create the answer choices
    output$cued_responsesUI <- renderUI({
        selectizeInput("cued_responses", "Choose the response column:",
                       choices = colnames(values$cued_data),
                       multiple = F)
    })

    output$cued_keyUI <- renderUI({
        selectizeInput("cued_key", "Choose the answer key column:",
                       choices = colnames(values$answer_key_cued),
                       multiple = F)
    })

    output$cued_key.trialUI <- renderUI({
        selectizeInput("cued_key.trial", "Choose the answer key trial number column:",
                       choices = colnames(values$answer_key_cued),
                       multiple = F)
    })

    output$cued_idUI <- renderUI({
        selectizeInput("cued_id", "Choose the participant id column:",
                       choices = colnames(values$cued_data),
                       multiple = F)
    })

    output$cued_id.trialUI <- renderUI({
        selectizeInput("cued_id.trial", "Choose the participant id trial number column:",
                       choices = colnames(values$cued_data),
                       multiple = F)
    })

    output$cued_group.byUI <- renderUI({
        selectizeInput("cued_group.by", "Choose the group by columns:",
                       choices = colnames(values$cued_data),
                       multiple = T)
    })

    # Score the free recall and do other related calculations
    observeEvent(input$cued_recall_go, {

        # free recall section ----
        values$cued_recall_calculated <- prop_correct_cued(
            data = values$cued_data,
            responses = input$cued_responses,
            key = values$answer_key_cued[ , input$cued_key],
            key.trial = values$answer_key_cued[ , input$cued_key.trial],
            id = input$cued_id,
            id.trial = input$cued_id.trial,
            cutoff = input$cued_cutoff,
            flag = input$cued_flag,
            group.by = c(input$cued_group.by))

        output$cued_recall_scored <- renderDT(server = F, {
            datatable(values$cued_recall_calculated$DF_Scored,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'cued_recall_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$cued_recall_participant <- renderDT(server = F, {
            datatable(values$cued_recall_calculated$DF_Participant,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'cued_participant_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$cued_recall_group.by <- renderDT(server = F, {
            datatable(values$cued_recall_calculated$DF_Group,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'cued_group_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$cued_recall_graph <- renderPlot({

            #if there are grouping variables
            if(!is.null(input$cued_group.by)){

                if (length(input$cued_group.by) == 1){

                    temp <- values$cued_recall_calculated$DF_Participant
                    temp[ , input$cued_group.by[1]] <- factor(temp[ , input$cued_group.by[1]])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$cued_group.by[1],
                                      y = "Proportion.Correct")) +
                        stat_summary(fun = mean,
                                     geom = "bar",
                                     fill = "White",
                                     color = "Black") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }

                if (length(input$cued_group.by) > 1){

                    temp <- values$cued_recall_calculated$DF_Participant
                    temp[ , input$cued_group.by[1]] <- factor(temp[ , input$cued_group.by[1]])
                    temp[ , input$cued_group.by[2]] <- factor(temp[ , input$cued_group.by[2]])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$cued_group.by[1],
                                      y = "Proportion.Correct",
                                      fill = input$cued_group.by[2])) +
                        stat_summary(fun = mean,
                                     geom = "bar") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }


            } else {

                stupid_graph <- ggplot(values$cued_recall_calculated$DF_Participant,
                       aes(x = Proportion.Correct)) +
                    geom_histogram() +
                    theme_bw()
            }

            stupid_graph
        })

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


    # Create the answer choices
    output$sentence_responsesUI <- renderUI({
        selectizeInput("sentence_responses", "Choose the response column:",
                       choices = colnames(values$sentence_data),
                       multiple = F)
    })

    output$sentence_keyUI <- renderUI({
        selectizeInput("sentence_key", "Choose the answer key column:",
                       choices = colnames(values$answer_key_sentence),
                       multiple = F)
    })

    output$sentence_key.trialUI <- renderUI({
        selectizeInput("sentence_key.trial", "Choose the answer key trial number column:",
                       choices = colnames(values$answer_key_sentence),
                       multiple = F)
    })

    output$sentence_idUI <- renderUI({
        selectizeInput("sentence_id", "Choose the participant id column:",
                       choices = colnames(values$sentence_data),
                       multiple = F)
    })

    output$sentence_id.trialUI <- renderUI({
        selectizeInput("sentence_id.trial", "Choose the participant id trial number column:",
                       choices = colnames(values$sentence_data),
                       multiple = F)
    })

    output$sentence_group.byUI <- renderUI({
        selectizeInput("sentence_group.by", "Choose the group by columns:",
                       choices = colnames(values$sentence_data),
                       multiple = T)
    })

    # Score the free recall and do other related calculations
    observeEvent(input$sentence_recall_go, {

        # sentence section ----
        values$sentence_recall_calculated <- prop_correct_sentence(
            data = values$sentence_data,
            responses = input$sentence_responses,
            key = values$answer_key_sentence[ , input$sentence_key],
            key.trial = values$answer_key_sentence[ , input$sentence_key.trial],
            id = input$sentence_id,
            id.trial = input$sentence_id.trial,
            cutoff = input$sentence_cutoff,
            flag = input$sentence_flag,
            group.by = c(input$sentence_group.by),
            token.split = input$sentence_token)

        output$sentence_recall_scored <- renderDT(server = F, {
            datatable(values$sentence_recall_calculated$DF_Scored,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'sentence_recall_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$sentence_recall_participant <- renderDT(server = F, {
            datatable(values$sentence_recall_calculated$DF_Participant,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'sentence_participant_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$sentence_recall_group.by <- renderDT(server = F, {
            datatable(values$sentence_recall_calculated$DF_Group,
                      extensions = 'Buttons',
                      options = list(dom = 'BRtp',
                                     filename = 'sentence_group_scored',
                                     buttons = c('copy', 'csv', 'excel')),
                      rownames = FALSE) #close datatable
        })

        output$sentence_recall_graph <- renderPlot({

            #if there are grouping variables
            if(!is.null(input$sentence_group.by)){

                if (length(input$sentence_group.by)==1){

                    temp <- values$sentence_recall_calculated$DF_Participant
                    temp[ , input$sentence_group.by[1]] <- factor(temp[ , input$sentence_group.by[1]])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$sentence_group.by[1],
                                      y = "Proportion.Correct")) +
                        stat_summary(fun = mean,
                                     geom = "bar",
                                     fill = "White",
                                     color = "Black") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }

                if (length(input$sentence_group.by) == 2){

                    temp <- values$sentence_recall_calculated$DF_Participant
                    temp[ , input$sentence_group.by[1]] <- factor(temp[ , input$sentence_group.by[1]])
                    temp[ , input$sentence_group.by[2]] <- factor(temp[ , input$sentence_group.by[2]])

                    stupid_graph <- ggplot(temp,
                           aes_string(x = input$sentence_group.by[1],
                                      y = "Proportion.Correct",
                                      fill = input$sentence_group.by[2])) +
                        stat_summary(fun = mean,
                                     geom = "bar") +
                        stat_summary(fun.data = mean_cl_normal,
                                     geom = "errorbar",
                                     width = .2,
                                     position = position_dodge(width = 0.90)) +
                        theme_bw()
                }


            } else {

                stupid_graph <- ggplot(values$sentence_recall_calculated$DF_Participant,
                       aes(x = Proportion.Correct)) +
                    geom_histogram() +
                    theme_bw()
            }

            stupid_graph
        })

    })

}

# Run the application
shinyApp(ui = ui, server = server)
