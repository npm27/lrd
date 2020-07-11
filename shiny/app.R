# Shiny application for lrd -----------------------------------------------
# Written by Nicholas Maxwell
# Updated to shinydashboard by Erin M. Buchanan

# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(vecsets)
library(Hmisc)
library(rio)
library(DT)

# Load Pages --------------------------------------------------------------

#####note here if these are functions in the package,
#####we should just load them directly from the package
source("scripts.R")
source("info_tab.R")
source("free_recall.R")
source("cued_recall.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "lrd"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info_tab",
                     icon = icon("question-circle")),
            menuItem("Free Recall", tabName = "free_recall",
                     icon = icon("id-card")),
            menuItem("Cued Recall", tabName = "cued_recall",
                     icon = icon("address-card"))
        )
    ),
    dashboardBody(
        tabItems(
            info_tab,
            free_recall,
            cued_recall
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    values <- reactiveValues()

    # Get Data ----------------------------------------------------------------

    observeEvent(input$file1, {
        if (is.null(input$file1)) return(NULL)
        values$cue_data <- import(input$file1$datapath)
    })

    # Show the data -----------------------------------------------------------

    output$raw_cue_data <- renderDataTable({
        datatable(values$cue_data, rownames = F,
                  options = list(scrollX = T))
    })

    # Score the data ----------------------------------------------------------

    output$scored_cue_data <- renderDataTable({

        # sliding percent scale
        percentage <- input$Percentage / 100

        #convert their cue_values$cue_dataa
        colnames(values$cue_data)[1:3] <- c("ID", "Response", "Key")
        values$cue_data$Response <- tolower(values$cue_data$Response)
        values$cue_data$Key <- tolower(values$cue_data$Key)

        if (length(values$cue_data) > 3) {

           scored_cue <- Percent_Match(values$cue_data$Response, key = values$cue_data$Key, id = values$cue_data$ID, other = values$cue_data[ c(4:length(values$cue_data))], cutoff = percentage)

        } else if (length(values$cue_data) == 3) {

            scored_cue <- Percent_Match(values$cue_data$Response, key = values$cue_data$Key, id = values$cue_data$ID, other = NULL, cutoff = percentage)

        }

        datatable(scored_cue, rownames = F,
                  options = list(scrollX = T))

    })



}

# Run the application
shinyApp(ui = ui, server = server)
