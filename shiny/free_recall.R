free_recall <-
  tabItem(tabName = "free_recall",
          fluidRow(

            # Upload your data --------------------------------------------------------
            box(
              title = tags$b("Upload Your Data and Answers"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Upload Your Data File:</b> Nearly all file types supported!
                   Only one header row is supported."),
              fileInput('free_data', 'Choose Data File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Upload Your Answer Key:</b> Your answer key can be in
                   the original data file, just upload it again here."),
              fileInput('answer_key_free', 'Choose Answer File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
              ), # box

            # Check your data ---------------------------------------------------------
            box(
              title = tags$b("Check Your Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's your free recall data:</b>"),
              DTOutput("free_recall_data"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's your answer key:</b>"),
              DTOutput("free_recall_answer"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Set Up Free Recall ---------------------------------------------------------
            box(
              title = tags$b("Participant Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              selectizeInput('free_responses', "Choose the response column:", choices = NULL),
              selectizeInput('free_key', "Choose the answer key column:", choices = NULL),
              selectizeInput('free_id', "Choose the participant ID:", choices = NULL),
              selectizeInput('free_group.by', "Choose the group by columns:", choices = NULL, multiple = TRUE),
              sliderInput('free_cutoff', label, min = 0, max = 5, value = 0, step = 1,
                          round = TRUE),
              checkboxInput('free_flag', "Do you want to flag for outliers?", value = FALSE, width = NULL)
            )
            #, # box
#
#             # Free Recall Output ---------------------------------------------------------
#             box(
#               title = tags$b("Scored Output"),
#               collapsible = TRUE,
#               solidHeader = TRUE,
#               status = "primary",
#               width = 12,
#               p(" "),
#               HTML("<b>Here's the scored participant output:</b>"),
#               DTOutput("free_recall_scored"),
#               tags$style(type = "text/css",
#                          ".shiny-output-error {visibility: hidden;}",
#                          ".shiny-output-error:before {visibility: hidden;}")
#             ), # box
#
#             box(
#               title = tags$b("Summarized Output"),
#               collapsible = TRUE,
#               solidHeader = TRUE,
#               status = "primary",
#               width = 12,
#               p(" "),
#               HTML("<b>Here's the scored participant output:</b>"),
#               DTOutput("free_recall_participant"),
#               tags$style(type = "text/css",
#                          ".shiny-output-error {visibility: hidden;}",
#                          ".shiny-output-error:before {visibility: hidden;}"),
#               DTOutput("free_recall_groupby"),
#               tags$style(type = "text/css",
#                          ".shiny-output-error {visibility: hidden;}",
#                          ".shiny-output-error:before {visibility: hidden;}")
#
#             ), # box
#
#             # Free Recall Graph ---------------------------------------------------------
#             box(
#               title = tags$b("Graphed Output"),
#               collapsible = TRUE,
#               solidHeader = TRUE,
#               status = "primary",
#               width = 12,
#               p(" "),
#               HTML("<b>Here's a graph of your results:</b>"),
#               plotOutput("free_recall_graph"),
#               tags$style(type = "text/css",
#                          ".shiny-output-error {visibility: hidden;}",
#                          ".shiny-output-error:before {visibility: hidden;}")
#             ) # box
          ) #fluidrow
        ) #close page
