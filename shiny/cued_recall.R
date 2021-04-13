cued_recall <-
  tabItem(tabName = "cued_recall",

          fluidRow(

            box(
              title = tags$b("Upload Your Data and Answers"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Important</b>: Your data file must be in long format for the
                   scoring to accurately process. You can use the Arrange Data tab to
                   first convert the data."),
              p(" "),
              HTML("<b>Upload Your Data File:</b> Nearly all file types supported!
                   Only one header row is supported."),
              p(" "),
              fileInput('cued_data', 'Choose Data File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Upload Your Answer Key:</b> Your answer key can be in
                   the original data file, just upload it again here."),
              p(" "),
              fileInput('answer_key_cued', 'Choose Answer File'),
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
              HTML("<b>Here's your cued-recall data:</b>"),
              p(" "),
              DTOutput("cued_recall_data"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's your answer key:</b>"),
              DTOutput("cued_recall_answer"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Set Up Cued Recall ---------------------------------------------------------
            box(
              title = tags$b("Scoring Set Up"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              htmlOutput("cued_responsesUI"),
              htmlOutput("cued_keyUI"),
              htmlOutput("cued_key.trialUI"),
              htmlOutput("cued_idUI"),
              htmlOutput("cued_id.trialUI"),
              htmlOutput("cued_group.byUI"),
              sliderInput('cued_cutoff', "Choose the scoring cutoff:", min = 0, max = 5, value = 0, step = 1,
                          round = TRUE),
              checkboxInput('cued_flag', "Do you want to flag for outliers?", value = FALSE, width = NULL),
              actionButton("cued_recall_go", "Score Your Data")
          ), # box

          # Cued Recall Output ---------------------------------------------------------
          box(
            title = tags$b("Scored Output"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(" "),
            HTML("<b>Here's the scored long version output:</b>"),
            p(" "),
            DTOutput("cued_recall_scored"),
            tags$style(type = "text/css",
                       ".shiny-output-error {visibility: hidden;}",
                       ".shiny-output-error:before {visibility: hidden;}")
          ), # box

          box(
            title = tags$b("Summarized Output"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(" "),
            HTML("<b>Here's the scored averaged by participant output:</b>"),
            p(" "),
            DTOutput("cued_recall_participant"),
            tags$style(type = "text/css",
                       ".shiny-output-error {visibility: hidden;}",
                       ".shiny-output-error:before {visibility: hidden;}"),
            p(" "),
            HTML("<b>Here's the scored group output:</b>"),
            p(" "),
            DTOutput("cued_recall_group.by"),
            tags$style(type = "text/css",
                       ".shiny-output-error {visibility: hidden;}",
                       ".shiny-output-error:before {visibility: hidden;}")

          ), # box

          # cued Recall Graph ---------------------------------------------------------
          box(
            title = tags$b("Graphed Cued-Recall Output"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(" "),
            HTML("<b>Here's a graph of your results:</b>"),
            p(" "),
            plotOutput("cued_recall_graph"),
            tags$style(type = "text/css",
                       ".shiny-output-error {visibility: hidden;}",
                       ".shiny-output-error:before {visibility: hidden;}")
            ) # box
          ) #fluidrow
  ) #close page
