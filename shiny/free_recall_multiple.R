free_recall_multiple <-
  tabItem(tabName = "free_recall_multiple",
          fluidRow(

            # Upload your data --------------------------------------------------------
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
              fileInput('free_data_multiple', 'Choose Data File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Upload Your Answer Key:</b> Your answer key can be in
                   the original data file, just upload it again here."),
              p(" "),
              fileInput('answer_key_multiple', 'Choose Answer File'),
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
              HTML("<b>Here's your free-recall data:</b>"),
              p(" "),
              DTOutput("multiple_recall_data"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's your answer key:</b>"),
              DTOutput("multiple_recall_answer"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Set Up Free Recall ---------------------------------------------------------
            box(
              title = tags$b("Scoring Set Up"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              htmlOutput("multiple_responsesUI"),
              htmlOutput("multiple_keyUI"),
              htmlOutput("multiple_key.trialUI"),
              htmlOutput("multiple_idUI"),
              htmlOutput("multiple_id.trialUI"),
              htmlOutput("multiple_group.byUI"),
              sliderInput('multiple_cutoff', "Choose the scoring cutoff:", min = 0, max = 5, value = 0, step = 1,
                          round = TRUE),
              checkboxInput('multiple_flag', "Do you want to flag for outliers?", value = FALSE, width = NULL),
              htmlOutput("multiple_positionUI"),
              actionButton("multiple_recall_go", "Score Your Data")
            ), # box

            # Free Recall Output ---------------------------------------------------------
            box(
              title = tags$b("Scored Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's the scored long version output:</b>"),
              p(" "),
              DTOutput("multiple_recall_scored"),
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
              DTOutput("free_recall_participant"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's the scored group output:</b>"),
              p(" "),
              DTOutput("free_recall_group.by"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")

            ), # box

            # Free Recall Graph ---------------------------------------------------------
            box(
              title = tags$b("Graphed Free-Recall Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's a graph of your results:</b>"),
              p(" "),
              plotOutput("free_recall_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Serial Position ---------------------------------------------------------
            box(
              title = tags$b("Serial Position Curves Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's a your serial position data:</b>"),
              p(" "),
              DTOutput("serial_data_output"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's a graph of your results:</b>"),
              p(" "),
              plotOutput("serial_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # PFR Position ---------------------------------------------------------
            box(
              title = tags$b("Probability of First Response Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's your probability of first response data:</b>"),
              p(" "),
              DTOutput("pfr_data_output"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's a graph of your results:</b>"),
              plotOutput("pfr_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box


            # CRP Position ---------------------------------------------------------
            box(
              title = tags$b("Conditional Response Probability Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's your conditional response probability data:</b>"),
              p(" "),
              DTOutput("crp_data_output"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's a graph of your results:</b>"),
              plotOutput("crp_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ) # box

          ) #fluidrow
        ) #close page
