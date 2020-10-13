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
              HTML("<b>Important</b>: Your data file must be in long format for the
                   scoring to accurately process. You can use the Arrange Data tab to
                   first convert the data."),
              p(" "),
              HTML("<b>Upload Your Data File:</b> Nearly all file types supported!
                   Only one header row is supported."),
              p(" "),
              fileInput('free_data', 'Choose Data File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Upload Your Answer Key:</b> Your answer key can be in
                   the original data file, just upload it again here."),
              p(" "),
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
              p(" "),
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
              title = tags$b("Scoring Set Up"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              htmlOutput("free_responsesUI"),
              htmlOutput("free_keyUI"),
              htmlOutput("free_idUI"),
              htmlOutput("free_group.byUI"),
              sliderInput('free_cutoff', "Choose the scoring cutoff:", min = 0, max = 5, value = 0, step = 1,
                          round = TRUE),
              checkboxInput('free_flag', "Do you want to flag for outliers?", value = FALSE, width = NULL),
              htmlOutput("free_positionUI"),
              actionButton("free_recall_go", "Score Your Data")
            ), # box

            # Free Recall Output ---------------------------------------------------------
            box(
              title = tags$b("Scored Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's the scored participant output:</b>"),
              p(" "),
              DTOutput("free_recall_scored"),
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
              HTML("<b>Here's the scored participant output:</b>"),
              p(" "),
              DTOutput("free_recall_participant"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's the scored group output:</b>"),
              p(" "),
              DTOutput("free_recall_groupby"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")

            ), # box

            # Free Recall Graph ---------------------------------------------------------
            box(
              title = tags$b("Graphed Free Recall Output"),
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
              HTML("<b>Here's a graph of your results:</b>"),
             # plotOutput("free_recall_graph"),
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
              HTML("<b>Here's a graph of your results:</b>"),
              #plotOutput("free_recall_graph"),
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
              HTML("<b>Here's a graph of your results:</b>"),
              #plotOutput("free_recall_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ) # box

          ) #fluidrow
        ) #close page
