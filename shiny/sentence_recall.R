sentence_recall <-
  tabItem(tabName = "sentence_recall",

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
              fileInput('sentence_data', 'Choose Data File'),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Upload Your Answer Key:</b> Your answer key can be in
                   the original data file, just upload it again here."),
              p(" "),
              fileInput('answer_key_sentence', 'Choose Answer File'),
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
              HTML("<b>Here's your sentence-recall data:</b>"),
              p(" "),
              DTOutput("sentence_recall_data"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's your answer key:</b>"),
              DTOutput("sentence_recall_answer"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Set Up Sentence Recall ---------------------------------------------------------
            box(
              title = tags$b("Scoring Set Up"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              htmlOutput("sentence_responsesUI"),
              htmlOutput("sentence_keyUI"),
              htmlOutput("sentence_key.trialUI"),
              htmlOutput("sentence_idUI"),
              htmlOutput("sentence_id.trialUI"),
              htmlOutput("sentence_group.byUI"),
              sliderInput('sentence_cutoff', "Choose the scoring cutoff:", min = 0, max = 5, value = 0, step = 1,
                          round = TRUE),
              checkboxInput('sentence_flag', "Do you want to flag for outliers?", value = FALSE, width = NULL),
              textInput("sentence_token", "What is the delimiter for your sentence tokens?
                        Default is a space, please delete it in the box if you do not
                        wish to use a space.", value = " "),
              actionButton("sentence_recall_go", "Score Your Data")
            ), # box

            # sentence Recall Output ---------------------------------------------------------
            box(
              title = tags$b("Scored Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's the scored long version output:</b>"),
              p(" "),
              DTOutput("sentence_recall_scored"),
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
              DTOutput("sentence_recall_participant"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              p(" "),
              HTML("<b>Here's the scored group output:</b>"),
              p(" "),
              DTOutput("sentence_recall_group.by"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")

            ), # box

            # sentence Recall Graph ---------------------------------------------------------
            box(
              title = tags$b("Graphed Sentence-Recall Output"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Here's a graph of your results:</b>"),
              p(" "),
              plotOutput("sentence_recall_graph"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ) # box
          ) #fluidrow
  ) #close page
