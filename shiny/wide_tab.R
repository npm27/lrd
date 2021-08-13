wide_tab <-
  tabItem(tabName = "wide_tab",
          fluidRow(

            # Upload your data --------------------------------------------------------
            box(
              title = tags$b("Upload Your Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              p("Use this page to convert your wide formatted free answer responses
                to long format with one answer per row."),
              HTML("<b>Upload Your Data File:</b> Nearly all file types supported!
                   Only one header row is supported."),
              HTML("An example of how to format the upload file is available <a href=\"https://osf.io/gyvjz/\">here</a>."),
              p(" "),
              fileInput('wide_input', 'Choose Data File'),
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
              HTML("<b>Here's your wide formatted data:</b>"),
              p(" "),
              DTOutput("wide_data"),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}")
            ), # box

            # Data Inputs ---------------------------------------------------------
            box(
              title = tags$b("Arrangement Set Up"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              htmlOutput("wide_responsesUI"),
              htmlOutput("wide_idUI"),
              htmlOutput("wide_repeatedUI"),
              textInput("wide_sep", "What is the separator for the
              participant answers? Be sure to only include a space
                        if you mean to use spaces.", value = ""),
              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              actionButton("wide_data_go", "Convert Your Data")
            ), # box

            # Download Data ---------------------------------------------------------
            box(
              title = tags$b("Download Converted Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              DTOutput("long_data_output")
            ) #box
          ) #fluidrow
  ) #close page
