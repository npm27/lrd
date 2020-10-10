cued_recall <-
  tabItem(tabName = "cued_recall",
          fluidRow(
            box(
              title = tags$b("Cued Recall Scoring Instructions"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(strong("Welcome to lrd!")),

              helpText("To begin, select the appropriate settings based on your upload file,
                       choose the cutoff percentage used for scoring (i.e., what percentage
                       of characters must match between response
                       and key for items to be counted as correct; 100% = Strictest, 75% is
                       recommended), and upload your file."),

              helpText("The scored dataset can be viewed by clicking on the \"Scored Output\"
                       tab and can be downloaded using the download button at the top of tab.
                       Use the slidebar to adjust the scoring cutoff. Each participant's mean
                       proportion of correct responses and corresponding z-score can be viewed using
                       the \"Proportion Correct\" tab. This output can be customized based
                       on any of the optional condition columns that are attached to the
                       upload .csv file. These values can be downloaded using the download
                       button at the top of the tab. Please note that z-scores are only
                       generated when splitting the data on one condition. The \"Plots\" tab can
                       be used to visualize the dataset. Plots can be customized based on the
                       optional condition columns in the dataset. If no condition columns are
                       included, this tab will display the distribution of participant responses
                       (This can also be viewed by selecting \"id\" as the grouping condition.
                       Please note that tabs will not populate until a .csv file in the correct
                       format has been uploaded."),

              p(strong("Instructions for Formatting Your Upload File:")),

              helpText("The upload .csv file must contain at minimun three columns that are
                       arranged in the following order: A unique participant identifier,
                       participant responses, and a scoring key. After uploading your file,
                       the scored data will be displayed below. Any additional columns
                       (such as those denoting experimental conditions) must be placed after
                       these. An example of how to format the upload file is available",
                       a("here.", href = "https://osf.io/evpq8/", target = "_blank")),
            ), # box

            # Input information -------------------------------------------------------
            box(
              title = tags$b("Upload Your Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,

              fileInput('file1', 'Choose CSV File',
                        accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv')),

              tags$style(type = "text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),

              sliderInput("Percentage", "Select Cutoff Percentage for Scoring:",
                          min = 50, max = 100,
                          value = 75),

              p(" "),
              p(strong("Here's your data you uploaded:")),
              DTOutput("raw_cue_data")

            ), # box

            # Scored Information ------------------------------------------------------
            box(
              title = tags$b("Download Scored Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(strong("Download the data using the buttons at the bottom of the table.")),
              DTOutput("scored_cue_data")
            ) # box

          ) #fluidrow
        ) #close page
