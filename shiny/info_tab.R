info_tab <-
  tabItem(tabName = "info_tab",
          fluidRow(
            box(
              title = tags$b("lrd: An app for quickly scoring lexical data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(" "),
              HTML("<b>Written by: </b>
                   Nicholas P. Maxwell, Erin M. Buchanan, Mark Huff"),
              p(" "),
              HTML("<b>Cite our paper:</b> Nick! Update this!"),
              p(" "),
              HTML("<b>How to navigate the app</b>:
                   Use the navigation side bar on the left to select
                   the type of data you are trying to score. You can
                   score free recall, cued recall, and sentence answers. <br>
                   On each page, you can upload your data and answer key
                   to score the data. Each of these boxes can be closed to view
                   other output from the function."),
              p(" "),
              HTML("<b>Arrange Data</b>:
                   Use the arrange data function to convert responses in wide
                   format (i.e., one row per participant) to long format (i.e.,
                   one row per answer per participant. All functions assume your data
                   is in long format!"),
              p(" "),
              HTML("<b>What is the scoring cutoff?</b>
                   Studies with open ended responses often have typos and other spelling
                   mistakes. The scoring cutoff ranges from 0 (most strict, must be
                   spelled exactly correct acccording to the answer key) to 5 (least
                   strict and many things will match, so choose this carefully).
                   What is this number? The score represents the number of insertions,
                   deletions, and substitutions one might need to convert the given
                   response to the answer key (Levenshtein distance). Therefore, a 1
                   represents one letter difference, while 5 represents 5 changes.")

              ) # box
            ) #fluidrow
          ) #close page
