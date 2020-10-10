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
                   is in long format!")

              ) # box
            ) #fluidrow
          ) #close page
