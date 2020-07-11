info_tab <-
  tabItem(tabName = "info_tab",
          fluidRow(
            box(
              title = tags$b("lrd: An app for quickly scoring lexical data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "success",
              width = 12,
              p("Written by: XYZ"),
              p("Cite our paper: XZY"),
              p("How to navigate the app")
              ) # box
            ) #fluidrow
          ) #close page
