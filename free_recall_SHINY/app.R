#####load libraries####
library(shiny)
library(ggplot2)
library(vecsets)
library(Hmisc)

####Custom Functions#####
#x is a dataframe column containing participant responses
#y is a dataframe column with the answer key (probably uploaded as a separate .csv) y won't be the same length as x
#z is the participant id. x and z should come from the same df (sample data.csv)

match_free_recall = function(x, key = y, id = z, cutoff = g){

  df = cbind(id, x)
  df = data.frame(df)
  scores = data.frame() #Make an empty dataframe for storage

  for (i in x) { #i = individual participant responses


    for (k in 1:nrow(df)) {

      scores2 = levenshteinDist(i, key) #Get a matrix of levenshtein distances. Could do any type of lexical overlap measure here.
      ##Really just trying to find a way to match up responses with key since subjects aren't going to recall things in the same order

    }


    scores = rbind(scores, scores2)

    #Adds things back to the dataframe

    colnames(scores)[1:length(scores)] = key[1:length(key)]

    #Now that I have the key arranged: Pull the key item that is closest to each response.

    output = c() #Make an empty vector for storage

    scores2 = apply(scores, 1, min) #Compute the minimum score (would need max depending on the overlap measure being used)

    for (n in 1:nrow(scores)){

      for (m in 1:length(scores)){

        if (scores2[n] <= cutoff){ #find all the minimums that meet the threshold

          if (scores[n,m] <= cutoff){ #The sign would need to be flipped if doing a percent match measure.

            output2 = colnames(scores)[m] #Not sure what to do for ties yet. colnames are the items in the key. So pull the key item matching the response

          }

        }

        else {

          output2 = NA #If there are no close matches, return an NA. NA responses could be saved to an intrusion list or something

        }

      }

      output = c(output, output2)

    }

  }

  ##combine the output with the sub IDs and participant responses
  df2 = cbind(df, output)

  ###Score data####
  df2$Scored = is.na(df2$output)
  df2$Scored = as.numeric(df2$Scored)

  df2$Scored = (df2$Scored - 1) * -1 #Invert 1's and 0's (0 should equal incorrect, 1 = correct)

  colnames(df2)[2] = "Response"

  return(df2)

}

##Proportion correct free recall function

prop.correct.f = function(x, key = y, id = z, flag = FALSE){

  input = data.frame(id, x)

  temp = c() #Make a blank vector for storage

  if (flag == FALSE) {

    k = length(key)

    ##Get number of 1's for each participant
    for (i in unique(input$id)){

      input2 = subset(input, #subset by participant id
                      input$id == i)

      output = as.numeric(table(input2$x))[2] / k #Get each participants total number of correct responses and divide by key

      temp = c(output, temp)

    }

    name.list = unique(input$id)

    output2 = data.frame(name.list, temp)

    colnames(output2)[1:2] = c("ID", "Proportion_Correct")

    output2$Z = scale(output2$Proportion_Correct)

    print(output2)

  }

  else if (flag == TRUE) {

    k = length(key)

    ##Get number of 1's for each participant
    for (i in unique(input$id)){

      input2 = subset(input, #subset by participant id
                      input$id == i)

      output = as.numeric(table(input2$x))[2] / k #Get each participants total number of correct responses and divide by key

      temp = c(output, temp)

    }

    name.list = unique(input$id)

    output2 = data.frame(name.list, temp)

    colnames(output2)[1:2] = c("ID", "Proportion_Correct")

    output2$Z = scale(output2$Proportion_Correct)

    output2$Flagged = rep(" ")
    output2$Flagged[output2$z >= 3] = "*"
    output2$Flagged[output2$z <= -3] = "*"

    colnames(output2)[4] = " "

    print(output2)

  }

}


####Set up page####
ui = fluidPage(

    fluidPage(

        titlePanel("lrd: Free Recall Calculator"),

        sidebarLayout(

            sidebarPanel(

                fileInput('file1', 'Upload Output',

                          accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv')),

                fileInput('file2', 'Upload Key',

                          accept = c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                tags$hr(),

                tags$style(type = "text/css",
                           ".shiny-output-error {visibility: hidden;}",
                           ".shiny-output-error:before {visibility: hidden;}"),

                checkboxInput('header', 'Header', TRUE),

                radioButtons('sep', 'Separator',
                             c(Comma = ',',
                               Semicolon = ';',
                               Tab ='\t'),
                             ','),

                radioButtons('quote', 'Quote',
                             c(None ='',
                               'Double Quote'= '"',
                               'Single Quote'= "'"),
                             '"'),

                sliderInput("Percentage", "Select Value for Scoring:",
                                min = 0, max = 5,
                                value = 2),

            ),

            mainPanel(
                tabsetPanel(

                    tabPanel("Instructions",

                             helpText(" "),

                             p(strong("Welcome to lrd!")),

                             helpText("To begin, select the appropriate settings based on your upload file, choose the cutoff percentage used for scoring",
                                     "[EXPLAIN CUTOFF], and ",
                                     "upload your file."),

                             helpText("[EXPLAIN TABS HERE]"),

                             p(strong("Instructions for Formatting Your Upload File:")),

                             helpText("Two upload .csv files are required. [EXPLAIN THEM AND FORMATTING HERE]", a("here.", href = "https://osf.io/evpq8/", target = "_blank"))),

                    tabPanel("Scored Output",

                            helpText(" "),

                            downloadButton('downloadData', 'Download'),

                            helpText(" "),

                            tableOutput('contents')),

                    tabPanel("Proportion Correct",

                             helpText(" "),

                             downloadButton('downloadData2', 'Download'),

                             helpText(" "),

                             uiOutput("select_grouping1"),

                             helpText(" "),

                             uiOutput("select_grouping2"),

                             tableOutput('contents2')),

                    tabPanel("Plots",

                             helpText(" "),

                             uiOutput("toCol"),

                             plotOutput(outputId = "distPlot"))
                )
            )
        )
    )
)

####Set up server####
server = function(input, output) {

    getData = reactive({

        inFile = input$file1

        if (is.null(input$file1))
            return(NULL)

        inFile2 = input$file2

        if(is.null(input$file2))
            return(NULL)

            dat = read.csv(inFile$datapath, header = input$header, sep = input$sep,
                 quote = input$quote)

            key = read.csv(inFile2$datapath, header = input$header, sep = input$sep,
                  quote = input$quote)

            ##use the free recall scoring function

            percentage = input$Percentage  #I think this is controlling the value from the slider

            colnames(dat)[1:2] = c("ID", "Response")
            dat$Response = tolower(dat$Response)

            colnames(key)[1] = "KEY"
            key$KEY = tolower(key$KEY)
            #dat2 = dat[ , c(4:length(dat))]

            ##Now score using the free recall function
            matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage)

        }
    )

    output$select_grouping1 = renderUI({

        df = getData()

        items = colnames(df)

        if (length(df) == 5) {

            items = names(df)
            items = items[1]
            selectInput("conditions2", "Select First Grouping Variable", items)

        }

        else if (length(df) > 5) {

            items = names(df)
            items = items[ -c(2:5)]
            selectInput("conditions2", "Select First Grouping Variable", items)

        }

    })

    output$select_grouping2 = renderUI({

        df = getData()

        items = colnames(df)

        if (length(df) == 5) {

            items = names(df)
            items = "None Available"
            selectInput("conditions3", "Select Second Grouping Variable (Optional)", items)

        }

        else if (length(df) > 5) {

            df2 = df[ ,-c(1:5)]

            if (input$conditions2 == "id"){

                items = names(df2)

                items = append(items, "None")

                selectInput("conditions3", "Select Second Grouping Variable (Optional)", items, selected = "None")

                }

           else if(input$conditions2 != "id"){

               items = names(df2)

               items = append(items, "None")

               selectInput("conditions3", "Select Second Grouping Variable (Optional)", items)

           }

        }

    })

    getData2 = reactive({

      inFile = input$file1

      if (is.null(input$file1))
        return(NULL)

      inFile2 = input$file2

      if(is.null(input$file2))
        return(NULL)

      dat = read.csv(inFile$datapath, header = input$header, sep = input$sep,
                     quote = input$quote)

      key = read.csv(inFile2$datapath, header = input$header, sep = input$sep,
                     quote = input$quote)

      ##use the free recall scoring function

      percentage = input$Percentage  #I think this is controlling the value from the slider

      colnames(dat)[1:2] = c("ID", "Response")
      dat$Response = tolower(dat$Response)

      colnames(key)[1] = "KEY"
      key$KEY = tolower(key$KEY)
      #dat2 = dat[ , c(4:length(dat))]

      ##Now score using the free recall function
      matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage)

      ##Now compute the proportion correct
      final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, flag = TRUE)

    }
    )

    getData3 = reactive({

        inFile = input$file1

        if (is.null(input$file1))
            return(NULL)

        dat = read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)

        ##use lrd to process the output

        percentage = input$Percentage / 100

        colnames(dat)[1:3] = c("ID", "Response", "Key")
        dat$Response = tolower(dat$Response)
        dat$Key = tolower(dat$Key)
        #dat2 = dat[ , c(4:length(dat))]

        if (length(dat) > 3) {

            matched = Percent_Match(dat$Response, key = dat$Key, id = dat$ID, other = dat[ c(4:length(dat))], cutoff = percentage)

            prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

            Participant = row.names(prop.correct.output)
            Participant = as.data.frame(Participant)
            final_out = cbind(Participant, prop.correct.output)
            colnames(final_out)[2] = "Proportion Correct Response"
            final_out

        }

        else if (length(dat) == 3) {

            matched = Percent_Match(dat$Response, key = dat$Key, id = dat$ID, other = NULL, cutoff = percentage)

            prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

            Participant = row.names(prop.correct.output)
            Participant = as.data.frame(Participant)
            final_out = cbind(Participant, prop.correct.output)
            colnames(final_out)[2] = "Proportion Correct Response"
            final_out

            }
        }
    )

    output$contents = renderTable(

        getData()

    )

    output$contents2 = renderTable(

        getData2()

    )

    output$toCol = renderUI({

        df = getData()

        items = colnames(df)

            if (length(df) == 5) {

                items = names(df)
                items = items[1]
                selectInput("conditions", "Select Grouping Condition", items)

        }

            else if (length(df) > 5) {

                items = names(df)
                items = items[ -c(2:5)]
                selectInput("conditions", "Select Grouping Condition", items)

            }

    })

    output$distPlot =  renderPlot({

        dat1 = getData()
        dat2 = getData3()

        #described = describe.condition(dat$scored, id = dat$id, group.by = dat[ , input$conditions])

        if (input$conditions == "id") {

            colnames(dat2)[2] = "Proportion_Correct"

            hist1 = ggplot(dat2, aes(Proportion_Correct)) + xlab("Proportion Correct") + ylab("Frequency") + geom_histogram(binwidth = 0.2, fill = "blue4", color = "white") + cleanup

            hist1

        }

        else if (input$conditions != "id") {

            bar = ggplot(dat1, aes(dat1[ ,input$conditions], dat1[ ,5]))

            bar = bar +

                stat_summary(fun.y = mean,
                           geom = "bar",
                           fill = "Blue4",
                           color = "White") +

                stat_summary(fun.data = mean_cl_normal,
                         geom = "errorbar",
                         width = .2,
                         position = "dodge") +

                cleanup +
                xlab(input$conditions) +
                ylab("Mean Correct Response") +
                labs(caption = "Bars = 95% CI")

        bar

        }
    })

    output$downloadData = downloadHandler(

        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },

        content = function(file) {

            write.csv(getData(), file, row.names = F)

        })

    output$downloadData2 = downloadHandler(

        filename = function() {
            paste("prop.correct-", Sys.Date(), ".csv", sep = "")
        },

        content = function(file) {

            write.csv(getData2(), file, row.names = F)

        })

}

# Run the app ----
shinyApp(ui, server)
