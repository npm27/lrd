#####load libraries####
library(shiny)
library(ggplot2)
library(vecsets)
library(Hmisc)
library(RecordLinkage)

####Custom Functions#####
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

#x is a dataframe column containing participant responses
#y is a dataframe column with the answer key (probably uploaded as a separate .csv) y won't be the same length as x
#z is the participant id. x and z should come from the same df (sample data.csv)

match_free_recall = function(x, key = y, id = z, cutoff = g, other = NULL){

  nb = is.null(other)

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

  if (nb == FALSE){

    other = data.frame(other)
    output3 = cbind(df2, other)

  }

  else if (nb == TRUE){

    output = df2

  }

}

##Proportion correct free recall function
prop.correct.f = function(x, key = y, id = z, flag = FALSE, group.by = NULL){

  a = is.null(group.by)

  input = data.frame(id, x)

  temp = c() #Make a blank vector for storage
  temp2 = c()
  temp3 = c()

  if (a == TRUE & flag == FALSE) {

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

  else if (a == TRUE & flag == TRUE) {

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

  else if (a == FALSE & flag == FALSE){

    input = cbind(input, group.by)

    k = length(key)

    ##Get number of 1's for each participant
    for (i in unique(input$id)){

      for (g in unique(input$group.by)){

        input2 = subset(input, #subset by participant id
                        input$id == i)

        input3 = subset(input2,
                        input2$group.by == g)
        print(input3)

        output = as.numeric(table(input3$x))[2] #Get each participants total number of correct responses and divide by key

        temp = c(output, temp)
        temp2 = c(temp2, g)
        temp3 = c(temp3, i)


      }

    }

    name.list = data.frame(temp3)
    conditions = data.frame(temp2)

    output2 = data.frame(temp3, temp, temp2)
    colnames(output2)[1:3] = c("ID", "Proportion_Correct", "Condition")

    output2$Proportion_Correct = output2$Proportion_Correct / k

    print(output2)

  }

}

##USE THIS ONE WHEN NEEDING TO COLLAPSE ACROSS PARTICIPANT (SO ID WOULD BE ONE OF THE GROUPING CONDITIONS)
prop.correct.f2 = function(x, key = y, id = z, flag = FALSE, group.by = NULL){

  a = is.null(group.by)

  input = data.frame(id, x)

  temp = c() #Make a blank vector for storage
  temp2 = c()
  temp3 = c()
  temp4 = data.frame()

  if (a == TRUE & flag == FALSE) {

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

  else if (a == TRUE & flag == TRUE) {

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

  else if (a == FALSE & flag == FALSE){

    input = cbind(input, group.by)

    k = length(key)

    ##Get number of 1's for each participant
    for (i in unique(input$id)){

      for (g in unique(input$group.by)){

        input2 = subset(input, #subset by participant id
                        input$id == i)

        input3 = subset(input2,
                        input2$group.by == g)
        #print(input3)

        output = as.numeric(table(input3$x))[2] #Get each participants total number of correct responses and divide by key

        temp = c(output, temp)
        temp2 = c(temp2, g)
        temp3 = c(temp3, i)


      }

    }

    name.list = data.frame(temp3)
    conditions = data.frame(temp2)

    output2 = data.frame(temp3, temp, temp2)
    colnames(output2)[1:3] = c("ID", "Proportion_Correct", "Condition")

    #print(output2$Proportion_Correct) ##Need to take these...

    output3 = tapply(input$x, list(input$id, input$group.by), length) ##And divide them by these

    output3 = data.frame(output3)

    for (i in 1:length(output3)){


      temp5 = output3[ , i]
      temp5 = data.frame(temp5)
      colnames(temp5)[1] = "Value"

      temp4 = rbind(temp4, temp5)

    }

    #Convert to a vector
    temp4 = as.vector(temp4[ , 1])

    #Now divide!
    output2$Proportion_Correct = output2$Proportion_Correct / temp4

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

                             p(strong("Welcome to the lrd free recall calculator!")),

                             helpText("To begin, select the appropriate settings based on your upload file and choose the criteria used for scoring",
                                     "(i.e., 0 = strictest, 5 = is most lenient). The scoring criteria uses a Levenshtein distance measure to match participant responses to the answer key."),

                             helpText("The scored dataset can be viewed by clicking on the \"Scored Output\" tab and can be downloaded using the download button at the top of tab. Use the slidebar to adjust the scoring cutoff.",
                                      "Each participant's mean proportion of correct responses and corresponding z-score can be viewed",
                                      "using the \"Proportion Correct\" tab. This output can be customized based on any of the optional condition columns that are attached to the upload .csv file. These values can be downloaded using the download button at the top of the tab.",
                                      "Please note that z-scores are only generated when splitting the data on one condition.",
                                      "The \"Plots\" tab can be used to visualize the dataset. Plots can be customized based on the optional condition columns in the dataset.",
                                      "If no condition columns are included, this tab will display the distribution of participant responses (This can also be viewed by selecting \"id\" as the grouping condition.",
                                      "Please note that tabs will not populate until a .csv file in the correct format has been uploaded."),

                             p(strong("Instructions for Formatting Your Upload Files:")),

                             helpText("Two upload .csv files are required. The first .csv file needs to contain the data to be scored. This file must contain at least two columns arranged in the following order:",
                                      "A column containing unique identifiers for each participant followed by a column containing participant responses.",
                                      "The second .csv file contains the scoring key used to process the data. This file should be formatted to contain only one column.",
                                      "Examples of each file structure are available ", a("here.", href = "https://osf.io/evpq8/", target = "_blank"))),

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

  ##get the input data for scoring

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

            percentage = input$Percentage  #This is controlling the value from the slider

            colnames(dat)[1:2] = c("ID", "Response")
            dat$Response = tolower(dat$Response)

            colnames(key)[1] = "KEY"
            key$KEY = tolower(key$KEY)
            #dat2 = dat[ , c(4:length(dat))]

            ##Now score using the free recall function
            ##With extra column
            if (length(dat) > 2){

              matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = dat[ c(3:length(dat))])

            }

            #Without extra columns
            else if (length(dat) == 2){

              matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = NULL)

            }
        }
    )

    ##This section controls the prop correct tab dropdown menus
    #First menu
    output$select_grouping1 = renderUI({

        df = getData()

        items = colnames(df)

        #No condition columns
        if (length(df) == 4) {

            items = names(df)
            items = items[1]
            selectInput("conditions2", "Select First Grouping Variable", items)

        }

        #condition columns
        else if (length(df) > 4) {

            items = names(df)
            items = items[ -c(2:4)]
            selectInput("conditions2", "Select First Grouping Variable", items)

        }

    })

    #Second menu
    output$select_grouping2 = renderUI({

        df = getData()

        items = colnames(df)

        #No condition columns (This one works as expected)
        if (length(df) == 4) {

            items = names(df)
            items = "None Available"
            selectInput("conditions3", "Select Second Grouping Variable (Optional)", items)

        }

        #One extra column (Finally got this one working!)
        else if (length(df) == 5) {

          df2 = df[ ,-c(1:3)]

          if (input$conditions2 == "id"){

            items = names(df2)

            items = append(items, "None")
            items = items[items != "Scored"]

            selectInput("conditions3", "Select Second Grouping Variable (Optional)", items, selected = "None")

          }

          else if(input$conditions2 != "id"){

            items = names(df2)

            items = "None"

            selectInput("conditions3", "Select Second Grouping Variable (Optional)", items, selected = "None")

          }

        }

        ##Multiple extra columns (This one also works)
        else if (length(df) > 5) {

            df2 = df[ ,-c(1:4)]

            if (input$conditions2 == "id"){

                items = names(df2)

                items = append(items, "None")

                selectInput("conditions3", "Select Second Grouping Variable (Optional)", items, selected = "None")

                }

           else if(input$conditions2 != "id"){

               items = names(df2)

               items = append(items, "None")

               selectInput("conditions3", "Select Second Grouping Variable (Optional)", items, selected = "None")

           }

        }

    })

    ##Get data for the prop correct tab

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

      percentage = input$Percentage

      colnames(dat)[1:2] = c("ID", "Response")
      dat$Response = tolower(dat$Response)

      colnames(key)[1] = "KEY"
      key$KEY = tolower(key$KEY)
      #dat2 = dat[ , c(4:length(dat))]

      ##Now score using the free recall function
      ##When the data contains multiple condition columns:
      if (length(dat) > 3){

        matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = dat[ c(3:length(dat))])

        ##If grouping by one variable
        if(input$conditions3 == "None"){

          ##IF specifically grouping by sub ID
          if (input$conditions2 == "id"){

            final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, flag = TRUE)

            colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

          ##If the one variable is something other than sub ID
          else if (input$conditions2 != "id"){

            final = prop.correct.f(matched$Scored, key = matched$Scored, id = dat[ , input$conditions2], flag = TRUE)

            colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

        }
        ##If grouping by multiple variables
        else if (input$conditions3 != "None"){

          ##If ID is selected as the first grouping variable
          if (input$conditions2 == "id"){

            final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, group.by = dat[ , input$conditions3])

            #colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

          else if (input$conditions2 != "id" & input$conditions2 != input$conditions3){

            final = prop.correct.f2(matched$Scored, key = matched$Scored, id = dat[ , input$conditions2], group.by = dat[ , input$conditions3])

            #colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

          ##If inputs 1 and 2 match
          else if (input$conditions2 == input$conditions3){

            final = "Please Select another option!"

          }

      ##Now compute the proportion correct
      #final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, flag = TRUE)
        }
      }
      ##If the dataset contains one condition column
      else if (length(dat) == 3){

        matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = dat[3])

        ##If grouping by one variable
        if(input$conditions3 == "None"){

          ##IF specifically grouping by sub ID
          if (input$conditions2 == "id"){

            final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, flag = TRUE)

            colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

          ##If the one variable is something other than sub ID
          else if (input$conditions2 != "id"){

            final = prop.correct.f(matched$Scored, key = key$KEY, id = dat[ , input$conditions2], flag = TRUE)

            colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
            final

          }

        }

        else if(input$conditions3 != "None"){

          final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, group.by = dat[ , input$conditions3])

          colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
          final

        }

      }

      ##If the dataset does not have condition columns
      else if (length(dat) == 2){

        matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = NULL)

        final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, group.by = NULL)

        colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
        final

      }

    }
    )

    ##Get data for the plots
    getData3 = reactive({

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

        percentage = input$Percentage

        colnames(dat)[1:2] = c("ID", "Response")
        dat$Response = tolower(dat$Response)

        colnames(key)[1] = "KEY"
        key$KEY = tolower(key$KEY)
        #dat2 = dat[ , c(4:length(dat))]

        if (length(dat) > 2) {

          matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = dat[ c(3:length(dat))])

          prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

          final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, flag = TRUE)

          colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
          final

        }

        else if (length(dat) == 2) {

          matched = match_free_recall(dat$Response, key = key$KEY, id = dat$ID, cutoff = percentage, other = NULL)

          final = prop.correct.f(matched$Scored, key = key$KEY, id = dat$ID, group.by = NULL)

          colnames(final)[1:2] = c("Participant", "Proportion Correct Response")
          final

          }
        }
    )

    output$contents = renderTable(

        getData()

    )

    output$contents2 = renderTable(

        getData2()

    )

    ##This controls the plots tab dropdown menus
    output$toCol = renderUI({

        df = getData()

        items = colnames(df)

        #If no condition columns (data has four columns after scoring)
            if (length(df) == 4) {

                items = names(df)
                items = items[1]
                selectInput("conditions", "Select Grouping Condition", items)

            }

        #If the data has condition columns
            else if (length(df) > 4) {

                items = names(df)
                items = items[ -c(2:4)]
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

            bar = ggplot(dat1, aes(dat1[ ,input$conditions], dat1[ ,4]))

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
