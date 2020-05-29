#####load libraries####
library(shiny)
library(ggplot2)

##Erin's clean up code
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

####Custom Functions#####
##Percent Match
Percent_Match = function(x, key = y, id = z, weight = FALSE, weight.by = NULL,
                         other = NULL, cutoff = qqq){

    xx = weight.by

    input = data.frame(id, x, key)

    input$id = as.character(input$id)
    input$x = as.character(input$x)
    input$key = as.character(input$key)

    input$length_x = nchar(input$x)
    input$length_key = nchar(input$key)
    input$diff = input$length_x - input$length_key

    sub1 = subset(input,
                  input$diff >= 1)

    sub2 = subset(input,
                  input$diff == 0)

    sub3 = subset(input,
                  input$diff < 0)

    sub1 = sub1[ , -c(4:6)]
    sub2 = sub2[ , -c(4:6)]
    sub3 = sub3[ , -c(4:6)]

    char.x1 = vector(mode = "character", length = nrow(sub1))
    char.x2 = vector(mode = "character", length = nrow(sub2))
    char.x3 = vector(mode = "character", length = nrow(sub3))

    char.y1 = vector(mode = "character", length = nrow(sub1))
    char.y2 = vector(mode = "character", length = nrow(sub2))
    char.y3 = vector(mode = "character", length = nrow(sub3))

    percent_match1 = vector(mode = "character", length = nrow(sub1))
    percent_match2 = vector(mode = "character", length = nrow(sub2))
    percent_match3 = vector(mode = "character", length = nrow(sub3))

    for (i in 1:nrow(sub1)) {

        char.x1[i] = strsplit(sub1$x[i], "")
        char.y1[i] = strsplit(sub1$key[i], "")

    }

    for (k in 1:nrow(sub1)) {

        c = char.x1[[k]]
        d = char.y1[[k]]

        percent_match1[k] = length(na.omit(d[is.element(c, d)])) /
            length(c)

        sub1$percent_match = percent_match1

    }

    for (h in 1:nrow(sub2)) {

        char.x2[h] = strsplit(sub2$x[h], "")
        char.y2[h] = strsplit(sub2$key[h], "")

    }

    for (j in 1:nrow(sub2)) {

        e = char.x2[[j]]
        f = char.y2[[j]]

        percent_match2[j] = length(na.omit(f[is.element(e, f)])) /
            length(e)

        sub2$percent_match = percent_match2

    }

    for (q in 1:nrow(sub3)) {

        char.x3[q] = strsplit(sub3$x[q], "")
        char.y3[q] = strsplit(sub3$key[q], "")

    }

    for (p in 1:nrow(sub3)) {

        r = char.x3[[p]]
        s = char.y3[[p]]

        percent_match3[p] = length(unique(r[r %in% s[s %in% r]])) /
            length(s)

        sub3$percent_match = percent_match3

    }

    output = rbind(sub1, sub2, sub3)
    colnames(output)[2] = "Response"
    output = output[order(as.numeric(rownames(output))),,drop = FALSE]


    if (is.null(other) == FALSE){

        output$Scored = as.numeric(output$percent_match >= cutoff)
        other = data.frame(other)
        output3 = cbind(output, other)
        print(output3)
    }

    else if (is.null(other) == TRUE){

        output$Scored = as.numeric(output$percent_match >= cutoff)
        print(output)

    }

    else if (weight == TRUE){

        if (weight.by <= 1 & is.null(weight.by) == FALSE){

            sub4 = subset(output,
                          output$percent_match == 1)
            sub5 = subset(output,
                          output$percent_match == 0)
            sub6 = subset(output,
                          output$percent_match < 1 & output$percent_match > 0)

            sub4$weighted_match = as.numeric(sub4$percent_match)
            sub5$weighted_match = as.numeric(sub5$percent_match)

            sub6$weighted_match = as.numeric(sub6$percent_match) + (weight.by / nchar(sub6$key))

            output2 = rbind(sub4, sub5, sub6)

            output2 = output2[order(as.numeric(rownames(output2))),,drop = FALSE]
            print(output2)

        }

        else if (is.null(weight.by) == FALSE & weight.by > 1) {

            print("weight.by must be a value between 0 and 1!")

        }

    }

    else if (weight == FALSE) {

        print(output)

    }

}

##Describe condition
describe.condition = function(x, group.by = y, id = z){

    input = data.frame(id, x, group.by)

    #Get participant level means
    tap1 = tapply(input$x, list(input$id, input$group.by), mean, na.rm = T)
    tap1 = data.frame(tap1)

    #Put everything in a dataframe
    names = row.names(tap1)
    temp = cbind(names, tap1)

    #Now get all the condition level descriptives
    Mean = apply(temp[2:length(temp)], 2, mean) #mean
    SD = apply(temp[2:length(temp)], 2, sd)#sd
    SE = SD / sqrt(length(unique(temp$names))) #se
    CI.95 = SE * 1.96 #95% CI
    Upper = Mean + CI.95 #upper Limit
    Lower = Mean - CI.95 #Lower Limit

    #Get everything in a dataframe for output
    names = data.frame(c("Mean", "SD", "SE", "95% CI", "Upper", "Lower"))

    labels = colnames(temp[ , -1])

    Mean = t(data.frame(Mean))
    SD = t(data.frame(SD))
    SE = t(data.frame(SE))
    CI.95 = t(data.frame(CI.95))
    Upper = t(data.frame(Upper))
    Lower = t(data.frame(Lower))

    temp2 = rbind(Mean, SD, SE, CI.95, Upper, Lower)
    temp2 = cbind(names, temp2)
    colnames(temp2)[1] = "names"

    condition.means = rbind(temp, temp2)

    colnames(condition.means)[1] = " "

    print(condition.means[ c((nrow(condition.means) - 5):nrow(condition.means)), ], row.names = F)

}

##Proportion Correct
prop.correct = function(x, group.by = NULL, id = z, flag = FALSE){

    a = is.null(group.by)

    if (a == TRUE & flag == FALSE) {

        input = data.frame(id, x)

        #Get participant level means
        tap1 = tapply(input$x, input$id, mean, na.rm = T)
        tap1 = data.frame(tap1)

        colnames(tap1)[1] = "Value"

        tap1$z = scale(tap1$Value)

        print(tap1)

    }

    else if (a == TRUE & flag == TRUE) {

        input = data.frame(id, x)

        #Get participant level means
        tap1 = tapply(input$x, input$id, mean, na.rm = T)
        tap1 = data.frame(tap1)

        colnames(tap1)[1] = "Value"

        tap1$z = scale(tap1$Value)

        tap1$Flagged = rep(" ")
        tap1$Flagged[tap1$z >= 3] = "*"
        tap1$Flagged[tap1$z <= -3] = "*"

        colnames(tap1)[3] = " "

        print(tap1)

    }

    else if (a == FALSE & flag == FALSE) {

        input = data.frame(id, x, group.by)

        #Get participant level means grouped by condition
        tap2 = tapply(input$x, list(input$id, input$group.by), mean, na.rm = T)
        tap2 = data.frame(tap2)

        print(tap2)

    }

}

####Set up page####
ui = fluidPage(

    fluidPage(

        titlePanel("lrd: An app for quickly scoring lexical data"),

        sidebarLayout(

            sidebarPanel(

                fileInput('file1', 'Choose CSV File',

                          accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv')),

                tags$hr(),

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

                sliderInput("Percentage", "Select Cutoff Percentage for Scoring:",
                                min = 50, max = 100,
                                value = 75),

                downloadButton('downloadData', 'Download')
            ),

            mainPanel(
                tabsetPanel(

                    tabPanel("Instructions",

                             p(strong("Welcome to lrd!")),

                             helpText("To begin, select the appropriate settings based on your input file, choose the cutoff percentage used for scoring",
                                     "(i.e., what percentage of characters must match between response and key for items to be counted as correct; 100% = Strictest), and ",
                                     "upload your file. Scored output will appear below. You can download a .csv file of the scored data using the button at the bottom of the screen."),

                             helpText("The scored dataset can be viewed by clicking on the \"Scored Output\" tab. The mean proportion correct and corresponding z-score for each participant can be viewed",
                                      "using the \"Proportion Correct\" tab. The \"Distrubitions\" tab can be used to visualize the dataset. Please note that these tabs will not populate until a datset has been uploaded."),

                             p(strong("Instructions for Formatting Your Upload File:")),

                             helpText("Input file must contain at least three columns arranged in the following order:",
                                      "A unique participant identifier, participant resopnses, a scoring key.",
                                      "After uploading your file, the scored data will be displayed below.",
                                      "Any additional columns must be placed after these.")),

                    tabPanel("Scored Output",

                            tableOutput('contents')),

                    tabPanel("Proportion Correct",

                            tableOutput('contents2')),

                    tabPanel("Distributions", h3("Proportion Correct", align = "center"),

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

            dat = read.csv(inFile$datapath, header = input$header, sep = input$sep,
                 quote = input$quote)

            ##use lrd to process the output

            percentage = input$Percentage / 100

            colnames(dat)[1:3] = c("ID", "Response", "Key")
            dat$Response = tolower(dat$Response)
            dat$Key = tolower(dat$Key)
            dat2 = dat[ , c(4:length(dat))]

            Percent_Match(dat$Response, key = dat$Key, id = dat$ID, other = dat[ c(4:length(dat))], cutoff = percentage)

        }
    )

    getData2 = reactive({

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

        matched = Percent_Match(dat$Response, key = dat$Key, id = dat$ID, other = dat[ c(4:length(dat))], cutoff = percentage)

        prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        colnames(final_out)[2] = "Proportion Correct Response"
        final_out

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

        matched = Percent_Match(dat$Response, key = dat$Key, id = dat$ID, other = dat[ c(4:length(dat))], cutoff = percentage)

        prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        colnames(final_out)[2] = "Proportion_Correct"

        ggplot(final_out, aes(Proportion_Correct)) + xlab("Proportion Correct") + ylab("Frequency") + geom_histogram(binwidth = 0.2, fill = "blue4", color = "white") + cleanup

        }
    )

    output$contents = renderTable(

        getData()

    )

    output$contents2 = renderTable(

        getData2()

    )

    output$distPlot = renderPlot({

        getData3()

        })


    output$downloadData = downloadHandler(

        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },

        content = function(file) {

            write.csv(getData(), file)

        })

}

# Run the app ----
shinyApp(ui, server)
