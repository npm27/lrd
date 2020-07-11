




# select_grouping1 --------------------------------------------------------

output$select_grouping1 <- renderUI({

  df <- getData()

  items <- colnames(df)

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

    if(input$conditions3 == "None"){

      if (input$conditions2 == "id"){

        prop.correct.output = prop.correct(matched$Scored, id = dat$ID, flag = TRUE)

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        colnames(final_out)[2] = "Proportion Correct Response"
        final_out

      }

      else if (input$conditions2 != "id") {

        prop.correct.output = prop.correct(matched$Scored, id = dat[ , input$conditions2], flag = TRUE)

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        colnames(final_out)[1] = names(dat[input$conditions2])
        colnames(final_out)[2] = "Proportion Correct Response"
        final_out

      }
    }

    else if (input$conditions3 != "None"){

      if (input$conditions2 == "id"){

        prop.correct.output = prop.correct(matched$Scored, id = dat$ID, group.by = dat[ , input$conditions3])

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        #colnames(final_out)[2] = "Proportion Correct Response"
        final_out

      }

      else if (input$conditions2 != "id"){

        prop.correct.output = prop.correct(matched$Scored, id = dat[ , input$conditions2], group.by = dat[, input$conditions3])

        Participant = row.names(prop.correct.output)
        Participant = as.data.frame(Participant)
        final_out = cbind(Participant, prop.correct.output)
        #colnames(final_out)[1] = names(dat[input$conditions2])
        #colnames(final_out)[2] = "Proportion Correct Response"
        final_out

      }

    }

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
