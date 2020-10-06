#' Proportion Correct Free Recalll
#'
#' This function computes the proportion of correct responses per participant. Proportions can either be separated by condition or collapsed across conditions. Note: This function returns mean values when used with non-binary data.
#'
#' @param x a vector containing participant scores
#' @param key a vector containing the scoring key
#' @param id a vector containing participant ID numbers
#' @param group.by an optional argument that can be used to group the output by one condition column
#' @return prints descriptive statistics. Output can be saved to a dataframe.
#' @export

##x is a df column of participant scores
##z is a column of sub ids
##y is the answer key (most likely going to be uploaded as a separate file)

prop.correct.f.shiny = function(x, key = y, id = z, flag = FALSE, group.by = NULL){

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

        output = as.numeric(table(input3$x))[2] / k #Get each participants total number of correct responses and divide by key

        temp = c(output, temp)
        temp2 = c(temp2, g)
        temp3 = c(temp3, i)

      }

    }

    name.list = data.frame(temp3)
    conditions = data.frame(temp2)

    output2 = data.frame(temp3, temp, temp2)
    colnames(output2)[1:3] = c("ID", "Proportion_Correct", "Condition")

    print(output2)

  }

}
