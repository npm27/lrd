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

prop.correct.f(df2$Scored, key = key, id = df2$id, flag = TRUE)