describe = function(x, group.by = y, id = z){
  
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

####test the function and see if it does what it's supposed to####
app.test = read.csv("app test 2.csv")

describe(scored$scored, group.by = scored$Condition1, id = scored$id)

#Test with another dataset:
test = read.csv("Delayed final data.csv")

describe(test$Jol, group.by = test$Direction, id = test$Subject)
describe(test$Recall, group.by = test$Direction, id = test$Subject)
