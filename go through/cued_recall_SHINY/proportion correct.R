#Goal here is to put out a dataframe containing everything
dat = read.csv("Delayed final data.csv")

#get the mean again Mean
tap1 = tapply(dat$Recall, list(dat$Subject, dat$Direction), mean)
tap1 = data.frame(tap1)

prop.correct = function(x, group.by = NULL, id = z){
  
  a = is.null(group.by)
  
  if (a == TRUE) {
    
    input = data.frame(id, x)
    
    #Get participant level means
    tap1 = tapply(input$x, input$id, mean, na.rm = T) 
    tap1 = data.frame(tap1)
    
    print(tap1)
    
  } 
  
  else if (a == FALSE) {
    
    input = data.frame(id, x, group.by)
    
    #Get participant level means grouped by condition
    tap2 = tapply(input$x, list(input$id, input$group.by), mean, na.rm = T) 
    tap2 = data.frame(tap2)

    print(tap2)
    
    }
  
}

prop.correct(dat$Recall, id = dat$Subject, group.by = dat$Direction)
