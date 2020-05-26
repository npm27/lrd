#' Proportion Correct
#'
#' This function computes the proportion of correct responses per participant. Proportions can either be separated by condition or collapsed across conditions. Note: This function returns mean values when used with non-binary data.
#'
#' @param x a vector containing participant scores
#' @return prints descriptive statistics. Output can be saved to a dataframe.
#' @export

prop.correct = function(x, group.by = NULL, id = z){
  
  a = is.null(group.by)
  
  if (a == TRUE) {
    
    input = data.frame(id, x)
    
    #Get participant level means
    tap1 = tapply(input$x, input$id, mean, na.rm = T) 
    tap1 = data.frame(tap1)
    
    colnames(tap1)[1] = "Value"
    
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