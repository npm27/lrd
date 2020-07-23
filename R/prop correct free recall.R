#' Proportion Correct Free Recall
#'
#' This function computes the proportion of correct responses per participant. Proportions can either be separated by condition or collapsed across conditions. Note: This function returns mean values when used with non-binary data.
#'
#' @param x a vector containing participant scores
#' @param key.length a numeric value denoting number of items in scoring key
#' @param id a vector containing participant ID numbers
#' @return prints descriptive statistics. Output can be saved to a dataframe.
#' @export

prop.correct.f = function(x, key.length = y, id = z){
   
    dat = data.frame(x, id)
    k = key.length
    print(dat)
    
  
    prop_correct = c()
    sub.id = c()
  
    for (i in unique(dat$id)){
      
      temp = subset(dat,
                  dat$id == i)
    
      x = sum(temp$x)
    
      y = x/k
    
      #print(y)
    
      sub.id = c(sub.id, i)
    
      prop_correct = c(prop_correct, y)
    
    }
  
    output = data.frame(sub.id, prop_correct)
    output$z = scale(output$prop_correct)
    
    print(output)
    
    return(output)
    
}

 