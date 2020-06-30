####Write a basic percent match function that computes percentage of shared characters between any two strings####
#The other percent match function requires a dataframe, I also need one that works on a smaller scale.

library(vecsets)

percent_match.s = function(x, y){
  
  char.a1 = strsplit(x, "")
  char.b1 = strsplit(y, "")
  
  
  length(vintersect(char.a1[[1]], char.b1[[1]])) /max(length((char.a1[[1]])), length((char.b1[[1]])))
  
}

