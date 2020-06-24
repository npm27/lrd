####Write a function to give each word its own dataframe row.####
##Essentially, doing a text to columns type thing then putting it into long format
dat = read.csv("sample data 2.csv")

spl = strsplit(dat$Response, ",")
spl = data.frame(dat$Sub.ID, V1 = sapply(spl, "[", 1), V2 = sapply(spl, "[", 2))

##the above would work great if all participants responded with the same number of words. But that's not going to happen

library(stringr)

strsplit(dat$Response[1], split = ",")

#Okay, could probably do a loop where it goes through the rows, splits things, and puts it all back together

#Let's try it!
#make an empty df for storage
x = data.frame()

for (i in dat$Response){
  
  for (j in 1:nrow(dat)){
  
    x2 = strsplit(i, split = ",")
    
    x2 = data.frame(x2)
    colnames(x2)[1] = "response"
  
  }
    
  x = rbind(x, x2)

}

x = as.data.frame(apply(x, 2, function(y)gsub('\\s+', '',y))) #remove spaces
