####Start with the Version A data####
##Set directory
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Restudy First/Good Data/Version F - Restudy First") #get the correct directory

##First convert all the txt files to csv
filelist = list.files(pattern = ".txt")

for (i in 1:length(filelist)){
  
  input = filelist[i]
  input2 = gsub(".txt", "", input)
  output = paste0(input2, ".csv")
  data = read.delim(input, header = TRUE)   
  write.table(data, file = output, sep = ",", col.names=TRUE, row.names=FALSE)
  
  
}
