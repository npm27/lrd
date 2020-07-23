filelist = list.files(pattern = ".txt")

get.csv = for (i in 1:length(filelist)){
  
  input = filelist[i]
  input2 = gsub(".txt", "", input)
  output = paste0(input2, ".csv")
  data = read.delim(input, header = TRUE)   
  write.table(data, file = output, sep = ",", col.names=TRUE, row.names=FALSE)
  
}