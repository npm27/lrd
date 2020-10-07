DF_test <- read.csv("data/wide_data.csv")
DF_answer <- read.csv("data/answer_key_free.csv")

colnames(DF_answer)[1] <- "Answer_Key" #Remove weird characters

source("R/prop_correct_free.R")
source("R/arrange_data.R")

DF_long <- arrange_data(responses = DF_test$Response,
                        sep = ",",
                        id = DF_test$Sub.ID,
                        other = DF_test$Disease.Condition)

#change other name to real column name
colnames(DF_long)[4] <- "Disease.Condition"

####Test the thing####
#Include group.by and Flag = True
scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long$Sub.ID,
                                   cutoff = 1,
                                   flag = TRUE,
                                   group.by = DF_long$Disease.Condition) #Doesn't run:  Error in aggregate.data.frame(as.data.frame(x), ...): arguments must have same length 

head(scored_output$DF_Scored)

head(scored_output$DF_Participant) 

head(scored_output$DF_Group) 

#Include group.by and Flag = FALSE
scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long$Sub.ID,
                                   cutoff = 1,
                                   flag = FALSE,
                                   group.by = DF_long$Disease.Condition)

#Also doesn't run: Error in names(x) <- value :'names' attribute [3] must be the same length as the vector [2]

#Remove group.by
scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long$Sub.ID,
                                   cutoff = 1,
                                   flag = TRUE) #This runs! 
head(scored_output$DF_Scored)

head(scored_output$DF_Participant)  #I think grouping.variable column should be renamed Z

##Try including disease condition as other

#Remove group.by and set flag to FALSE
scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long$Sub.ID,
                                   cutoff = 1,
                                   flag = FALSE) #Doesn't run: Also doesn't run: Error in names(x) <- value :'names' attribute [3] must be the same length as the vector [2]
head(scored_output$DF_Scored)

##Other
DF_long2 = DF_long
DF_long2$thing = c(rep("a", times = 25), rep("b", times = 26))

scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long2$Sub.ID,
                                   cutoff = 1,
                                   flag = TRUE,
                                   other = DF_long2$thing) #Also run!

head(scored_output$DF_Scored) #colnames for other colum is NA

#what about multiple other columns?
DF_long2$thing2 = c(rep("c", times = 25), rep("d", times = 26))

scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long2$Sub.ID,
                                   cutoff = 1,
                                   flag = TRUE,
                                   other = DF_long2[ , 5:6])

head(scored_output$DF_Scored) #colnames for others are both NA... but this thing runs! 

head(scored_output$DF_Participant) #First other column is labeled as grouping variable. But z-score column is now correct. Is there something in the code forcing the third column to be named Grouping.Variable?