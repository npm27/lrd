DF_test <- read.csv("data/wide_data.csv")
DF_answer <- read.csv("data/answer_key_free.csv")

colnames(DF_answer)[1] <- "Answer_Key" #Remove weird characters

source("R/prop_correct_free.R")
source("R/arrange_data.R")

DF_long <- arrange_data(responses = DF_test$Response,
                        sep = ",",
                        id = DF_test$Sub.ID,
                        other = DF_test$Disease.Condition,
                        other.names = "Disease.Condition")

#be sure to include position!
scored_output <- prop_correct_free(responses = DF_long$response,
                                   key = DF_answer$Answer_Key,
                                   id = DF_long$Sub.ID,
                                   cutoff = 1,
                                   flag = TRUE,
                                   other = DF_long$position,
                                   other.names = "position",
                                   group.by = DF_long$Disease.Condition,
                                   group.by.names = "Disease.Condition")

head(scored_output$DF_Scored)
