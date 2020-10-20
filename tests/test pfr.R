library(lrd)
data(free_data)
data("answer_key_free2")
DF_test <- free_data
DF_answer <- answer_key_free2

colnames(DF_answer)[1] <- "Answer_Key" #Remove weird characters

#source("R/prop_correct_free.R")
#source("R/arrange_data.R")

DF_test <- subset(DF_test,
                  List_Type == "Cat_Recall_L1")

DF_long <- arrange_data(data = DF_test,
                        responses = "Response",
                        sep = " ",
                        id = "Username")

scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = TRUE,
                                    group.by = "Version")

scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = TRUE)

#source("R/pfr.R")

pfr_output <- pfr(data = scored_output$DF_Scored,
                  position = "position",
                  answer = "Answer",
                  id = "Sub.ID",
                  key = DF_answer$Answer_Key,
                  scored = "Scored",
                  group.by = "Version")

head(pfr_output)
