DF_test <- read.csv("data/free_data.csv")
DF_answer <- read.csv("data/answer_key_free2.csv")

colnames(DF_answer)[1] <- "Answer_Key" #Remove weird characters

source("R/prop_correct_free.R")
source("R/arrange_data.R")

DF_test <- subset(DF_test,
                  List_Type == "Cat_Recall_L1")

DF_long <- arrange_data(responses = DF_test$Response,
                        sep = " ",
                        id = DF_test$Username,
                        other = DF_test$Version,
                        other.names = "Version")

scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = TRUE,
                                    group.by = "Version")

source("R/crp.R")

crp_output <- crp(data = scored_output$DF_Scored,
                  position = "position",
                  answer = "Answer",
                  id = "Sub.ID",
                  key = DF_answer$Answer_Key,
                  scored = "Scored")

head(crp_output)