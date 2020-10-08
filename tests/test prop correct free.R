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
 # data = DF_long
 # responses = "response"
 # key = DF_answer$Answer_Key
 # id = "Sub.ID"
 # cutoff = 1
 # flag = TRUE
 # group.by = "Disease.Condition"

#Include group.by and Flag = True ----
scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = TRUE,
                                    group.by = "Disease.Condition")

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

#Include group.by and Flag = FALSE ----
scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = FALSE,
                                    group.by = "Disease.Condition")

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

#Remove group.by ----
scored_output <- prop_correct_free(data = DF_long,
                                   responses = "response",
                                   key = DF_answer$Answer_Key,
                                   id = "Sub.ID",
                                   cutoff = 1,
                                   flag = TRUE)
head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

##Try including disease condition as other

#Remove group.by and set flag to FALSE ----
scored_output <- prop_correct_free(data = DF_long,
                                   responses = "response",
                                   key = DF_answer$Answer_Key,
                                   id = "Sub.ID",
                                   cutoff = 1,
                                   flag = FALSE)

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

##Other ----
DF_long2 = DF_long
DF_long2$thing = c(rep(c("a", "b"), times = 25), "a")
table(DF_long2$Disease.Condition, DF_long2$thing)

scored_output <- prop_correct_free(data = DF_long2,
                                   responses = "response",
                                   key = DF_answer$Answer_Key,
                                   id = "Sub.ID",
                                   cutoff = 1,
                                   flag = TRUE)

head(scored_output$DF_Scored)

#Multiple group.by
scored_output <- prop_correct_free(data = DF_long2,
                                   responses = "response",
                                   key = DF_answer$Answer_Key,
                                   id = "Sub.ID",
                                   cutoff = 1,
                                   flag = FALSE,
                                   group.by = c("Disease.Condition", "thing"))

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

