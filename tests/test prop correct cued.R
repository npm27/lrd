DF_test <- read.csv("data/cued_data.csv")

source("R/prop_correct_cued.R")

data = DF_test
responses = "response"
key = "key"
key.trial = "trial"
id = "id"
id.trial = "trial"
cutoff = 1
flag = TRUE
group.by = "condition"

#group.by and "" key answers
scored_output <- prop_correct_free(data = DF_test,
                                   responses = "response",
                                   key = "key",
                                   key.trial = "trial",
                                   id = "id",
                                   id.trial = "trial",
                                   cutoff = 1,
                                   flag = TRUE,
                                   group.by = "condition")

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

#group.by and $ key answers
scored_output <- prop_correct_free(data = DF_test,
                                   responses = "response",
                                   key = DF_test$key,
                                   key.trial = DF_test$trial,
                                   id = "id",
                                   id.trial = "trial",
                                   cutoff = 1,
                                   flag = TRUE,
                                   group.by = "condition")

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)

#need to test multiple
