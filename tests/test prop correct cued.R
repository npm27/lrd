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

scored_output2 <- prop_correct_free(responses = DF_test$response,
                                   key = DF_test$key,
                                   key.trial = DF_test$trial,
                                   id = DF_test$id,
                                   id.trial = DF_test$trial,
                                   cutoff = 2,
                                   flag = FALSE,
                                   group.by = DF_test$condition,
                                   group.by.names = "condition",
                                   other = c(rep("stuff", nrow(DF_test))),
                                   other.names = "fake_column")

head(scored_output2$DF_Scored)

head(scored_output2$DF_Participant)

head(scored_output2$DF_Group)
