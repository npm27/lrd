library(lrd)
data(cued_data)
DF_test <- cued_data

source("R/prop_correct_cued.R")

#data = DF_test
#responses = "response"
#key = "key"
#key.trial = "trial"
#id = "id"
#id.trial = "trial"
#cutoff = 1
#flag = TRUE
#group.by = "condition"

scored_output <- prop_correct_cued(data = DF_test,
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

##No group by
scored_output <- prop_correct_cued(data = DF_test,
                                   responses = "response",
                                   key = "key",
                                   key.trial = "trial",
                                   id = "id",
                                   id.trial = "trial",
                                   cutoff = 1,
                                   flag = TRUE)

head(scored_output$DF_Scored)

head(scored_output$DF_Participant)

head(scored_output$DF_Group)
