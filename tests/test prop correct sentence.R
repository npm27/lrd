DF_test <- read.csv("data/sentence_data.csv")

#me testing as I build ----
data = DF_test
responses = "Response"
key = "Sentence"
key.trial = "Trial.ID"
id = "Sub.ID"
id.trial = "Trial.ID"
cutoff = 1
flag = TRUE
group.by = "Condition"
token.split = " "

source("R/prop_correct_sentence.R")

scored_output <- prop_correct_sentence(
  data = DF_test,
  responses = "Response",
  key = "Sentence",
  key.trial = "Trial.ID",
  id = "Sub.ID",
  id.trial = "Trial.ID",
  cutoff = 1,
  flag = TRUE,
  group.by = "Condition",
  token.split = " "
)

head(scored_output$DF_Scored)

scored_output$DF_Participant

scored_output$DF_Group
