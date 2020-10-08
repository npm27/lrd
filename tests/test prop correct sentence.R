DF_test <- read.csv("data/sentence_data.csv")

#me testing as I build ----
# responses = DF_test$Response
# key = DF_test$Sentence
# key.trial = DF_test$Trial.ID
# id = DF_test$Sub.ID
# id.trial = DF_test$Trial.ID
# cutoff = 1
# flag = TRUE
# group.by = DF_test$Condition
# group.by.names = "condition"
# other = c(rep("stuff", nrow(DF_test)))
# other.names = "fake_column"
# token.split = " "

source("R/prop_correct_sentence.R")

scored_output <- prop_correct_sentence(
  responses = DF_test$Response,
  key = DF_test$Sentence,
  key.trial = DF_test$Trial.ID,
  id = DF_test$Sub.ID,
  id.trial = DF_test$Trial.ID,
  cutoff = 1,
  flag = TRUE,
  group.by = DF_test$Condition,
  group.by.names = "condition",
  other = c(rep("stuff", nrow(DF_test))),
  other.names = "fake_column",
  token.split = " "
)

scored_output$DF_Scored

scored_output$DF_Participant

scored_output$DF_Group
