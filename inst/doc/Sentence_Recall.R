## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## --------------------------------------------------
library(lrd)
data("sentence_data")
head(sentence_data)
#?sentence_data

## --------------------------------------------------
sentence_data$Sentence <- tolower(sentence_data$Sentence)
sentence_data$Response <- tolower(sentence_data$Response)

## --------------------------------------------------
sentence_ouptut <- 
  prop_correct_sentence(data = sentence_data,
                        responses = "Response",
                        key = "Sentence",
                        key.trial = "Trial.ID",
                        id = "Sub.ID",
                        id.trial = "Trial.ID",
                        cutoff = 1,
                        flag = TRUE,
                        group.by = "Condition",
                        token.split = " ")

str(sentence_ouptut)

## --------------------------------------------------
#Overall
sentence_ouptut$DF_Scored

#Participant
sentence_ouptut$DF_Participant

#Groups
sentence_ouptut$DF_Group

