## ----setup, include = FALSE--------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----------------------------------------------------------
library(lrd)
data("cued_recall_manuscript")
head(cued_recall_manuscript)
#?cued_recall_manuscript

## ----------------------------------------------------------
cued_recall_manuscript$Target <- tolower(cued_recall_manuscript$Target)
cued_recall_manuscript$Target <- tolower(cued_recall_manuscript$Target)

## ----------------------------------------------------------
cued_output <- prop_correct_cued(data = cued_recall_manuscript,
                                 responses = "Answer",
                                 key = "Target",
                                 key.trial = "Trial_num",
                                 id = "Sub.ID",
                                 id.trial = "Trial_num",
                                 cutoff = 1,
                                 flag = TRUE,
                                 group.by = NULL)

str(cued_output)

## ----------------------------------------------------------
#Overall
cued_output$DF_Scored

#Participant
cued_output$DF_Participant

