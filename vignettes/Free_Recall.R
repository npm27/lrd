## ----setup, include = FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------------------------------------------------
library(lrd)
data("wide_data")
head(wide_data)
#?wide_data

data("answer_key_free")
head(answer_key_free)
#?answer_key_free

library(ggplot2)

## ------------------------------------------------------------------------------------------------------------------
DF_long <- arrange_data(data = wide_data,
      responses = "Response",
      sep = ",",
      id = "Sub.ID")
head(DF_long)

## ------------------------------------------------------------------------------------------------------------------
DF_long$response <- tolower(DF_long$response)
answer_key_free$Answer_Key <- tolower(answer_key_free$Answer_Key)

## ------------------------------------------------------------------------------------------------------------------
free_output <- prop_correct_free(data = DF_long,
                                 responses = "response",
                                 key = answer_key_free$Answer_Key,
                                 id = "Sub.ID",
                                 cutoff = 1,
                                 flag = TRUE,
                                 group.by = "Disease.Condition")


str(free_output)

## ------------------------------------------------------------------------------------------------------------------
#Overall
free_output$DF_Scored

#Participant
free_output$DF_Participant

#Group
free_output$DF_Group

## ------------------------------------------------------------------------------------------------------------------
serial_output <- serial_position(data = free_output$DF_Scored,
                                 key = answer_key_free$Answer_Key,
                                 position = "position",
                                 scored = "Scored",
                                 answer = "Answer",
                                 group.by = "Disease.Condition")

head(serial_output)

ggplot(serial_output, aes(Tested.Position, Proportion.Correct, color = Disease.Condition)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw() 

## ------------------------------------------------------------------------------------------------------------------
crp_output <- crp(data = free_output$DF_Scored,
                  key = answer_key_free$Answer_Key,
                  position = "position",
                  scored = "Scored",
                  answer = "Answer",
                  id = "Sub.ID")

head(crp_output)

crp_output$participant_lags <- as.numeric(as.character(crp_output$participant_lags))

ggplot(crp_output, aes(participant_lags, CRP, color = Disease.Condition)) +
  geom_line() +
  geom_point() +
  xlab("Lag Distance") +
  ylab("Conditional Response Probability") +
  theme_bw()

## ------------------------------------------------------------------------------------------------------------------
pfr_output <- pfr(data = free_output$DF_Scored,
                  key = answer_key_free$Answer_Key,
                  position = "position",
                  scored = "Scored",
                  answer = "Answer",
                  id = "Sub.ID",
                  group.by = "Disease.Condition")

head(pfr_output)

pfr_output$Tested.Position <- as.numeric(as.character(pfr_output$Tested.Position))

ggplot(pfr_output, aes(Tested.Position, pfr, color = Disease.Condition)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw()

