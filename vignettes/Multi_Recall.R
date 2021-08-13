## ----setup, include = FALSE---------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------
library(lrd)
data("multi_data")
head(multi_data)
#?multi_data

data("multi_answers")
head(multi_answers)
#?multi_answers

library(ggplot2)
library(reshape)

## -----------------------------------------------
DF_long <- arrange_data(data = multi_data,
      responses = "Response",
      sep = " ",
      id = "Sub.ID",
      repeated = "List.Number")
head(DF_long)

## -----------------------------------------------
multi_answers$position <- 1:nrow(multi_answers) #this column is only to reshape
answer_long <- melt(multi_answers,
                    measured = colnames(multi_answers),
                    id = "position")

#fix columns
colnames(answer_long) <- c("position", "List.ID", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers 
answer_long$List.ID <- gsub(pattern = "List", 
                            replacement = "", 
                            x = answer_long$List.ID)

head(answer_long)

## -----------------------------------------------
DF_long$response <- tolower(DF_long$response)
answer_long$Answer <- tolower(answer_long$Answer)
answer_long$Answer <- gsub(" ", "", answer_long$Answer)

head(DF_long)

head(answer_long)

## -----------------------------------------------
free_output <- prop_correct_multiple(data = DF_long,
                                 responses = "response",
                                 key = answer_long$Answer,
                                 key.trial = answer_long$List.ID,
                                 id = "Sub.ID",
                                 id.trial = "List.Number", 
                                 cutoff = 1,
                                 flag = TRUE)


str(free_output)

## -----------------------------------------------
#Overall
free_output$DF_Scored

#Participant
free_output$DF_Participant

#Group
#free_output$DF_Group

## -----------------------------------------------
serial_output <- serial_position_multiple(data = free_output$DF_Scored,
                                position = "position", 
                                answer = "Answer", 
                                key = answer_long$Answer,
                                key.trial = answer_long$List.ID,
                                scored = "Scored",
                                id.trial = "List.Number")

head(serial_output)

ggplot(serial_output, aes(Tested.Position, Proportion.Correct, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw() 

## -----------------------------------------------
crp_output <- crp_multiple(data = free_output$DF_Scored,
                  key = answer_long$Answer,
                  position = "position",
                  scored = "Scored",
                  answer = "Answer",
                  id = "Sub.ID", 
                  key.trial = answer_long$List.ID,
                  id.trial = "List.Number")

head(crp_output)

crp_output$participant_lags <- as.numeric(as.character(crp_output$participant_lags))

ggplot(crp_output, aes(participant_lags, CRP, color = List.Number)) +
  geom_line() +
  geom_point() +
  xlab("Lag Distance") +
  ylab("Conditional Response Probability") +
  theme_bw()

## -----------------------------------------------
pfr_output <- pfr_multiple(data = free_output$DF_Scored,
                  key = answer_long$Answer,
                  position = "position",
                  scored = "Scored",
                  answer = "Answer",
                  id = "Sub.ID",
                  key.trial = answer_long$List.ID,
                  id.trial = "List.Number")

head(pfr_output)

pfr_output$Tested.Position <- as.numeric(as.character(pfr_output$Tested.Position))

ggplot(pfr_output, aes(Tested.Position, pfr, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw()

