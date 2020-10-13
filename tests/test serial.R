library(lrd)
data(free_data)
data("answer_key_free2")

DF_test <- free_data
DF_answer <- answer_key_free2

colnames(DF_answer)[1] <- "Answer_Key" #Remove weird characters

#source("R/prop_correct_free.R")
#source("R/arrange_data.R")

DF_test <- subset(DF_test,
                  List_Type == "Cat_Recall_L1")

DF_long <- arrange_data(data = DF_test,
                        responses = "Response",
                        sep = " ",
                        id = "Username")

scored_output <- prop_correct_free( data = DF_long,
                                    responses = "response",
                                    key = DF_answer$Answer_Key,
                                    id = "Sub.ID",
                                    cutoff = 1,
                                    flag = TRUE,
                                    group.by = "Version")

#key = DF_answer$Answer_Key
#data = scored_output$DF_Scored
#position = "position"
#scored = "Scored"
#answer = "Answer"
#group.by = "Version"

#source("R/serial_position.R")

serial_output <- serial_position(data = scored_output$DF_Scored,
                                 key = DF_answer$Answer_Key,
                                 position = "position",
                                 scored = "Scored",
                                 answer = "Answer",
                                 group.by = "Version")
head(serial_output)

tapply(serial_output$Proportion.Correct, serial_output$Tested.Position, mean)

##Quick plot
library(ggplot2)

ggplot(data = serial_output,
       aes(x = Tested.Position, y = Proportion.Correct, color = Version)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Proportion.Correct - SE, ymax = Proportion.Correct + SE),
                width = .2, position = position_dodge()) +
  xlab("Tested Position") +
  ylab("Proportion Correct") +
  theme_bw()
