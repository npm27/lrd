##Trying to figure out why prop_correct_multiple crashes when using lev above 0

long.dat = read.csv("multiple_long.csv")
key = read.csv("answer_long.csv")

library(lrd)
library(ggplot2)

####Start with 0####
##score it first
output0 = prop_correct_multiple(long.dat,
                               responses = "response",
                               key = key$Answer,
                               key.trial = key$List.ID,
                               id = "Sub.ID",
                               id.trial = "List.Number",
                               cutoff = 0)
output0$DF_Scored
output0$DF_Participant ##so far so good!

##plot time!
#serial position
sp0 = serial_position_multiple(data = output0$DF_Scored,
                         position = "position", 
                         answer = "Answer", 
                         key = key$Answer,
                         key.trial = key$List.ID,
                         scored = "Scored",
                         id.trial = "List.Number") #works!

#pfr
pf0 = pfr_multiple(output0$DF_Scored,
             position = "position",
             answer = "Answer",
             id = "Sub.ID",
             key = key$Answer,
             key.trial = key$List.ID,
             id.trial = "List.Number",
             scored = "Scored") #Also works!

#crp
cr0 = crp_multiple(data = output0$DF_Scored,
             key = key$Answer,
             position = "position",
             scored = "Scored",
             answer = "Answer",
             id = "Sub.ID", 
             key.trial = key$List.ID,
             id.trial = "List.Number") #and this one works!

####Now for a lev of 1####
##Expecting this one to break since this is what broke the shiny
output1 = prop_correct_multiple(long.dat,
                                responses = "response",
                                key = key$Answer,
                                key.trial = key$List.ID,
                                id = "Sub.ID",
                                id.trial = "List.Number",
                                cutoff = 1)
output1$DF_Scored
output1$DF_Participant ##Okay, working so far!

##plot time!
#serial position
sp1 = serial_position_multiple(data = output1$DF_Scored,
                         position = "position", 
                         answer = "Answer", 
                         key = key$Answer,
                         key.trial = key$List.ID,
                         scored = "Scored",
                         id.trial = "List.Number") #works!

#pfr
pf1 = pfr_multiple(output1$DF_Scored,
             position = "position",
             answer = "Answer",
             id = "Sub.ID",
             key = key$Answer,
             key.trial = key$List.ID,
             id.trial = "List.Number",
             scored = "Scored") #Also works!

#crp
cr1 = crp_multiple(data = output1$DF_Scored,
             key = key$Answer,
             position = "position",
             scored = "Scored",
             answer = "Answer",
             id = "Sub.ID", 
             key.trial = key$List.ID,
             id.trial = "List.Number") #and this one works!

##Okay, after swapping out the 1 with 2-5 everything still works great. Maybe its an issue w/ the shiny? or the ggplot code the shiny is using to display the figs?
#Let's try plotting?
##Serial position
#start w/ the 0 since this worked in shiny
ggplot(sp0, aes(Tested.Position, Proportion.Correct, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw() 

#now the 1
ggplot(sp1, aes(Tested.Position, Proportion.Correct, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw() 

#okay, both of these work

##PFR
#0
ggplot(pf0, aes(Tested.Position, pfr, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw()

#1
ggplot(pf1, aes(Tested.Position, pfr, color = List.ID)) +
  geom_line() +
  geom_point() +
  xlab("Tested Position") +
  ylab("Probability of First Response") +
  theme_bw()

#and both of these work...

##crp
ggplot(cr0, aes(participant_lags, CRP, color = List.Number)) +
  geom_line() +
  geom_point() +
  xlab("Lag Distance") +
  ylab("Conditional Response Probability") +
  theme_bw()

ggplot(cr1, aes(participant_lags, CRP, color = List.Number)) +
  geom_line() +
  geom_point() +
  xlab("Lag Distance") +
  ylab("Conditional Response Probability") +
  theme_bw()

##And great, both of these work. So it's either something w/ the data I was using or its shiny specific

##okay, just tested the shiny using this data and its also working fine

####Let's try testing the data the wouldn't work####
long.dat2 = read.csv("Anie 1A Long.csv")
key2 = read.csv("Anie 1A Key.csv")

##score with a lev of 0
output0B = prop_correct_multiple(long.dat2,
                                responses = "response",
                                key = key2$Key,
                                key.trial = key2$List,
                                id = "Sub.ID",
                                id.trial = "Key_Label",
                                cutoff = 0)
output0B$DF_Scored
output0B$DF_Participant ##Okay, this works

#Now try lev of 1
output1B = prop_correct_multiple(long.dat2,
                                 responses = "response",
                                 key = key2$Key,
                                 key.trial = key2$List,
                                 id = "Sub.ID",
                                 id.trial = "Key_Label",
                                 cutoff = 1)
output1B$DF_Scored
output1B$DF_Participant ##and this works

##Try the plotting
#LEV OF 0 FIRST
#serial position
sp0B = serial_position_multiple(data = output0B$DF_Scored,
                               position = "position", 
                               answer = "Answer", 
                               key = key2$Key,
                               key.trial = key2$List,
                               scored = "Scored",
                               id.trial = "Key_Label") #works!

#pfr
pf0B = pfr_multiple(output0B$DF_Scored,
                   position = "position",
                   answer = "Answer",
                   id = "Sub.ID",
                   key = key2$Key,
                   key.trial = key2$List,
                   id.trial = "Key_Label",
                   scored = "Scored") #Also works!

#crp
cr0B = crp_multiple(data = output0B$DF_Scored,
                   key = key2$Key,
                   position = "position",
                   scored = "Scored",
                   answer = "Answer",
                   id = "Sub.ID", 
                   key.trial = key2$List,
                   id.trial = "Key_Label") #and this one works!

##Now try lev of 1
#serial position
sp1B = serial_position_multiple(data = output1B$DF_Scored,
                                position = "position", 
                                answer = "Answer", 
                                key = key2$Key,
                                key.trial = key2$List,
                                scored = "Scored",
                                id.trial = "Key_Label") #works!

#pfr
pf1B = pfr_multiple(output1B$DF_Scored,
                    position = "position",
                    answer = "Answer",
                    id = "Sub.ID",
                    key = key2$Key,
                    key.trial = key2$List,
                    id.trial = "Key_Label",
                    scored = "Scored") #Also works!

#crp
cr1B = crp_multiple(data = output1B$DF_Scored,
                    key = key2$Key,
                    position = "position",
                    scored = "Scored",
                    answer = "Answer",
                    id = "Sub.ID", 
                    key.trial = key2$List,
                    id.trial = "Key_Label") ##This one throws an error message
