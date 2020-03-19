####Create the dummy data####
##set up
library(LexOPS)
library(stringr)

####generate some stim!####
##will need an equal number of cue and target items
#will want matches, close matches, and completely wrong items

stim = lexops %>%

  subset(PK.Brysbaert >= 0.7 & Length < 5) %>%

  split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%

  split_by(BG.SUBTLEX_UK, 0:0.003 ~ 0.009:0.013) %>%


  control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%

  generate(n = 10)

stim2 = lexops %>%

  subset(PK.Brysbaert >= 0.8 & Length < 6) %>%

  split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%

  split_by(BG.SUBTLEX_UK, 0:0.003 ~ 0.009:0.013) %>%


  control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%

  generate(n = 10)

##sample data will have five subjects
#subject 1 will have all correct responses,
#subject 2 will have minor typos on some responses
#subject 3 will have minor typos across most responses
#subject 4 will have some incorrect responses
#subject 5 will have mostly incorrect responses

##Subject 1
#put things together
key = c(stim$A2_B1, stim$A2_B2,
           stim2$A2_B1, stim2$A2_B2)

sub1_Response = key

#check for duplicates
length(unique(key))

subject1 = data.frame(key, sub1_Response)
subject1$subID = rep(1)

colnames(subject1)[2] = "Response"

##Subject 2
sub2_Response = key

sub2_Response = gsub("e", "", sub2_Response) #remove e's

subject2 = data.frame(key, sub2_Response)
subject2$subID = rep(2)

colnames(subject2)[2] = "Response"

##Subject 3
sub3_Response = key

sub3_Response = gsub("i", "", sub3_Response) #remove i's
sub3_Response = gsub("e", "a", sub3_Response) #replace e's with a's
sub3_Response = gsub("y", "yy", sub3_Response) #make y's double

subject3 = data.frame(key, sub3_Response)
subject3$subID = rep(3)

colnames(subject3)[2] = "Response"

##Subject 4
sub4_Response = key

#note that these may change. It seems like the words lexops generates are random
few_major = c("curb", "chowder", "swan", "oak", "kin", "jade", "blob", "waxy", "baked", "carpet",
              "fang", "bong", "hallway", "faucet", "maine", "twin", "grin", "tote", "dean", "vein",
              "hula", "gas", "milk", "slap", "paw", "skip", "digs", "pulp", "spa", "slam", "reins",
              "pot", "paint", "mint", "rink", "chess", "halo", "basin", "angus", "hay")

subject4 = data.frame(key, few_major)
subject4$subID = rep(4)

colnames(subject4)[2] = "Response"

##Subject 5
sub5_Response = key

many_major = c("crushed", NA, "swan", "trees", "kin", NA, "blob", "waxy", "baked", "carpet",
              "fang", "bong", "hallway", "fawcet", "maine", "twin", "smiling", "tote", "deen", NA,
              "hoop", "gas", "milk", "slap", "dog", "skip", "digs", "pulp", "spa", "slam", "reins",
              "pot", "paint", "peppermint", "rink", "checkers", "halo", "basin", "angus", "hey")

subject5 = data.frame(key, many_major)
subject5$subID = rep(5)

colnames(subject5)[2] = "Response"

##combine everything
combined = rbind(subject1, subject2, subject3, subject4, subject5)

##write the output to a .csv
#write.csv(combined, "test_data.csv", row.names = T)
