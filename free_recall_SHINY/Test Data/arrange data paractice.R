####Work on arrange data function####
library(readxl)
source("format data function.R")

dat = read_excel("Version A Guess Recall Data.xlsx", sheet = "Guess Data")
colnames(dat)[2:4] = c("List_Num", "List_Type", "Items_Guessed")

x = arrange.data(dat$Items_Guessed, sep = "  ", id = dat$Subj, other = dat[ , c(2:3)])

dat2 = read_excel("Version A Guess Recall Data.xlsx", sheet = "Int Recall Data")
colnames(dat2)[2:4] = c("List_Num", "List_Type", "Items_Recalled")

y = arrange.data(dat2$Items_Recalled, sep = "  ", id = dat2$Subj, other = dat[ , c(2:3)])

dat3 = read_excel("Version A Guess Recall Data.xlsx", sheet = "Final Recall Data")
colnames(dat3)[2] = "Items_Recalled"

z = arrange.data(dat3$Items_Recalled, sep = "  ", id = dat3$Subj)
