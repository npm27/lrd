####Score the test data####
##set up
library(lrd)

dat = read.csv("test_data.csv")
dat = dat[ , -1]

summary(dat)

#make sure everything is lowercase
dat$Response = tolower(dat$Response)

#replace response NAs with blanks
dat$Response[is.na(dat$Response)] <- ""

#compute percent match
output = percent_match(dat$Response, key = dat$key, id = dat$subID)

score_recall(output, set.cutoff = .75) #note that this stores the scored output in a .csv file