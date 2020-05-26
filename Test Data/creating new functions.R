####Figure out some way to get summary statistics from scored data####
##set up
##load libraries
library(lrd)
library(devtools)
library(reshape)

##Load data
dat = read.csv("Rescored/max_huff processed.csv")
dat2 = read.csv("Original/Maxwell_Huff output.csv")

dat = dat[ , -c(8:12, 14:18)]

##How would I go about getting summary statistics for each subject?
##Percent match
tapply(dat$percent_match, dat$Sub.ID, mean) #This would give mean percent match for each subject

tapply(dat$percent_match, dat$Cue, mean) #and this would give it to you based on cue word

##Mean percent recall
#tap1 = tapply(dat$seventy_five, dat$Sub.ID, mean)
#tap2 = data.frame(tap1)

#names = rownames(tap2)
#names2 = data.frame(names)

#output = cbind(names2, tap2)

#colnames(output)[1] = "Sub ID"
#colnames(output)[2] = "Mean.percent.Recall"

##Okay, that gets the mean, but what about the other stats?
#Goal here is to put out a dataframe containing everything
#get the mean again Mean
tap1 = tapply(dat$seventy_five, dat$Sub.ID, mean)

#Standard deviation
tap2 = tapply(dat$seventy_five, dat$Sub.ID, sd)

#SE
tap3 = tap2 / sqrt(length(unique(dat$Cue)))

#CI
tap4 = tap3 * 1.96

#UPPER
tap5 = tap1 + tap4

#LOWER
tap6 = tap1 - tap4

##Stick it all together in a dataframe
#formatting
temp = data.frame(tap1)

Sub.ID = rownames(temp)
Sub.ID = data.frame(Sub.ID)

#Put everything together
out = cbind(Sub.ID, temp)
colnames(out)[2] = "Mean"

temp = data.frame(tap2)

out = cbind(out, temp)
colnames(out)[3] = "SD"

temp = data.frame(tap3)

out = cbind(out, temp)
colnames(out)[4] = "SE"

temp = data.frame(tap4)

out = cbind(out, temp)
colnames(out)[5] = "95% CI"

temp = data.frame(tap5)

out = cbind(out, temp)
colnames(out)[6] = "Upper Limit"

temp = data.frame(tap6)

out = cbind(out, temp)
colnames(out)[7] = "Lower Limit"

##The above works -- but what about if you wanted to separate out by a condition column?
summary(dat) #Need to find a dataset with multiple condition columns

#Get the app test data
setwd('..')

getwd()

app.test = read.csv("lrdSHINY/Delayed Final Data.csv")

#Need to score test data
ID = app.test$Subject
key = tolower(app.test$Target)
Response = tolower(app.test$Response)

matched = percent_match(Response, key = key, id = ID)
score_recall(matched, set.cutoff = .75)

scored = read.csv("output.csv")

#Now add in the condition columns
scored$Condition1 = app.test$Condition
scored$Condition2 = app.test$Conditon.2

####Start getting descriptives again####
#First need ID number
names = row.names(tap1)

#Get the subject level mean
tap1 = tapply(scored$scored, list(scored$id, scored$Condition1), mean, na.rm = T)
tap1 = data.frame(tap1)

temp = cbind(names, tap1)

#now get condition level
m1 = apply(temp[2:length(temp)], 2, mean) #mean
sd1 = apply(temp[2:length(temp)], 2, sd)#sd
se = sd1 / sqrt(length(unique(temp$names))) #se
ci = se * 1.96
upper = m1 + ci
lower = m1 - ci

##Now slap everything onto the bottom of the dataframe
#Get row names and column labels
names = data.frame(c("Mean", "SD", "SE", "95% CI", "Upper", "Lower"))

labels = colnames(temp[ , -1])

m1 = t(data.frame(m1))
sd1 = t(data.frame(sd1))
se = t(data.frame(se))
ci = t(data.frame(ci))
upper = t(data.frame(upper))
lower = t(data.frame(lower))

temp2 = rbind(m1, sd1, se, ci, upper, lower)
temp2 = cbind(names, temp2)
colnames(temp2)[1] = "names"

condition.means = rbind(temp, temp2)
