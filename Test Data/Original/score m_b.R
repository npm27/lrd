####set up####
dat = read.csv("Maxwell Buchanan 2019 part 2.csv")

colnames(dat)[3] = "Cue"
colnames(dat)[6] = "manually_coded"

library(lrd)

#make sure response and key are each lowercase
dat$Response = tolower(dat$Response)
dat$Key = tolower(dat$Key)

#Now compute the percent match
maxbuch_matched = percent_match(dat$Response, key = dat$Key, id = dat$Sub.ID)

####Score the data####
#going to use 10 cutoff points: 100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50

#first add percent match
dat$percent_match = maxbuch_matched$percent_match

#100%
score_recall(maxbuch_matched, set.cutoff = 1)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$one_hundred = output$scored

#95%
score_recall(maxbuch_matched, set.cutoff = .95)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$ninety_five = output$scored

#90%
score_recall(maxbuch_matched, set.cutoff = .90)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$ninety= output$scored

#85%
score_recall(maxbuch_matched, set.cutoff = .85)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$eightyfive= output$scored

#80%
score_recall(maxbuch_matched, set.cutoff = .80)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$eighty= output$scored

#75%
score_recall(maxbuch_matched, set.cutoff = .75)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$seventy_five = output$scored

#70%
score_recall(maxbuch_matched, set.cutoff = .70)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$seventy = output$scored

#65%
score_recall(maxbuch_matched, set.cutoff = .65)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$sixty_five = output$scored

#60%
score_recall(maxbuch_matched, set.cutoff = .60)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$sixty = output$scored

#55%
score_recall(maxbuch_matched, set.cutoff = .55)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$fifty_five = output$scored

#50%
score_recall(maxbuch_matched, set.cutoff = .50)

#now add the scored data to the original file
output = read.csv("output.csv") #load in the scored data

dat$fifty = output$scored

#write.csv(dat,
    #      file = "max_buch processed.csv", row.names = F)
