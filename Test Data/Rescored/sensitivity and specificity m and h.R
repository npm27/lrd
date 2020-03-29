####setup####
dat = read.csv("max_huff processed.csv")

library(caret)

####sensitivity analysis####
#set up
data100 = factor(dat$one_hundred)
data95 = factor(dat$ninety_five)
data90 = factor(dat$ninety)
data85 = factor(dat$eightyfive)
data80 = factor(dat$eighty)
data75 = factor(dat$seventy_five)
data70 = factor(dat$seventy)
data65 = factor(dat$sixty_five)
data60 = factor(dat$sixty)
data55 = factor(dat$fifty_five)
data50 = factor(dat$fifty)

ref = factor(dat$manually_coded)

#start with 100
table(data100, ref)
sensitivity(data100, ref)
specificity(data100, ref)

#95
table(data95, ref)
sensitivity(data95, ref)
specificity(data95, ref)

#90
table(data90, ref)
sensitivity(data90, ref)
specificity(data90, ref)

#85
table(data85, ref)
sensitivity(data85, ref)
specificity(data85, ref)

#80
table(data80, ref)
sensitivity(data80, ref)
specificity(data80, ref)

#75
table(data75, ref)
sensitivity(data75, ref)
specificity(data75, ref)

#70
table(data70, ref)
sensitivity(data70, ref)
specificity(data70, ref)

#65
table(data65, ref)
sensitivity(data65, ref)
specificity(data65, ref)

#60
table(data60, ref)
sensitivity(data60, ref)
specificity(data60, ref)

#55
table(data55, ref)
sensitivity(data55, ref)
specificity(data55, ref)

#50
table(data50, ref)
sensitivity(data50, ref)
specificity(data50, ref)
