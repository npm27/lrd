####Check Human coded and set up for sensitivity/specificity####
dat = read.csv("Human Coded Arranged.csv")

tapply(dat$Score, dat$list_type, mean) ##These means line up correctly

##Make a dataset for each recalll type
Ad_hoc = subset(dat,
                dat$list_type == "Ad_hoc")
Cat = subset(dat,
             dat$list_type == "Cat")
Unrel = subset(dat,
               dat$list_type == "Unrel")

##Write them all to csv files
write.csv(Ad_hoc, file = "Ad_hoc_human.csv", row.names = F)
write.csv(Cat, file = "Cat_human.csv", row.names = F)
write.csv(Unrel, file = "Unrel_human.csv", row.names = F)
