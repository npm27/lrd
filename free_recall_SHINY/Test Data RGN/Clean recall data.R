####Format data for scoring####

##Read in participant responses
dat = read.csv("Huff et al Data.csv")

##Lowercase things
dat$Response = tolower(dat$Response)

##Remove line breaks
dat$Response = gsub("<br />", ",", dat$Response)
dat$Response = gsub("  ,  , ", ",", dat$Response)

#Make words separated by space
dat$Response = gsub(" ,  , ", ",", dat$Response)
dat$Response = gsub(" ,, , ", ",", dat$Response)
dat$Response = gsub(",", " ", dat$Response)
dat$Response = gsub("  ", " ", dat$Response)

##Remove space from compound words only
dat$Response = gsub("baseball bat", "baseballbat", dat$Response)
dat$Response = gsub("rocking horse", "rockinghorse", dat$Response)
dat$Response = gsub("curling iron", "curlingiron", dat$Response)
dat$Response = gsub("hair dryer", "hairdryer", dat$Response)
dat$Response = gsub("hair dryar", "hairdryer", dat$Response)

####Write cleaned data to file####
#write.csv(dat, file = "Huff et al cleaned.csv", row.names = F)
