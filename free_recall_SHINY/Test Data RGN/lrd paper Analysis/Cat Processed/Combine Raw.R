#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$ids))

dat = dat[ , -1]

count(dat$ids) #A3VKMZEZL3CCPN

ids = rep("A21LYHAOO5P2CC", times = 20)

temp = subset(dat,
             dat$ids == "a21kldyy9rz2p")

x = unique(dat$items)

items = x[21:40]

scores = rep(0, times = 20)

#dat2 = data.frame(ids, items, scores)

#dat = rbind(dat, dat2)

write.csv(dat, file = "cat4_processed.csv", row.names = F)
