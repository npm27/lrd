#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$ids))

dat = dat[ , -1]

plyr::count(dat$ids)

# A3EHI7J2E5DSJK == 20 A248G7WMC2OS7F  == 20

#ids = rep("A3EHI7J2E5DSJK ", times = 20)

#temp = subset(dat,
           # dat$ids == "A3EHI7J2E5DSJK")

#x = unique(dat$items)

#items = x[41:60]

scores = rep(0, times = 20)

#at2 = data.frame(ids, items, scores)

#dat = rbind(dat, dat2)

ids = rep("A248G7WMC2OS7F ", times = 20)

temp = subset(dat,
              dat$ids == "A248G7WMC2OS7F")

x = unique(dat$items)

items = x[21:40]

dat2 = data.frame(ids, items, scores)

dat = rbind(dat, dat2)

write.csv(dat, file = "Unrel3_processed.csv", row.names = F)
