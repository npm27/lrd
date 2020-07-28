#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$ids))

dat = dat[ , -1]

count(dat$ids) #A3VKMZEZL3CCPN

ids = rep("A3VKMZEZL3CCPN", times = 20)

items = c("tornado", "hurricane", "windmill", "spiral", "propeller", "gyroscope", "yo-yo", "rotisserie", "bottle", "gear", "sphere", "record", "top", "orb", "dreidel", "satellite", "bycicle", "pinwheel", "spool", "roulette")

scores = rep(0, times = 20)

dat2 = data.frame(ids, items, scores)

dat = rbind(dat, dat2)

write.csv(dat, file = "adhoc0_processed.csv", row.names = F)
