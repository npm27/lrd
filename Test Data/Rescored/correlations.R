dat1 = read.csv("max_buch processed.csv")
dat2 = read.csv("max_huff processed.csv")
dat3 = read.csv("cmh processed.csv")

dat2 = na.omit(dat2)

corr.test(dat1$manually_coded, dat1[ , 8:17]) #Maxwell Buchananan
corr.test(dat2$manually_coded, dat2[ , 8:17]) #Maxwell Huff
corr.test(dat2$manually_coded, dat2[ , 8:17]) #Cates Maxwell Huff
