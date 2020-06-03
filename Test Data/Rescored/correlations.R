dat1 = read.csv("max_buch processed.csv")
dat2 = read.csv("max_huff processed.csv")

length(unique(dat1$Sub.ID))
length(unique(dat2$Sub.ID))

dat2 = na.omit(dat2)

#item level
corr.test(dat1$manually_coded, dat1[ , 8:17]) #Maxwell Buchananan
corr.test(dat2$manually_coded, dat2[ , 8:17]) #Maxwell Huff

#participant level

library(psych)

c1 = cohen.kappa(dat1[ , c(6, 8:18)]) #M & B
c1
print(c1, all = T)

cohen.kappa(dat2[ , c(6, 8:18)]) #M & H

#cohen.k

####Descriptives, ANOVAS, t-tests####
library(ez)
library(reshape)

#set up for ANOVA
long.dat1 = melt(dat1[ , c(1, 6, 8:18)],
                id = c("Sub.ID"))

colnames(long.dat1)[2] = "score_type"
colnames(long.dat1)[3] = "score"

long.dat2 = melt(dat2[ , c(2, 6, 8:18)],
                 id = c("Sub.ID"))

colnames(long.dat2)[2] = "score_type"
colnames(long.dat2)[3] = "score"


long.dat1$score = long.dat1$score * 100
long.dat2$score = long.dat2$score * 100

#convert to subject level data
final.1 = cast(long.dat1, Sub.ID ~ score_type, mean)
final.2 = cast(long.dat2, Sub.ID ~ score_type, mean)

anova.1 = melt(final.1,
               id = "Sub.ID")

colnames(anova.1)[2] = "score"
colnames(anova.1)[3] = "score_type"

anova.2 = melt(final.2,
               id = "Sub.ID")

colnames(anova.2)[2] = "score"
colnames(anova.2)[3] = "score_type"

#Anova time!
model1 = ezANOVA(anova.1,
        dv = score,
        wid = Sub.ID,
        between = score_type,
        detailed = T,
        type = 3) #Non-SIG
model1

anovaLength = length(model1$ANOVA)
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##get t values for comparison
library(reshape)

tapply(anova.1$score,
       anova.1$score_type, mean)

#get 95%CI
tap1 = tapply(anova.1$score,
              anova.1$score_type, sd)

se1 = tap1 / sqrt(length(unique(anova.1$Sub.ID)))

se1 * 1.96

mb_posthoc = cast(long.dat1, Sub.ID ~ score_type, mean)

t.test(mb_posthoc$fifty, mb_posthoc$one_hundred, paired = F, p.adjust.methods = "bonferroni") #sig

##Now do the maxwell and huff data
model2 = ezANOVA(anova.2,
        dv = score,
        wid = Sub.ID,
        between = score_type,
        detailed = T,
        type = 3)
model2

anovaLength2 = length(model2$ANOVA)
model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

##now do the other dataset
tapply(anova.2$score,
       anova.2$score_type, mean)


tap2 = tapply(anova.2$score,
       anova.2$score_type, sd)

se2 = tap2 / sqrt(length(unique(anova.2$Sub.ID)))

se2 * 1.96

#get t-values
mh_posthoc = cast(long.dat2, Sub.ID ~ score_type, mean)

temp = t.test(mh_posthoc$fifty, mh_posthoc$sixty_five, paired = F, p.adjust.methods = "bonferroni")
temp
p4 = round(temp$p.value, 3)
t4 = temp$statistic
SEM4 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
SEM4
