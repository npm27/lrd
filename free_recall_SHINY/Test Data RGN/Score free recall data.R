####Score the Huff et al. (2018) data####
library(RecordLinkage)

##load in cleaned data
dat = read.csv("Huff et al cleaned.csv")

##load libraries
library(readxl)
library(lrd)

##Read in answer keys
ver.A = read_excel("Huff et al Key.xlsx", sheet = "Version A")
ver.B = read_excel("Huff et al Key.xlsx", sheet = "Version B")
ver.C = read_excel("Huff et al Key.xlsx", sheet = "Version C")
ver.D = read_excel("Huff et al Key.xlsx", sheet = "Version D")
ver.E = read_excel("Huff et al Key.xlsx", sheet = "Version E")
ver.F = read_excel("Huff et al Key.xlsx", sheet = "Version F")

##Remove spaces from answer key
ver.A = apply(ver.A, 2, function(x)gsub(' ', '', x))
ver.A = as.data.frame(ver.A)

ver.B = apply(ver.B, 2, function(x)gsub(' ', '', x))
ver.B = as.data.frame(ver.B)

ver.C = apply(ver.C, 2, function(x)gsub(' ', '', x))
ver.C = as.data.frame(ver.C)

ver.D = apply(ver.D, 2, function(x)gsub(' ', '', x))
ver.D = as.data.frame(ver.D)

ver.E = apply(ver.E, 2, function(x)gsub(' ', '', x))
ver.E = as.data.frame(ver.E)

ver.F = apply(ver.F, 2, function(x)gsub(' ', '', x))
ver.F = as.data.frame(ver.F)

##Lowercase everything
ver.A = data.frame(lapply(ver.A, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.B = data.frame(lapply(ver.B, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.C = data.frame(lapply(ver.C, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.D = data.frame(lapply(ver.D, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))


ver.E = data.frame(lapply(ver.E, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.F = data.frame(lapply(ver.F, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

##subset responses by version
datA = subset(dat,
              dat$Version == "A")
datB = subset(dat,
              dat$Version == "B")
datC = subset(dat,
              dat$Version == "C")
datD = subset(dat,
              dat$Version == "D")
datE = subset(dat,
              dat$Version == "E")
datF = subset(dat,
              dat$Version == "F")

##now need to subset out each list type
##A
A1 = subset(datA,
            datA$List_Type == "Cat_Recall_L1")
A2 = subset(datA,
            datA$List_Type == "AdHoc_Recall_L2")
A3 = subset(datA,
            datA$List_Type == "Unrel_Recall_L3")
A4 = subset(datA,
            datA$List_Type == "Unrel_Recall_L4")
A5 = subset(datA,
            datA$List_Type == "Cat_Recall_L5")
A6 = subset(datA,
            datA$List_Type == "AdHoc_Recall_L6")

##B
B1 = subset(datB,
            datB$List_Type == "Unrel_Recall_L1")
B2 = subset(datB,
            datB$List_Type == "AdHoc_Recall_L2")
B3 = subset(datB,
            datB$List_Type == "AdHoc_Recall_L3")
B4 = subset(datB,
            datB$List_Type == "Cat_Recall_L4")
B5 = subset(datB,
            datB$List_Type == "Cat_Recall_L5")
B6 = subset(datB,
            datB$List_Type == "Unrel_Recall_L6")

##C
C1 = subset(datC,
            datC$List_Type == "Cat_Recall_L1")
C2 = subset(datC,
            datC$List_Type == "AdHoc_Recall_L2")
C3 = subset(datC,
            datC$List_Type == "Unrel_Recall_L3")
C4 = subset(datC,
            datC$List_Type == "Unrel_Recall_L4")
C5 = subset(datC,
            datC$List_Type == "Cat_Recall_L5")
C6 = subset(datC,
            datC$List_Type == "AdHoc_Recall_L6")

##D
D1 = subset(datD,
            datD$List_Type == "Unrel_Recall_L1")
D2 = subset(datD,
            datD$List_Type == "AdHoc_Recall_L2")
D3 = subset(datD,
            datD$List_Type == "AdHoc_Recall_L3")
D4 = subset(datD,
            datD$List_Type == "Cat_Recall_L4")
D5 = subset(datD,
            datD$List_Type == "Cat_Recall_L5")
D6 = subset(datD,
            datD$List_Type == "Unrel_Recall_L6")

##E
E1 = subset(datE,
            datE$List_Type == "Cat_Recall_L1")
E2 = subset(datE,
            datE$List_Type == "AdHoc_Recall_L2")
E3 = subset(datE,
            datE$List_Type == "Unrel_Recall_L3")
E4 = subset(datE,
            datE$List_Type == "Unrel_Recall_L4")
E5 = subset(datE,
            datE$List_Type == "Cat_Recall_L5")
E6 = subset(datE,
            datE$List_Type == "AdHoc_Recall_L6")

##F
F1 = subset(datF,
            datF$List_Type == "Unrel_Recall_L1")
F2 = subset(datF,
            datF$List_Type == "AdHoc_Recall_L2")
F3 = subset(datF,
            datF$List_Type == "AdHoc_Recall_L3")
F4 = subset(datF,
            datF$List_Type == "Cat_Recall_L4")
F5 = subset(datF,
            datF$List_Type == "Cat_Recall_L5")
F6 = subset(datF,
            datF$List_Type == "Unrel_Recall_L6")

##Remove blank responses
A4 = A4[-15, ]

##Remove question marks
E6 = E6[-112, ]

####Get data in long format####
##Version A
A1 = arrange.data(A1$Response, sep = " ", id = A1$Username, other = A1[ , c(4:5)])
A2 = arrange.data(A2$Response, sep = " ", id = A2$Username, other = A2[ , c(4:5)])
A3 = arrange.data(A3$Response, sep = " ", id = A3$Username, other = A3[ , c(4:5)])
A4 = arrange.data(A4$Response, sep = " ", id = A4$Username, other = A4[ , c(4:5)])
A5 = arrange.data(A5$Response, sep = " ", id = A5$Username, other = A5[ , c(4:5)])
A6 = arrange.data(A6$Response, sep = " ", id = A6$Username, other = A6[ , c(4:5)])

##Version B
B1 = arrange.data(B1$Response, sep = " ", id = B1$Username, other = B1[ , c(4:5)])
B2 = arrange.data(B2$Response, sep = " ", id = B2$Username, other = B2[ , c(4:5)])
B3 = arrange.data(B3$Response, sep = " ", id = B3$Username, other = B3[ , c(4:5)])
B4 = arrange.data(B4$Response, sep = " ", id = B4$Username, other = B4[ , c(4:5)])
B5 = arrange.data(B5$Response, sep = " ", id = B5$Username, other = B5[ , c(4:5)])
B6 = arrange.data(B6$Response, sep = " ", id = B6$Username, other = B6[ , c(4:5)])

##Version C
C1 = arrange.data(C1$Response, sep = " ", id = C1$Username, other = C1[ , c(4:5)])
C2 = arrange.data(C2$Response, sep = " ", id = C2$Username, other = C2[ , c(4:5)])
C3 = arrange.data(C3$Response, sep = " ", id = C3$Username, other = C3[ , c(4:5)])
C4 = arrange.data(C4$Response, sep = " ", id = C4$Username, other = C4[ , c(4:5)])
C5 = arrange.data(C5$Response, sep = " ", id = C5$Username, other = C5[ , c(4:5)])
C6 = arrange.data(C6$Response, sep = " ", id = C6$Username, other = C6[ , c(4:5)])

##Version D
D1 = arrange.data(D1$Response, sep = " ", id = D1$Username, other = D1[ , c(4:5)])
D2 = arrange.data(D2$Response, sep = " ", id = D2$Username, other = D2[ , c(4:5)])
D3 = arrange.data(D3$Response, sep = " ", id = D3$Username, other = D3[ , c(4:5)])
D4 = arrange.data(D4$Response, sep = " ", id = D4$Username, other = D4[ , c(4:5)])
D5 = arrange.data(D5$Response, sep = " ", id = D5$Username, other = D5[ , c(4:5)])
D6 = arrange.data(D6$Response, sep = " ", id = D6$Username, other = D6[ , c(4:5)])

##Version E
E1 = arrange.data(E1$Response, sep = " ", id = E1$Username, other = E1[ , c(4:5)])
E2 = arrange.data(E2$Response, sep = " ", id = E2$Username, other = E2[ , c(4:5)])
E3 = arrange.data(E3$Response, sep = " ", id = E3$Username, other = E3[ , c(4:5)])
E4 = arrange.data(E4$Response, sep = " ", id = E4$Username, other = E4[ , c(4:5)])
E5 = arrange.data(E5$Response, sep = " ", id = E5$Username, other = E5[ , c(4:5)])
E6 = arrange.data(E6$Response, sep = " ", id = E6$Username, other = E6[ , c(4:5)])

##Version E
F1 = arrange.data(F1$Response, sep = " ", id = F1$Username, other = F1[ , c(4:5)])
F2 = arrange.data(F2$Response, sep = " ", id = F2$Username, other = F2[ , c(4:5)])
F3 = arrange.data(F3$Response, sep = " ", id = F3$Username, other = F3[ , c(4:5)])
F4 = arrange.data(F4$Response, sep = " ", id = F4$Username, other = F4[ , c(4:5)])
F5 = arrange.data(F5$Response, sep = " ", id = F5$Username, other = F5[ , c(4:5)])
F6 = arrange.data(F6$Response, sep = " ", id = F6$Username, other = F6[ , c(4:5)])

####Okay now we can score the stuff####
##First batch of scoring will use a cutoff of 2

##Start with version A
scoredA1 = score.recall.f(A1$response, key = ver.A$Cat_Recall_L1, id = A1$Sub.ID, cutoff = 4)
scoredA2 = score.recall.f(A2$response, key = ver.A$AdHoc_Recall_L2, id = A2$Sub.ID, cutoff = 4)
scoredA3 = score.recall.f(A3$response, key = ver.A$Unrel_Recall_L3, id = A3$Sub.ID, cutoff = 4)
scoredA4 = score.recall.f(A4$response, key = ver.A$Unrel_Recall_L4, id = A4$Sub.ID, cutoff = 4)
scoredA5 = score.recall.f(A5$response, key = ver.A$Cat_Recall_L5, id = A5$Sub.ID, cutoff = 4)
scoredA6 = score.recall.f(A6$response, key = ver.A$AdHoc_Recall_L6, id = A6$Sub.ID, cutoff = 4)

scoredA1$list = rep("Cat")
scoredA2$list = rep("AdHoc")
scoredA3$list = rep("Unrel")
scoredA4$list = rep("Unrel")
scoredA5$list = rep("Cat")
scoredA6$list = rep("AdHoc")

##Now version B
scoredB1 = score.recall.f(B1$response, key = ver.D$Unrel_Recall_L1, id = B1$Sub.ID, cutoff = 4) ##ver.B key didn't work for some reason. Using D since it contains the same items
scoredB2 = score.recall.f(B2$response, key = ver.B$AdHoc_Recall_L2, id = B2$Sub.ID, cutoff = 4)
scoredB3 = score.recall.f(B3$response, key = ver.B$AdHoc_Recall_L3, id = B3$Sub.ID, cutoff = 4)
scoredB4 = score.recall.f(B4$response, key = ver.B$Cat_Recall_L4, id = B4$Sub.ID, cutoff = 4)
scoredB5 = score.recall.f(B5$response, key = ver.B$Cat_Recall_L5, id = B5$Sub.ID, cutoff = 4)
scoredB6 = score.recall.f(B6$response, key = ver.B$Unrel_Recall_L6, id = B6$Sub.ID, cutoff = 4)

scoredB1$list = rep("Unrel")
scoredB2$list = rep("AdHoc")
scoredB3$list = rep("AdHoc")
scoredB4$list = rep("Cat")
scoredB5$list = rep("Cat")
scoredB6$list = rep("Unrel")

##Now version C
scoredC1 = score.recall.f(C1$response, key = ver.C$Cat_Recall_L1, id = C1$Sub.ID, cutoff = 4)
scoredC2 = score.recall.f(C2$response, key = ver.C$AdHoc_Recall_L2, id = C2$Sub.ID, cutoff = 4)
scoredC3 = score.recall.f(C3$response, key = ver.C$Unrel_Recall_L3, id = C3$Sub.ID, cutoff = 4)
scoredC4 = score.recall.f(C4$response, key = ver.C$Unrel_Recall_L4, id = C4$Sub.ID, cutoff = 4)
scoredC5 = score.recall.f(C5$response, key = ver.C$Cat_Recall_L5, id = C5$Sub.ID, cutoff = 4)
scoredC6 = score.recall.f(C6$response, key = ver.C$AdHoc_Recall_L6, id = C6$Sub.ID, cutoff = 4)

scoredC1$list = rep("Cat")
scoredC2$list = rep("AdHoc")
scoredC3$list = rep("Unrel")
scoredC4$list = rep("Unrel")
scoredC5$list = rep("Cat")
scoredC6$list = rep("AdHoc")

##Version D
scoredD1 = score.recall.f(D1$response, key = ver.D$Unrel_Recall_L1, id = D1$Sub.ID, cutoff = 4)
scoredD2 = score.recall.f(D2$response, key = ver.D$AdHoc_Recall_L2, id = D2$Sub.ID, cutoff = 4)
scoredD3 = score.recall.f(D3$response, key = ver.D$AdHoc_Recall_L3, id = D3$Sub.ID, cutoff = 4)
scoredD4 = score.recall.f(D4$response, key = ver.D$Cat_Recall_L4, id = D4$Sub.ID, cutoff = 4)
scoredD5 = score.recall.f(D5$response, key = ver.D$Cat_Recall_L5, id = D5$Sub.ID, cutoff = 4)
scoredD6 = score.recall.f(D6$response, key = ver.D$Unrel_Recall_L6, id = D6$Sub.ID, cutoff = 4)

scoredD1$list = rep("Unrel")
scoredD2$list = rep("AdHoc")
scoredD3$list = rep("AdHoc")
scoredD4$list = rep("Cat")
scoredD5$list = rep("Cat")
scoredD6$list = rep("Unrel")

##Version E
scoredE1 = score.recall.f(E1$response, key = ver.E$Cat_Recall_L1, id = E1$Sub.ID, cutoff = 4)
scoredE2 = score.recall.f(E2$response, key = ver.E$AdHoc_Recall_L2, id = E2$Sub.ID, cutoff = 4)
scoredE3 = score.recall.f(E3$response, key = ver.E$Unrel_Recall_L3, id = E3$Sub.ID, cutoff = 4)
scoredE4 = score.recall.f(E4$response, key = ver.E$Unrel_Recall_L4, id = E4$Sub.ID, cutoff = 4)
scoredE5 = score.recall.f(E5$response, key = ver.E$Cat_Recall_L5, id = E5$Sub.ID, cutoff = 4)
scoredE6 = score.recall.f(E6$response, key = ver.C$AdHoc_Recall_L6, id = E6$Sub.ID, cutoff = 4) ##E didn't work here but c did (they should contain the same items...)

scoredE1$list = rep("Cat")
scoredE2$list = rep("AdHoc")
scoredE3$list = rep("Unrel")
scoredE4$list = rep("Unrel")
scoredE5$list = rep("Cat")
scoredE6$list = rep("AdHoc")

##Now version F
scoredF1 = score.recall.f(F1$response, key = ver.D$Unrel_Recall_L1, id = F1$Sub.ID, cutoff = 4) #f didn't work so used D
scoredF2 = score.recall.f(F2$response, key = ver.F$AdHoc_Recall_L2, id = F2$Sub.ID, cutoff = 4)
scoredF3 = score.recall.f(F3$response, key = ver.F$AdHoc_Recall_L3, id = F3$Sub.ID, cutoff = 4)
scoredF4 = score.recall.f(F4$response, key = ver.F$Cat_Recall_L4, id = F4$Sub.ID, cutoff = 4)
scoredF5 = score.recall.f(F5$response, key = ver.F$Cat_Recall_L5, id = F5$Sub.ID, cutoff = 4)
scoredF6 = score.recall.f(F6$response, key = ver.F$Unrel_Recall_L6, id = F6$Sub.ID, cutoff = 4)

scoredF1$list = rep("Unrel")
scoredF2$list = rep("AdHoc")
scoredF3$list = rep("AdHoc")
scoredF4$list = rep("Cat")
scoredF5$list = rep("Cat")
scoredF6$list = rep("Unrel")

####Okay, now to combine scored output####
Unrel = rbind(scoredA3, scoredA4, scoredB1, scoredB6, scoredC3, scoredC4,
              scoredD1, scoredD6, scoredE3, scoredE4, scoredF1, scoredF6)
unique(Unrel$list)

Ad_hoc = rbind(scoredA2, scoredA6, scoredB2, scoredB3, scoredC2, scoredC6,
               scoredD2, scoredD3, scoredE2, scoredE6, scoredF2, scoredF3)
unique(Ad_hoc$list)

Cat = rbind(scoredA1, scoredA5, scoredB4, scoredB5, scoredC1, scoredC5,
            scoredD4, scoredD5, scoredE1, scoredE5, scoredF4, scoredF5)
unique(Cat$list)

##Remove cells with spaces
Cat = Cat[-c(1614, 1644), ]

Unrel = Unrel[-c(1092, 1853, 1854, 1415, 115), ]

Ad_hoc = Ad_hoc[-c(436:438, 2523, 2539, 2529, 2537), ]
Ad_hoc = Ad_hoc[-2531, ]

####Compute Proportion Correct####
key1 = rbind(ver.A, ver.C)

length(key1$Cat_Recall_L1)

Cat.proportions = prop.correct.f(Cat$Scored, key.length = 40, id = Cat$id)
Unrel.Proportions = prop.correct.f(Unrel$Scored, key.length = 40, id = Unrel$id)
Ad_hoc.Proportions = prop.correct.f(Ad_hoc$Scored, key.length = 40, id = Ad_hoc$id)

##Get means
mean(Cat.proportions$prop_correct, na.rm = T)
mean(Unrel.Proportions$prop_correct, na.rm = T)
mean(Ad_hoc.Proportions$prop_correct, na.rm = T)

####Combine datasets for senstivity/specificity####
A.Scored = rbind(scoredA1, scoredA2, scoredA3, scoredA4, scoredA5, scoredA6)
B.Scored = rbind(scoredB1, scoredB2, scoredB3, scoredB4, scoredB5, scoredB6)
C.Scored = rbind(scoredC1, scoredC2, scoredC3, scoredC4, scoredC5, scoredC6)
D.Scored = rbind(scoredD1, scoredD2, scoredD3, scoredD4, scoredD5, scoredD6)
E.Scored = rbind(scoredE1, scoredE2, scoredE3, scoredE4, scoredE5, scoredE6)
F.Scored = rbind(scoredF1, scoredF2, scoredA3, scoredF4, scoredF5, scoredF6)

Final = rbind(A.Scored, B.Scored, C.Scored, D.Scored, E.Scored, F.Scored)

Adhoc = subset(Final,
               Final$list == "AdHoc")
Unrel2 = subset(Final,
               Final$list == "Unrel")
Cat2 = subset(Final,
                Final$list == "Cat")

#write.csv(Adhoc, file = "Adhoc_4.csv", row.names = F)
#write.csv(Unrel2, file = "Unrel_4.csv", row.names = F)
#write.csv(Cat2, file = "Cat_4.csv", row.names = F)
