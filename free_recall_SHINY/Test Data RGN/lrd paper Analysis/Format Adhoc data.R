####Format the scored data to include answer key items not given####
####Score the Huff et al. (2018) data####
#Libraries
library(RecordLinkage)
library(readxl)
library(lrd)
library(sjmisc)

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

####Let's start with Adhoc 0
dat = read.csv("Adhoc/Adhoc_4.csv")

##Get Version A
A = subset(dat,
           dat$ver == "A")

##Now subset out list type
l1 = subset(A,
            A$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.A$AdHoc_Recall_L2

ids = c()
items = c()
scores = c()

x = str_contains("windmill", l1.key)
length(table(x)) == 1

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_A2 = testoutput[order(testoutput$items),]
output_A2 = output_A2[order(output_A2$ids),]
output_A2$list = rep("2")

####Now do list A6####
##Now subset out list type
l1 = subset(A,
            A$KEY == "AdHoc_Recall_L6")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.A$AdHoc_Recall_L6

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_A6 = testoutput[order(testoutput$items),]
output_A6 = output_A6[order(output_A6$ids),]
output_A6$list = rep("6")

####Now for Version B####
##Get B
B = subset(dat,
           dat$ver == "B")

l1 = subset(B,
            B$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.B$AdHoc_Recall_L2

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_B2 = testoutput[order(testoutput$items),]
output_B2 = output_B2[order(output_B2$ids),]
output_B2$list = rep("2")

##Now for the next list
l1 = subset(B,
            B$KEY == "AdHoc_Recall_L3")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.B$AdHoc_Recall_L3

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_B3 = testoutput[order(testoutput$items),]
output_B3 = output_B3[order(output_B3$ids),]
output_B3$list = rep("3")

####Now for Version C####
##Get Version C
C = subset(dat,
           dat$ver == "C")

##Now subset out list type
l1 = subset(C,
            C$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.C$AdHoc_Recall_L2

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_C2 = testoutput[order(testoutput$items),]
output_C2 = output_C2[order(output_C2$ids),]
output_C2$list = rep("2")

####Now do list C6####
##Now subset out list type
l1 = subset(C,
            C$KEY == "AdHoc_Recall_L6")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.C$AdHoc_Recall_L6

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_C6 = testoutput[order(testoutput$items),]
output_C6 = output_C6[order(output_C6$ids),]
output_C6$list = rep("6")

####Now for version D####
##Get D
D = subset(dat,
           dat$ver == "D")

l1 = subset(D,
            D$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.D$AdHoc_Recall_L2

temp = c()
ids = c()
key.items = c()

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_D2 = testoutput[order(testoutput$items),]
output_D2 = output_D2[order(output_D2$ids),]
output_D2$list = rep("2")

##Now for the next list
l1 = subset(D,
            D$KEY == "AdHoc_Recall_L3")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.D$AdHoc_Recall_L3

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_D3 = testoutput[order(testoutput$items),]
output_D3 = output_D3[order(output_D3$ids),]
output_D3$list = rep("3")

####Now for Version E####
##Get Version E
E = subset(dat,
           dat$ver == "E")

##Now subset out list type
l1 = subset(E,
            E$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.E$AdHoc_Recall_L2

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_E2 = testoutput[order(testoutput$items),]
output_E2 = output_E2[order(output_E2$ids),]
output_E2$list = rep("2")

####Now do list C6####
##Now subset out list type
l1 = subset(E,
            E$KEY == "AdHoc_Recall_L6")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.E$Cat_Recall_L6 ##This one had the wrong column name

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_E6 = testoutput[order(testoutput$items),]
output_E6 = output_E6[order(output_E6$ids),]
output_E6$list = rep("6")

####Now for Version F####
##Get D
f = subset(dat,
           dat$ver == "F")

l1 = subset(f,
            f$KEY == "AdHoc_Recall_L2")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.F$AdHoc_Recall_L2

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_F2 = testoutput[order(testoutput$items),]
output_F2 = output_F2[order(output_F2$ids),]
output_F2$list = rep("2")

##Now for the next list
l1 = subset(f,
            f$KEY == "AdHoc_Recall_L3")

length(unique(l1$id)) ##We have the 20 participants from version A who completed this list

l2 = na.omit(l1)

l1.key = ver.F$AdHoc_Recall_L3

ids = c()
items = c()
scores = c()

for (p in unique(l2$id)){
  
  temp = subset(l2,
                l2$id == p)
  
  for (i in l1.key){
    
    x = str_contains(i, temp$Response)
    
    print(i)
    
    print(table(x))
    
    if (length(table(x)) == 1){
      
      val = FALSE
      
      scores = c(scores, val)
      
    } else {
      
      val = TRUE
      
      scores = c(scores, val)
      
    }
    
    items = c(items, i)
    
    ids = c(ids, p)
    
  }
  
}

testoutput = data.frame(ids, items, scores)

output_F3 = testoutput[order(testoutput$items),]
output_F3 = output_F3[order(output_F3$ids),]
output_F3$list = rep("3")

##Add in the person who missed everything on the last f list
missing.id = rep("A3VKMZEZL3CCPN", times = 20)
missing.responses = rep(FALSE, times = 20)
missing.list = rep("3", times = 20)

missing = data.frame(missing.id, l1.key, missing.responses, missing.list)
colnames(missing)[1:4] = c("ids", "items", "scores", "list")
missing = missing[order(missing$items),]

#output_F3 = rbind(output_F3, missing)

##Write to .csv
final = rbind(output_A2, output_A6, output_B2, output_B3,
              output_C2, output_C6, output_D2, output_D3,
              output_E2, output_E6, output_F2, output_F3)

final = final[order(final$ids),]

##Write to file
write.csv(final, file = "adhoc_4 formatted.csv", row.names = F)
mean(final$scores)
