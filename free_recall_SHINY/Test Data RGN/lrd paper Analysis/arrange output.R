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

colnames(ver.E)[6] = "AdHoc_Recall_L6"

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

####version A####
####Let's start with Adhoc 0
dat = read.csv("Adhoc/Adhoc_1.csv")


A = subset(dat,
           dat$ver == "A")

##Now subset out list type
l1 = subset(A,
            A$KEY == "AdHoc_Recall_L2")

length(unique(l1$id))

l2 = na.omit(l1)

dat$ver == A$AdHoc_Recall_L2 ##Remember to switch this out

for (af in unique(l2$id)){

  test = subset(l2,
                l2$id == af)
  
  ids = c()
  items = c()
  scores = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$output)
      
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
  
  test.true = subset(testoutput,
                      testoutput$scores == TRUE)
  test.false = subset(testoutput,
                     testoutput$scores == FALSE)
  
  ids2 = c()
  items2 = c()
  scores2 = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$Response)
      
      print(i)
      
      print(table(x))
      
      if (length(table(x)) == 1){
        
        val = FALSE
        
        scores2 = c(scores2, val)
        
      } else {
        
        val = TRUE
        
        scores2 = c(scores2, val)
        
      }
      
      items2 = c(items2, i)
      
      ids2 = c(ids2, p)
      
    }
    
  }
  
  testoutput2 = data.frame(ids2, items2, scores2)
  
  colnames(testoutput2)[1:3] = c("ids", "items", "scores")
  
  
  test2.true = subset(testoutput2,
                      testoutput2$scores == TRUE)
  test2.false = subset(testoutput2,
                       testoutput2$scores == FALSE)
  
  thing = rbind(test.true, test2.true)
  thing2 = rbind(test.false, test2.false)
  
  thing$dupes = duplicated(thing$items)
  thing2$dupes = duplicated(thing2$items)
  
  thing = subset(thing,
                 thing$dupes == FALSE)
  thing2 = subset(thing2,
                 thing2$dupes == FALSE)
  
  thing3 = rbind(thing, thing2)
  thing3$dupes2 = duplicated(thing3$items)
  
  thing3 = subset(thing3, thing3$dupes2 == FALSE)
  
  thing3 = thing3[ , c(1:3)]
  
  thing3$scores = as.numeric(thing3$scores)
  mean(thing3$scores)
  
  duplicated(thing$items)
  
  write.csv(thing3, file = paste0(af, "_lev1_l1.csv"))

}

####Let's start with Adhoc 0
dat = read.csv("Adhoc/Adhoc_2.csv")


A = subset(dat,
           dat$ver == "A")

##Now subset out list type
l1 = subset(A,
            A$KEY == "AdHoc_Recall_L2")

length(unique(l1$id))

l2 = na.omit(l1)

dat$ver == A$AdHoc_Recall_L2 ##Remember to switch this out

for (af in unique(l2$id)){
  
  test = subset(l2,
                l2$id == af)
  
  ids = c()
  items = c()
  scores = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$output)
      
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
  
  test.true = subset(testoutput,
                     testoutput$scores == TRUE)
  test.false = subset(testoutput,
                      testoutput$scores == FALSE)
  
  ids2 = c()
  items2 = c()
  scores2 = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$Response)
      
      print(i)
      
      print(table(x))
      
      if (length(table(x)) == 1){
        
        val = FALSE
        
        scores2 = c(scores2, val)
        
      } else {
        
        val = TRUE
        
        scores2 = c(scores2, val)
        
      }
      
      items2 = c(items2, i)
      
      ids2 = c(ids2, p)
      
    }
    
  }
  
  testoutput2 = data.frame(ids2, items2, scores2)
  
  colnames(testoutput2)[1:3] = c("ids", "items", "scores")
  
  
  test2.true = subset(testoutput2,
                      testoutput2$scores == TRUE)
  test2.false = subset(testoutput2,
                       testoutput2$scores == FALSE)
  
  thing = rbind(test.true, test2.true)
  thing2 = rbind(test.false, test2.false)
  
  thing$dupes = duplicated(thing$items)
  thing2$dupes = duplicated(thing2$items)
  
  thing = subset(thing,
                 thing$dupes == FALSE)
  thing2 = subset(thing2,
                  thing2$dupes == FALSE)
  
  thing3 = rbind(thing, thing2)
  thing3$dupes2 = duplicated(thing3$items)
  
  thing3 = subset(thing3, thing3$dupes2 == FALSE)
  
  thing3 = thing3[ , c(1:3)]
  
  thing3$scores = as.numeric(thing3$scores)
  mean(thing3$scores)
  
  duplicated(thing$items)
  
  write.csv(thing3, file = paste0(af, "_lev2_l1.csv"))
  
}

####Let's start with Adhoc 0
dat = read.csv("Adhoc/Adhoc_3.csv")


A = subset(dat,
           dat$ver == "A")

##Now subset out list type
l1 = subset(A,
            A$KEY == "AdHoc_Recall_L2")

length(unique(l1$id))

l2 = na.omit(l1)

dat$ver == A$AdHoc_Recall_L2 ##Remember to switch this out

for (af in unique(l2$id)){
  
  test = subset(l2,
                l2$id == af)
  
  ids = c()
  items = c()
  scores = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$output)
      
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
  
  test.true = subset(testoutput,
                     testoutput$scores == TRUE)
  test.false = subset(testoutput,
                      testoutput$scores == FALSE)
  
  ids2 = c()
  items2 = c()
  scores2 = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$Response)
      
      print(i)
      
      print(table(x))
      
      if (length(table(x)) == 1){
        
        val = FALSE
        
        scores2 = c(scores2, val)
        
      } else {
        
        val = TRUE
        
        scores2 = c(scores2, val)
        
      }
      
      items2 = c(items2, i)
      
      ids2 = c(ids2, p)
      
    }
    
  }
  
  testoutput2 = data.frame(ids2, items2, scores2)
  
  colnames(testoutput2)[1:3] = c("ids", "items", "scores")
  
  
  test2.true = subset(testoutput2,
                      testoutput2$scores == TRUE)
  test2.false = subset(testoutput2,
                       testoutput2$scores == FALSE)
  
  thing = rbind(test.true, test2.true)
  thing2 = rbind(test.false, test2.false)
  
  thing$dupes = duplicated(thing$items)
  thing2$dupes = duplicated(thing2$items)
  
  thing = subset(thing,
                 thing$dupes == FALSE)
  thing2 = subset(thing2,
                  thing2$dupes == FALSE)
  
  thing3 = rbind(thing, thing2)
  thing3$dupes2 = duplicated(thing3$items)
  
  thing3 = subset(thing3, thing3$dupes2 == FALSE)
  
  thing3 = thing3[ , c(1:3)]
  
  thing3$scores = as.numeric(thing3$scores)
  mean(thing3$scores)
  
  duplicated(thing$items)
  
  write.csv(thing3, file = paste0(af, "_lev3_l1.csv"))
  
}

####Let's start with Adhoc 0
dat = read.csv("Adhoc/Adhoc_0.csv")


A = subset(dat,
           dat$ver == "A")

##Now subset out list type
l1 = subset(A,
            A$ListNum == "2")

length(unique(l1$id))

l2 = na.omit(l1)

dat$ver == A$AdHoc_Recall_L2 ##Remember to switch this out

for (af in unique(l2$id)){
  
  test = subset(l2,
                l2$id == af)
  
  ids = c()
  items = c()
  scores = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$output)
      
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
  
  test.true = subset(testoutput,
                     testoutput$scores == TRUE)
  test.false = subset(testoutput,
                      testoutput$scores == FALSE)
  
  ids2 = c()
  items2 = c()
  scores2 = c()
  
  for (p in unique(test$id)){
    
    temp = subset(test,
                  test$id == p)
    
    for (i in l1.key){
      
      x = str_contains(i, temp$Response)
      
      print(i)
      
      print(table(x))
      
      if (length(table(x)) == 1){
        
        val = FALSE
        
        scores2 = c(scores2, val)
        
      } else {
        
        val = TRUE
        
        scores2 = c(scores2, val)
        
      }
      
      items2 = c(items2, i)
      
      ids2 = c(ids2, p)
      
    }
    
  }
  
  testoutput2 = data.frame(ids2, items2, scores2)
  
  colnames(testoutput2)[1:3] = c("ids", "items", "scores")
  
  
  test2.true = subset(testoutput2,
                      testoutput2$scores == TRUE)
  test2.false = subset(testoutput2,
                       testoutput2$scores == FALSE)
  
  thing = rbind(test.true, test2.true)
  thing2 = rbind(test.false, test2.false)
  
  thing$dupes = duplicated(thing$items)
  thing2$dupes = duplicated(thing2$items)
  
  thing = subset(thing,
                 thing$dupes == FALSE)
  thing2 = subset(thing2,
                  thing2$dupes == FALSE)
  
  thing3 = rbind(thing, thing2)
  thing3$dupes2 = duplicated(thing3$items)
  
  thing3 = subset(thing3, thing3$dupes2 == FALSE)
  
  thing3 = thing3[ , c(1:3)]
  
  thing3$scores = as.numeric(thing3$scores)
  mean(thing3$scores)
  
  duplicated(thing$items)
  
  write.csv(thing3, file = paste0(af, "_lev0_l1.csv"))
  
}

