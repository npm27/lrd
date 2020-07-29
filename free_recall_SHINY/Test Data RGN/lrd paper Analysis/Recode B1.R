
####Let's start with Cat 0
dat = read.csv("Cat/Cat_1.csv")


A = subset(dat,
           dat$ver == "B")

##Now subset out list type
l1 = subset(A,
            A$KEY == "Cat_Recall_L4")

length(unique(l1$id))

l2 = na.omit(l1)

l1.key = ver.B$Cat_Recall_L4 ##Remember to switch this out

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

####Let's start with Cat 0
dat = read.csv("Cat/Cat_2.csv")


A = subset(dat,
           dat$ver == "B")

##Now subset out list type
l1 = subset(A,
            A$KEY == "Cat_Recall_L4")

length(unique(l1$id))

l2 = na.omit(l1)

l1.key = ver.B$Cat_Recall_L4 ##Remember to switch this out

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

####Let's start with Cat 0
dat = read.csv("Cat/Cat_3.csv")


A = subset(dat,
           dat$ver == "B")

##Now subset out list type
l1 = subset(A,
            A$KEY == "Cat_Recall_L4")

length(unique(l1$id))

l2 = na.omit(l1)

l1.key = ver.B$Cat_Recall_L4 ##Remember to switch this out

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

####Let's start with Cat 0
dat = read.csv("Cat/Cat_0.csv")


A = subset(dat,
           dat$ver == "B")

##Now subset out list type
l1 = subset(A,
            A$KEY == "Cat_Recall_L4")

length(unique(l1$id))

l2 = na.omit(l1)

l1.key = ver.B$Cat_Recall_L4 ##Remember to switch this out

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

####Let's start with Cat 0
dat = read.csv("Cat/Cat_4.csv")


A = subset(dat,
           dat$ver == "B")

##Now subset out list type
l1 = subset(A,
            A$KEY == "Cat_Recall_L4")

length(unique(l1$id))

l2 = na.omit(l1)

l1.key = ver.B$Cat_Recall_L4 ##Remember to switch this out

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

  write.csv(thing3, file = paste0(af, "_lev4_l1.csv"))

}
