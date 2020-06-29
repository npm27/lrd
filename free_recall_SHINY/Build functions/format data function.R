#Let's try it!
#make an empty df for storage

arrange.dat = function(x, sep = y, id = z){

df = data.frame()
lengths = data.frame()
df2 = data.frame()
df3 = data.frame()

  for (i in x){

    for (j in 1:length(x)){

      x2 = strsplit(i, split = sep)

      x2 = data.frame(x2)
      colnames(x2)[1] = "response"

      lengths2 = nrow(x2)

      lengths2 = data.frame(lengths2)
      colnames(lengths2)[1] = "Length"

    }

    lengths = rbind(lengths, lengths2)
    lengths$Length = as.numeric(lengths$Length)

    df = rbind(df, x2)

  }

  #Get subject IDs

  s = 0

  for (m in lengths$Length){

    s = s + 1

    for (h in 1:length(id)){

      x3 = rep(id[s], m)

      x3 = data.frame(x3)
      colnames(x3)[1] = "Sub.ID"

      }

    df2 = rbind(df2, x3)

  }

  #Now add in position tagging

  s = 0 #Reset the counter

  for (m in lengths$Length){

    s = s + 1

    for (h in 1:length(id)){

      x4 = rep(1:m)

      x4 = data.frame(x4)
      colnames(x4)[1] = "pos.tag"

    }

    df3 = rbind(df3, x4)

  }

  #Remove spaces
  df = as.data.frame(apply(df, 2, function(y)gsub('\\s+', '',y)))

  final = cbind(df2, df, df3)

}

output2 = arrange.dat(dat$Response, id = dat$Sub.ID, sep = ",")
