#X = participant responses (see Guess recall data files for formatting example)
#Sep = whatever character is separating responses from the same response set
#Id = participant id
#Other = any condition columns
#Right now these will need to be specified through indexing -- so other = dat[ , c(2:3)]

arrange.data = function(x, sep = y, id = z, other = NULL){

  df = data.frame()
  lengths = data.frame()
  df2 = data.frame()
  df3 = data.frame()
  x6 = c()

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

  ##Add in subject IDs

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

  ##Now add in position tagging

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

  if(is.null(other) == FALSE){

    namelist = names(other)

    df5 = data.frame(matrix(NA, nrow = nrow(df2), ncol = length(other)))

    for (i in namelist){

      #print(i)

      sub1 = as.data.frame(other[`i`])

      q = 0
      q = q + 1

      r = 0

      j = 1

      for (t in lengths$Length){

        r = r + 1

        x5 = (rep(sub1[r, ], t))

        x6 = c(x6, x5)

      }

      ##The above loop puts all the columns in one list. Now I need to sub divide it up into the correct number of columns
    }

    rownum = nrow(df) #Get number of rows in the final dataset

    splitstuff = split(x6, ceiling(seq_along(x6)/rownum))

    splitstuff = data.frame(splitstuff)

    c = 0

    for (i in namelist){

      c = c + 1

      colnames(splitstuff)[c] = i

    }

    #Remove spaces
    df = as.data.frame(apply(df, 2, function(y)gsub('\\s+', '',y)))

    final = cbind(df2, df, df3, splitstuff)

  } else if (is.null(other) == TRUE){

    #Remove spaces
    df = as.data.frame(apply(df, 2, function(y)gsub('\\s+', '',y)))

    final = cbind(df2, df, df3)

  }

}
