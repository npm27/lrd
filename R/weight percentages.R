#' Compute Percent Match
#'
#' This function computes the percentage of shared characters between two strings
#'
#' @param x a vector containing input data
#' @return a dataframe object containing the input vectors and a percent match column
#' @export

percent_match = function(x, key = y, id = z, weight = FALSE, weight.by = NULL){

  xx = weight.by

  input = data.frame(id, x, key)

  input$id = as.character(input$id)
  input$x = as.character(input$x)
  input$key = as.character(input$key)

  input$length_x = nchar(input$x)
  input$length_key = nchar(input$key)
  input$diff = input$length_x - input$length_key

  sub1 = subset(input,
                input$diff >= 1)

  sub2 = subset(input,
                input$diff == 0)

  sub3 = subset(input,
                input$diff < 0)

  sub1 = sub1[ , -c(4:6)]
  sub2 = sub2[ , -c(4:6)]
  sub3 = sub3[ , -c(4:6)]

  char.x1 = vector(mode = "character", length = nrow(sub1))
  char.x2 = vector(mode = "character", length = nrow(sub2))
  char.x3 = vector(mode = "character", length = nrow(sub3))

  char.y1 = vector(mode = "character", length = nrow(sub1))
  char.y2 = vector(mode = "character", length = nrow(sub2))
  char.y3 = vector(mode = "character", length = nrow(sub3))

  percent_match1 = vector(mode = "character", length = nrow(sub1))
  percent_match2 = vector(mode = "character", length = nrow(sub2))
  percent_match3 = vector(mode = "character", length = nrow(sub3))

  for (i in 1:nrow(sub1)) {

    char.x1[i] = strsplit(sub1$x[i], "")
    char.y1[i] = strsplit(sub1$key[i], "")

  }

  for (k in 1:nrow(sub1)) {

    c = char.x1[[k]]
    d = char.y1[[k]]

    percent_match1[k] = length(na.omit(d[is.element(c, d)])) /
      length(c)

    sub1$percent_match = percent_match1

  }

  for (h in 1:nrow(sub2)) {

    char.x2[h] = strsplit(sub2$x[h], "")
    char.y2[h] = strsplit(sub2$key[h], "")

  }

  for (j in 1:nrow(sub2)) {

    e = char.x2[[j]]
    f = char.y2[[j]]

    percent_match2[j] = length(na.omit(f[is.element(e, f)])) /
      length(e)

    sub2$percent_match = percent_match2

  }

  for (q in 1:nrow(sub3)) {

    char.x3[q] = strsplit(sub3$x[q], "")
    char.y3[q] = strsplit(sub3$key[q], "")

  }

  for (p in 1:nrow(sub3)) {

    r = char.x3[[p]]
    s = char.y3[[p]]

    percent_match3[p] = length(unique(r[r %in% s[s %in% r]])) /
      length(s)

    sub3$percent_match = percent_match3

  }

  output = rbind(sub1, sub2, sub3)
  output = output[order(as.numeric(rownames(output))),,drop = FALSE]

  if (weight == TRUE){

    if (weight.by <= 1 & is.null(weight.by) == FALSE){

      sub4 = subset(output,
                    output$percent_match == 1)
      sub5 = subset(output,
                    output$percent_match == 0)
      sub6 = subset(output,
                    output$percent_match < 1 & output$percent_match > 0)

      sub4$weighted_match = as.numeric(sub4$percent_match)
      sub5$weighted_match = as.numeric(sub5$percent_match)

      sub6$weighted_match = as.numeric(sub6$percent_match) + (weight.by / nchar(sub6$key))

      output2 = rbind(sub4, sub5, sub6)

      output2 = output2[order(as.numeric(rownames(output2))),,drop = FALSE]
      print(output2)

    }

    else if (is.null(weight.by) == FALSE & weight.by > 1) {

      print("weight.by must be a value between 0 and 1!")

    }

  }

  else if (weight == FALSE) {

    print(output)

  }

}
