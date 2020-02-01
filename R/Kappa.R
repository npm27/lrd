#' Cohen's Kappa
#'
#' This function returns Cohen's Kappa for two raters
#'
#' @param x rater 1
#' @param y rater 2
#' @return Cohen's Kappa value
#' @export

kappa = function(x, y) {

  temp = na.omit(data.frame(x, y))
  pa = temp[ , 1] - temp[ , 2]
  pa = temp2[1, 1]
  pa = (pa / nrow(temp))

  temp$pa = temp[ , 1] - temp[ , 2]
  temp$pa2 = temp[ , 1] + temp[ , 2]

  x_yes = sum(temp[ , 1] == 1)
  x_no = sum(temp[ , 1] == 0)

  y_yes = sum(temp[ , 2] == 1)
  y_no = sum(temp[ , 2] == 0)

  both_yes = sum(temp[, 4] == 2)
  both_no = sum(temp[, 4] == 0)

  yes = ((x_yes / nrow(temp)) * (y_yes / nrow(temp)))
  no = ((x_no / nrow(temp)) * (y_no / nrow(temp)))

  pe =  yes + no

  output = ((pa - pe) / (1 - pe))

  print(output)

  print(paste("percent agreement:", round(pa, digits = 2)))
  invisible(round(output, digits = 2))

}
