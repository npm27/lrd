#' Percent Agree
#'
#' This function allows you to check the percentage that
#' that two raters agree in their ratings.
#' @param x rater 1
#' @param y rater 2
#' @keywords percent
#' @export
#' @examples
#' percent_agree()

percent_agree = function(x, y) {

  temp = na.omit(data.frame(x, y))
  pa = temp[ , 1] - temp[ , 2]
  pa = temp2[1, 1]
  pa2 = (pa / nrow(temp))

  print(paste("percent agreement:",
              round(pa2 * 100, digits = 2),"%"))

}
