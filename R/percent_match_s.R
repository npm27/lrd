#' Compute the Percent Match between two words
#'
#' This function computes the percentage of shared characters between two strings
#'
#' @param x word 1, a character string
#' @param y word 2, a character string
#' @return a percent match value
#' @import vecsets
#' @export

percent_match.w = function(x, y){

  char.a1 = strsplit(x, "")
  char.b1 = strsplit(y, "")


  length(vintersect(char.a1[[1]], char.b1[[1]])) / max(c(length((char.a1[[1]])), length((char.b1[[1]]))))

}
