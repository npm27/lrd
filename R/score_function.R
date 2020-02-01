#' Score Lexical Data
#'
#' Use this function to score lexical data based on percent match
#'
#' @param x a dataframe object containing the scored data
#' @return a .csv file containing the scored output
#' @export

score_recall = function(x, weight = FALSE, set.cutoff = y){

  if (weight == FALSE){

    x$scored = as.numeric(x$percent_match >= set.cutoff)
    x$scored[is.na(x$scored)] = 0

    write.csv(x, file = "output.csv", row.names = F)

  }

  else if (weight == TRUE){

    x$scored = as.numeric(x$weighted_match >= set.cutoff)
    x$scored[is.na(x$scored)] = 0

    write.csv(x, file = "output.csv", row.names = F)

    }

}



