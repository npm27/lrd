#' Score cued-recall data
#'
#' Use this function to score cued-recall data based on percent match
#'
#' @param x a dataframe object containing the scored data
#' @param set.cutoff a value used to specify the scoring cutoff. Must be a value between 0 and 1.
#' @return a .csv file containing the scored output
#' @export

score_recall.cr = function(x, weight = FALSE, cutoff = y){

  if (weight == FALSE){

    x$scored = as.numeric(x$percent_match >= cutoff)
    x$scored[is.na(x$scored)] = 0

    write.csv(x, file = "output.csv", row.names = F)

  }

  else if (weight == TRUE){

    x$scored = as.numeric(x$weighted_match >= cutoff)
    x$scored[is.na(x$scored)] = 0

    write.csv(x, file = "output.csv", row.names = F)

    }

}



