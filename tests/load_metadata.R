#' Load Metadata
#'
#' This function loads the current metadata available from the
#' \href{https://github.com/orgs/SemanticPriming/}{Semantic Priming GitHub Group}.
#'
#' @param webaddress The default value for webaddress is the current location
#' of the metadata list.
#' @return
#' \item{metadata}{The metadata list of avaliable datasets}
#'
#' @keywords metadata, datasets, linguistic norms
#' @import utils
#' @export
#' @examples
#'
#' #Use the following to load the metadata:
#' metadata <- load_metadata()
#' head(metadata)

load_metadata <- function(webaddress = "https://rb.gy/l82i9p") {

  #data link
  #https://raw.githubusercontent.com/SemanticPriming/LAB-data/master/included_data.csv

  # for r cran check
  included <- NULL

  metadata <- read.csv(url(webaddress), stringsAsFactors = F)

  colnames(metadata)[1] <- "included"

  metadata <- subset(metadata, included == "yes")

  return(metadata)
}

#' @rdname load_metadata
