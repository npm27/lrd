#' Load Metadata
#'
#' This function loads the current metadata avaliable from the
#' \href{https://github.com/orgs/SemanticPriming/}{Semantic Priming GitHub Group}.
#'
#' @param webaddress The default value for webaddress is the current location
#' of the metadata list.
#' @return
#' \item{metadata}{The metadata list of avaliable datasets}
#'
#' @keywords metadata, datasets, linguistic norms
#' @export
#' @examples
#'
#' #Use the following to load the metadata:
#' metadata <- load_metadata()
#' View(metadata)

load_metadata <- function(webaddress = "https://raw.githubusercontent.com/SemanticPriming/LAB-data/master/included_data.csv") {

  metadata <- read.csv(url(webaddress), stringsAsFactors = F)
  
  colnames(metadata)[1] <- "included"

  metadata <- subset(metadata, included == "yes")

  return(metadata)
}

#' @rdname load_metadata
