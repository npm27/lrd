#' Arrange Data for Free Recall Scoring
#'
#' This function takes wide format free recall data where all
#' responses are stored in the same cell and converts it to long format.
#'
#' @param data a dataframe of the variables you would like to return.
#' Other variables will be included in the returned output in long format.
#' @param responses a column name in the dataframe that contains
#' the participant answers for each item in quotes (i.e., "column")
#' @param sep a character separating each response in quotes - example: ",".
#' @param id a column name containing participant ID numbers from
#' the original dataframe
#'
#' @return A dataframe of the participant answers including:
#' \item{Sub.ID}{The participant id number}
#' \item{response}{The participant response}
#' \item{position}{The position number of the response listed}
#' \item{other}{Any additional columns included}
#'
#' @keywords data, arrange, wide, long
#' @import stats
#' @export
#' @examples
#'
#' #This dataset includes a subject number, set of answers, and
#' #experiment condition.
#'
#' load("data/wide_data.Rdata")
#' DF_long <- arrange_data(
#'  data = wide_data,
#'  responses = "Response",
#'  sep = ",",
#'  id = "Sub.ID")
#' head(DF_long)
#'
arrange_data <- function(data, responses, sep, id){

  #get the data
  data <- as.data.frame(data)

  #split the strings, returns a list
  answers <- strsplit(data[ , responses], split = sep)

  #list to long
  df <- data.frame(response = unlist(answers))

  #take out any extra spaces
  df$response <- gsub("\\s+", " ", df$response)
  df$response <- trimws(df$response, "both")

  #add participant id
  df$Sub.ID <- rep(data[ , id], unlist(lapply(answers, length)))

  #add position tag
  df$position <- ave(df$Sub.ID, df$Sub.ID, FUN = seq_along)

  #add back in other columns that are one to one
  other.columns <- setdiff(colnames(data),
                           c("response", "Sub.ID", "position", responses))
  for (col in other.columns){
    data_temp <- unique(data[ , c("Sub.ID", col)])
    if (sum(duplicated(data$Sub.ID)) == 0){
      df <- merge(df, data_temp, by = "Sub.ID")
    }
  }

  return(df)
}

#' @rdname arrange_data
