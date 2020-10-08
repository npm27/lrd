#' Arrange Data for Free Recall Scoring
#'
#' This function takes wide format free recall data where all
#' responses are stored in the same cell and converts it to long format.
#'
#' @param responses a vector containing participant responses
#' @param sep a character separating each response in quotes - example: ",".
#' @param id a vector containing participant ID numbers
#' @param other an optional argument for passing condition or other columns
#' you would like to include with the rearranged data. Note: It should
#' contain the same number of rows as the id variable.
#' @param other.names an optional argument that can be used to
#' name the other variables in your output
#'
#' @return A dataframe of the participant answers including:
#' \item{Sub.ID}{The participant id number}
#' \item{response}{The participant response}
#' \item{position}{The position number of the response listed}
#' \item{other}{Any additional columns included}
#'
#' @keywords data, arrange, wide, long
#' @export
#' @examples
#'
#' #This dataset includes a subject number, set of answers, and
#' #experiment condition.
#'
#' DF <- read.csv("data/wide_data.csv")
#' DF_long <- arrange_data(responses = DF$Response, sep = ",",
#'            id = DF$Sub.ID, other = DF$Disease.Condition)
#' head(DF_long)
#'
arrange_data <- function(responses, sep, id,
                         other = NULL, other.names = NULL){

  #split the strings, returns a list
  answers <- strsplit(responses, split = sep)

  #list to long
  df <- data.frame(response = unlist(answers))

  #take out any extra spaces
  df$response <- gsub("\\s+", " ", df$response)
  df$response <- trimws(df$response, "both")

  #add participant id
  df$Sub.ID <- rep(id, unlist(lapply(answers, length)))

  #add position tag
  df$position <- ave(df$Sub.ID, df$Sub.ID, FUN = seq_along)

  #add back in other variables
  if(!is.null(other)){

    #make a dataframe
    other <- as.data.frame(other)
    if (!is.null(other.names)){
      if(ncol(other) != length(other.names)){
        stop("Your other columns and other.names arguments do not have the
             same number of items. Please check your code.")
      }
      colnames(other) <- other.names
    }

    #check its length
    if(nrow(other) != length(id)){
      stop("The number of rows or items in the additional columns need to match
           the number of participants in the id column.")
    } else {
        other$Sub.ID <- id
        final_df <- merge(df, other, by = "Sub.ID")
    }
    #otherwise just return final data
  } else {
      final_df <- df
    }

  return(final_df)
}

#' @rdname arrange_data
