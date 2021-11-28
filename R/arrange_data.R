#' Arrange Data for Free Recall Scoring
#'
#' This function takes wide format free recall data where all
#' responses are stored in the same cell and converts it to long format.
#'
#' @param data a dataframe of the variables you would like to return.
#' Other variables will be included in the returned output in long format
#' if they represent a one to one match with the participant ID. If you
#' have repeated data, please use the repeated argument or run this
#' function several times for each trial.
#' @param responses a column name in the dataframe that contains
#' the participant answers for each item in quotes (i.e., "column")
#' @param sep a character separating each response in quotes - example: ",".
#' @param id a column name containing participant ID numbers from
#' the original dataframe
#' @param repeated (optional) a single column name or set of columns
#' that indicate repeated measures columns you would like to
#' keep with the data. You should include all columns that are not a one
#' to one match with the subject ID (i.e., participants saw multiple
#' trials). Please see our vignette for an example.
#'
#' @return A dataframe of the participant answers including:
#' \item{Sub.ID}{The participant id number}
#' \item{response}{The participant response}
#' \item{position}{The position number of the response listed}
#' \item{other}{Any additional columns included}
#'
#' @keywords arrange data wide long
#' @import stats
#' @export
#' @examples
#'
#' #This dataset includes a subject number, set of answers, and
#' #experiment condition.
#'
#' data(wide_data)
#'
#' DF_long <- arrange_data(
#'  data = wide_data,
#'  responses = "Response",
#'  sep = ",",
#'  id = "Sub.ID")
#'
#' head(DF_long)
#'
arrange_data <- function(data, responses, sep, id, repeated = NULL){

  if (is.null(repeated)){

    #get the data
    data <- as.data.frame(data)
    colnames(data)[grepl(id, colnames(data))] <- "Sub.ID"

    #split the strings, returns a list
    answers <- strsplit(data[ , responses], split = sep)

    #list to long
    df <- data.frame(response = unlist(answers))

    #take out any extra spaces
    df$response <- gsub("\\s+", " ", df$response)
    df$response <- trimws(df$response, "both")

    #add participant id
    df$Sub.ID <- rep(data$Sub.ID, unlist(lapply(answers, length)))

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
  } else {

    #get the data
    data <- as.data.frame(data)
    colnames(data)[grepl(id, colnames(data))] <- "Sub.ID"

    #create unique identifier
    data$unique_ID <- apply(data[ , c("Sub.ID", repeated)],
                            1, paste, collapse = "~~")


    #split the strings, returns a list
    answers <- strsplit(data[ , responses], split = sep)

    #list to long
    df <- data.frame(response = unlist(answers))

    #take out any extra spaces
    df$response <- gsub("\\s+", " ", df$response)
    df$response <- trimws(df$response, "both")

    #add unique ID
    df$unique_ID <-rep(data$unique_ID, unlist(lapply(answers, length)))

    #add position tag
    df$position <- ave(df$unique_ID, df$unique_ID, FUN = seq_along)

    #split columns back
    df <- cbind(df, do.call('rbind',  strsplit(as.character(df$unique_ID), "~~", fixed = TRUE)))

    #rename the new columns
    colnames(df) <- c("response", "unique_ID", "position", "Sub.ID", repeated)

    #drop unique id
    df$unique_ID <- NULL
    data$unique_ID <- NULL

    #add back in other columns that are one to one
    other.columns <- setdiff(colnames(data),
                             c("response", "Sub.ID", "position", responses, repeated))
    for (col in other.columns){
      data_temp <- unique(data[ , c("Sub.ID", col)])
      if (sum(duplicated(data$Sub.ID)) == 0){
        df <- merge(df, data_temp, by = "Sub.ID")
      }
    }

  } #end else


  return(df)
}

#' @rdname arrange_data
