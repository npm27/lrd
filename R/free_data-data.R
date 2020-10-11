#' Free Recall Data
#'
#' Dataset that includes free recall data in long format.
#' Participants were given a list of words to remember, and
#' then asked to recall the words. This dataset
#' is in wide format, which should be converted with arrange
#' data.
#'
#' @docType data
#'
#' @usage data(free_data)
#'
#' @format A data frame of answers for a free recall test data
#'
#' Username: the participant id
#' List_Types: a repeated measures condition participants were in
#' Response: the response the participant gave to the cue
#' Version: the version of the list_type given
#' Batch: the batch of participants that were run together
#'
#' @keywords datasets
#'
"free_data"
