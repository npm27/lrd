#' Conditional Response Probability for Multiple Lists
#'
#' This function calculates the conditional response
#' probability of each lag position. Participants' lag
#' between subsequent named items is tallied and then
#' divided by the possible combination of subsequent lags
#' given their response pattern. This function was designed
#' to handle multiple or randomized lists across participants.
#'
#' This output can then be used to create a CRP visualizations,
#' and an example can be found in our manuscript/vignettes.
#'
#' @param data a dataframe of the scored free recall that you would
#' like to calculate - use prop_correct_free() for best formatting.
#' @param position a column name in the dataframe that contains
#' answered position of each response in quotes (i.e., "column")
#' @param answer a column name of the answer given for that position
#' in the original dataframe.
#' @param id a column name of the participant id in the original
#' dataframe.
#' @param key a vector containing the scoring key or data column name.
#' This column does not have to be included in the original dataframe.
#' We assume your answer key is in the tested position order. You should
#' not include duplicates in your answer key.
#' @param key.trial a vector containing the trial numbers for each answer.
#' Note: If you input long data (i.e., repeating trial-answer responses),
#' we will take the unique combination of the responses. If a trial number
#' is repeated, you will receive an error. Key and key.trial can also be
#' a separate dataframe, depending on how your output data is formatted.
#' @param id.trial a column name containing the trial numbers
#' for the participant data from the original dataframe. Note that
#' the free response "key" trial and this trial number should match.
#' The trial key will be repeated for each answer a participant gave.
#' @param scored a column in the original dataframe indicating if the
#' participant got the answer correct (1) or incorrect (0).
#'
#' @return
#' \item{DF_CRP}{A dataframe of the proportion correct for each
#' conditional lag position including any other between subjects
#' variables present in the data.}
#'
#' @keywords proportion correct, scoring, free recall, serial position
#' @export
#' @examples
#'
#' data("multi_data")
#' data("multi_answers")
#'
#' DF_long <- arrange_data(data = multi_data,
#'                        responses = "Response",
#'                        sep = " ",
#'                        id = "Sub.ID",
#'                        repeated = "List.Number")
#'
#' library(reshape)
#' multi_answers$position <- 1:nrow(multi_answers)
#' answer_long <- melt(multi_answers,
#'                     measured = colnames(multi_answers),
#'                     id = "position")
#' colnames(answer_long) <- c("position", "List.ID", "Answer")
#'
#' answer_long$List.ID <- gsub(pattern = "List",
#'                             replacement = "",
#'                             x = answer_long$List.ID)
#'
#' DF_long$response <- tolower(DF_long$response)
#' answer_long$Answer <- tolower(answer_long$Answer)
#' answer_long$Answer <- gsub(" ", "", answer_long$Answer)
#'
#' scored_output <- prop_correct_multiple(data = DF_long,
#'                                     responses = "response",
#'                                     key = answer_long$Answer,
#'                                     key.trial = answer_long$List.ID,
#'                                     id = "Sub.ID",
#'                                     id.trial = "List.Number",
#'                                     cutoff = 1,
#'                                     flag = TRUE)
#'
#' head(scored_output$DF_Scored)
#'
#' head(scored_output$DF_Participant)
#'
#'
#' crp_output <- crp_multiple(data = scored_output$DF_Scored,
#'                           key = answer_long$Answer,
#'                           position = "position",
#'                           scored = "Scored",
#'                           answer = "Answer",
#'                           id = "Sub.ID",
#'                           key.trial = answer_long$List.ID,
#'                           id.trial = "List.Number")
#'
#'  head(crp_output)
#'
crp_multiple <- function(data, position, answer, id,
                key, key.trial, id.trial, scored){

  #get list IDs
  list_ids <- unique(data[ , id.trial])

  #split the data based on ID
  data_list <- split(data, data[ , id.trial])

  answer_key <- data.frame("Answers" = key, "List.ID" = key.trial)
  answer_list <- split(answer_key, answer_key$List.ID)

  #create a storage space for the final scored data
  scored_data <- list()

  #run the function on each list separately
  for (i in 1:length(data_list)){
    scored_data[[i]] <- crp(data = data_list[[i]],
                            position = position,
                            answer = answer,
                            id = id,
                            key = answer_list[[i]]$Answers,
                            scored = scored)
  }

  DF_crp <- do.call("rbind", scored_data)
  #lengths <- unlist(lapply(scored_data, nrow))
  #DF_crp$List.ID <- rep(list_ids, lengths)

  return(DF_crp)

}

#' @rdname crp_multiple
