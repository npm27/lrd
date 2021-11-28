#' Serial Position Calculator for Multiple Lists
#'
#' This function calculates the proportion correct of each item in the
#' serial position curve. Data should include the participant's answers
#' in long format (use arrange_data() in this package for help), the answer
#' key of the items in order, and a column that denotes the order a
#' participant listed each item. The function will then calculate
#' the items remembered within a window of 1 before or 1 after the
#' tested position. The first and last positions must be answered in the
#' correct place. Specifically, this function is an extension of
#' serial_position() for free recall when there are multiple lists
#' or randomized lists.
#'
#' This output can then be used to create a serial position curve visualizations,
#' and an example can be found in our manuscript/vignettes.
#'
#'
#' @param data a dataframe of the scored free recall that you would
#' like to calculate - use prop_correct_multiple() for best formatting.
#' @param position a column name in the dataframe that contains
#' answered position of each response in quotes (i.e., "column")
#' @param answer a column name of the answer given for that position
#' in the original dataframe.
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
#' @param group.by an optional argument that can be used to group the
#' output by condition columns. These columns should be in the original
#' dataframe and concatenated c() if there are multiple columns
#'
#' @return
#' \item{DF_Serial}{A dataframe of the proportion correct for each
#' tested position by any optional grouping variables included.}
#'
#' @keywords proportion correct scoring free recall serial position
#' @import stats
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
#'serial_output <- serial_position_multiple(data = scored_output$DF_Scored,
#'                                          position = "position",
#'                                          answer = "Answer",
#'                                          key = answer_long$Answer,
#'                                          key.trial = answer_long$List.ID,
#'                                          scored = "Scored",
#'                                          id.trial = "List.Number")
#'
#'  head(serial_output)
#'
serial_position_multiple <- function(data, position, answer,
                            key, key.trial, id.trial,
                            scored, group.by = NULL){

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
    scored_data[[i]] <- serial_position(data = data_list[[i]],
                                        position = position,
                                        answer = answer,
                                        key = answer_list[[i]]$Answers,
                                        scored = scored,
                                        group.by = group.by)
  }

  DF_serial <- do.call("rbind", scored_data)
  lengths <- unlist(lapply(scored_data, nrow))
  DF_serial$List.ID <- rep(list_ids, lengths)

  return(DF_serial)

}

#' @rdname serial_position_multiple
