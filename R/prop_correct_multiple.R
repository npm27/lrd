#' Proportion Correct Free Recall fro Multiple Lists
#'
#' This function computes the proportion of correct responses
#' per participant. Proportions can either be separated by
#' condition or collapsed across conditions. This function
#' extends prop_correct_multiple() to including multiple or randomized
#' lists for participants.
#'
#' Note: other columns included in the dataframe will be found
#' in the final scored dataset. If these other columns are
#' between subjects data, they will also be included in the
#' participant dataset (i.e., there's a one to one match of
#' participant ID and column information).
#'
#' @param data a dataframe of the variables you would like to return.
#' Other variables will be included in the scored output and
#' in the participant output if they are a one to one match with
#' the participant id.
#' @param responses a column name in the dataframe that contains
#' the participant answers for each item in quotes (i.e., "column")
#' @param key a vector containing the scoring key or data column name.
#' This column does not have to be included in the original dataframe.
#' @param key.trial a vector containing the trial numbers for each answer.
#' Note: If you input long data (i.e., repeating trial-answer responses),
#' we will take the unique combination of the responses. If a trial number
#' is repeated, you will receive an error. Key and key.trial can also be
#' a separate dataframe, depending on how your output data is formatted.
#' @param id a column name containing participant ID numbers from
#' the original dataframe.
#' @param id.trial a column name containing the trial numbers
#' for the participant data from the original dataframe. Note that
#' the free response "key" trial and this trial number should match.
#' The trial key will be repeated for each answer a participant gave.
#' @param cutoff a numeric value that determines the criteria for
#' scoring (i.e., 0 = strictest, 5 = is most lenient). The scoring
#' criteria uses a Levenshtein distance measure to match participant
#' responses to the answer key.
#' @param flag a logical argument if you want to flag participant scores
#' that are outliers using z-scores away from the mean score for group
#' @param group.by an optional argument that can be used to group the
#' output by condition columns. These columns should be in the original
#' dataframe and concatenated c() if there are multiple columns
#'
#' @return
#' \item{DF_Scored}{The dataframe of the original response, answer,
#' scoring, and any other or grouping variables. This dataframe can
#' be used to determine if the cutoff score and scoring matched your
#' answer key as intended. Distance measures are not perfect! Issues
#' and suggestions for improvement are welcome.}
#' \item{DF_Participant}{A dataframe of the proportion correct by
#' participant, which also includes optional z-scoring, grouping, and
#' other variables.}
#' \item{DF_Group}{A dataframe of the summary scores by any optional
#' grouping variables, along with overall total proportion correct
#' scoring.}
#'
#' @keywords proportion correct, scoring, free recall
#' @import stats
#' @import utils
#' @export
#' @examples
#'
#' data(wide_data)
#' data(answer_key_free)
#'
#' DF_long <- arrange_data(data = wide_data,
#'  responses = "Response",
#'  sep = ",",
#'  id = "Sub.ID")
#'
#' scored_output <- prop_correct_multiple(data = DF_long,
#'  responses = "response",
#'  key = answer_key_free$Answer_Key,
#'  id = "Sub.ID",
#'  cutoff = 1,
#'  flag = TRUE,
#'  group.by = "Disease.Condition")
#'
#' head(scored_output$DF_Scored)
#'
#' head(scored_output$DF_Participant)
#'
#' head(scored_output$DF_Group)
#'
prop_correct_multiple <- function(data, #data frame
                         responses, #participant responses
                         key, #answers
                         key.trial, #free order
                         id, #participant id
                         id.trial, #free to participant match
                         cutoff = 0, flag = FALSE,
                         group.by = NULL){

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
    scored_data[[i]] <- prop_correct_free(data = data_list[[i]],
                                          responses = responses,
                                          key = answer_list[[i]]$Answers,
                                          id = id,
                                          cutoff = cutoff,
                                          flag = flag,
                                          group.by = group.by)
  }

  #recreate the data
  DF_Scored <- lapply(scored_data, `[[`, 1)
  DF_Scored <- do.call("rbind", DF_Scored)

  DF_Participant <- lapply(scored_data, `[[`, 2)
  DF_Participant <- do.call("rbind", DF_Participant)

  if (!is.null(group.by)){
    DF_Group <- lapply(scored_data, `[[`, 3)
    DF_Group <- do.call("rbind", DF_Group)

    return(list(DF_Scored, DF_Participant, DF_Group))
  } else {
    return(list(DF_Scored, DF_Participant))
  }

}

#' @rdname prop_correct_multiple
