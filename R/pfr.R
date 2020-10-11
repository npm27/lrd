#' Probability of First Recall
#'
#' This function calculates the probability of first recall
#' for each serial position. The total number of times an
#' item was recalled first is divided by the total number of
#' first recalls (i.e., the number of participants who wrote
#' anything down!).
#'
#' This output can then be used to create a PFR visualizations,
#' and an example can be found in our manuscript/vignettes.
#'
#' Important: The code is written assuming the data provided are for
#' a single recall list. If repeated measures are used (i.e., there are
#' multiple lists completed by each participant or multiple list versions),
#' you should use this function several times, once on each list/answer key.
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
#' @param scored a column in the original dataframe indicating if the
#' participant got the answer correct (1) or incorrect (0).
#' @param group.by an optional argument that can be used to group the
#' output by condition columns. These columns should be in the original
#' dataframe and concatenated c() if there are multiple columns
#'
#' @return
#' \item{DF_PFR}{A dataframe of the probability of first response
#' for each position including group by variables if indicated.}
#'
#' @keywords proportion correct, scoring, free recall, serial position
#' @export
#' @examples
#'
#' data("data/free_data.RData")
#' data("data/answer_key_free2.RData")
#'
#' free_data <- subset(free_data,
#'  List_Type == "Cat_Recall_L1")
#'
#' DF_long <- arrange_data(data = free_data,
#'  responses = "Response",
#'  sep = " ",
#'  id = "Username")
#'
#' scored_output <- prop_correct_free(data = DF_long,
#'  responses = "response",
#'  key = answer_key_free2$Answer_Key,
#'  id = "Sub.ID",
#'  cutoff = 1,
#'  flag = TRUE,
#'  group.by = "Version")
#'
#' pfr_output <- pfr(data = scored_output$DF_Scored,
#'  position = "position",
#'  answer = "Answer",
#'  id = "Sub.ID",
#'  key = answer_key_free2$Answer_Key,
#'  scored = "Scored",
#'  group.by = "Version")
#'
#'  head(pfr_output)
#'
pfr <- function(data, position, answer, id,
                key, scored, group.by){

  # for r cran check
  Answered.Position <- NULL

  #create answer key with order
  if (sum(duplicated(key)) > 0){
    stop("Your answer key contains duplicates. Please check your data.")
  }
  key <- data.frame("Answer" = key, "Tested.Position" = 1:length(key))

  #merge that with the data
  DF <- as.data.frame(data)
  colnames(DF)[grepl(answer, colnames(DF))] <- "Answer"
  colnames(DF)[grepl(position, colnames(DF))] <- "Answered.Position"
  colnames(DF)[grepl(scored, colnames(DF))] <- "Scored"
  colnames(DF)[grepl(id, colnames(DF))] <- "Sub.ID"
  DF <- merge(DF, key, by = "Answer")

  #calculate the number of times within window
  DF$Answered.Position <- as.numeric(DF$Answered.Position)
  DF$Tested.Position <- as.numeric(DF$Tested.Position)

  DF_first <- subset(DF, Answered.Position == 1)

  if(!is.null(group.by)){
    pfr_table <- as.data.frame(table(DF_first[ , c("Tested.Position", group.by)]))

    DF_unique <- unique(DF[ , c("Sub.ID", group.by)])
    group_sizes <- as.data.frame(table(DF_unique[ , group.by]))
    group_sizes$group_code <- paste(group_sizes[ , -ncol(group_sizes)])

    group_code <- paste(pfr_table[ , group.by])
    pfr_table$pfr <- NA

    for (group in group_code){
      pfr_table$pfr[group_code == group] <- pfr_table$Freq[group_code == group] / group_sizes$Freq[group_sizes$group_code == group]
    }

  } else{

    pfr_table <- as.data.frame(table(DF_first$Tested.Position))
    group_sizes <- length(unique(DF$Sub.ID))
    pfr_table$pfr <- pfr_table$Freq / group_sizes

  }

  return(pfr_table)

}

#' @rdname pfr

