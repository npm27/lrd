#' Conditional Response Probability
#'
#' This function calculates the conditional response
#' probability of each lag position. Participants' lag
#' between subsequent named items is tallied and then
#' divided by the possible combination of subsequent lags
#' given their response pattern.
#'
#' This output can then be used to create a CRP visualizations,
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
#' load("data/free_data.RData")
#' load("data/answer_key_free2.RData")
#'
#' free_data <- subset(free_data,
#'  List_Type == "Cat_Recall_L1")
#'
#' DF_long <- arrange_data(data = free_data,
#'  responses = "Response",
#'  sep = " ",
#'  id = "Username")
#'
#' scored_output <- prop_correct_free( data = DF_long,
#'  responses = "response",
#'  key = answer_key_free2$Answer_Key,
#'  id = "Sub.ID",
#'  cutoff = 1,
#'  flag = TRUE,
#'  group.by = "Version")
#'
#' crp_output <- crp(data = scored_output$DF_Scored,
#'  position = "position",
#'  answer = "Answer",
#'  id = "Sub.ID",
#'  key = answer_key_free2$Answer_Key,
#'  scored = "Scored")
#'
#'  head(crp_output)
#'
crp <- function(data, position, answer, id,
                key, scored){

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
  DF$Lag <- DF$Tested.Position - DF$Answered.Position

  number_spots <- 1:max(DF$Tested.Position)
  DF_final <- NULL

  #for each participant calculate the possible lags
  for (i in DF$Sub.ID){

    temp_part <- subset(DF,
                        Sub.ID == i)
    temp_part <- temp_part[order(temp_part$Answered.Position), ]

    participant_lags <- diff(temp_part$Tested.Position)
    possible_lags <- c()

    if (nrow(temp_part) > 1){
      #participant lags
      for (j in 1:nrow(temp_part)){

        #take up to the current answered position
        current_used <- temp_part$Tested.Position[1:j]
        answers_left <- setdiff(number_spots, current_used)
        current_spot <- temp_part$Tested.Position[j]

        possible_lags <- c(possible_lags, answers_left - current_spot)

      } #answers loop

      table_part_lags <- as.data.frame(table(participant_lags))
      table_possible_lags <- as.data.frame(table(possible_lags))
      colnames(table_possible_lags) <- c("participant_lags", "Possible.Freq")
      table_part_lags <- merge(table_part_lags,
                               table_possible_lags,
                               by = "participant_lags")

      table_part_lags$Sub.ID <- i

      #add back in other columns that are one to one
      other.columns <- setdiff(colnames(DF),
                               c("Responses", "Sub.ID", "Answer",
                                 "Scored", "Answered.Position",
                                 "Tested.Position", "Lag",
                                 colnames(table_part_lags)))
      for (col in other.columns){
        DF_temp <- unique(DF[ , c("Sub.ID", col)])
        if (sum(duplicated(DF_temp$Sub.ID)) == 0){
          table_part_lags <- merge(table_part_lags, DF_temp, by = "Sub.ID")
        }
      } #for other columns

      if (is.null(DF_final)){
        DF_final <- table_part_lags
      } else {
        DF_final <- rbind(DF_final, table_part_lags)
      }

    } #must have more than one row

  } #participant loop

  DF_final$CRP <- DF_final$Freq / DF_final$Possible.Freq
  return(DF_final)

}

#' @rdname crp
