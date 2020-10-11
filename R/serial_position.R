#' Serial Position Calculator
#'
#' This function calculates the proportion correct of each item in the
#' serial position curve. Data should include the participant's answers
#' in long format (use arrange_data() in this package for help), the answer
#' key of the items in order, and a column that denotes the order a
#' participant listed each item. The function will then calculate
#' the items remembered within a window of 1 before or 1 after the
#' tested position. The first and last positions must be answered in the
#' correct place.
#'
#' This output can then be used to create a serial position curve visualizations,
#' and an example can be found in our manuscript/vignettes.
#'
#' Important: The code is written assuming group.by variables are
#' between subjects for an individual recall list.
#' If repeated measures are used (i.e., there are
#' multiple lists completed by each participant or multiple list versions),
#' you should use this function several times, once on each list/answer key.
#'
#' @param data a dataframe of the scored free recall that you would
#' like to calculate - use prop_correct_free() for best formatting.
#' @param position a column name in the dataframe that contains
#' answered position of each response in quotes (i.e., "column")
#' @param answer a column name of the answer given for that position
#' in the original dataframe.
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
#' \item{DF_Serial}{A dataframe of the proportion correct for each
#' tested position by any optional grouping variables included.}
#'
#' @keywords proportion correct, scoring, free recall, serial position
#' @import stats
#' @export
#' @examples
#'
#' data(free_data)
#' data(answer_key_free2)
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
#' serial_output <- serial_position(data = scored_output$DF_Scored,
#'  key = answer_key_free2$Answer_Key,
#'  position = "position",
#'  scored = "Scored",
#'  answer = "Answer",
#'  group.by = "Version")
#'
#'  head(serial_output)
#'
serial_position <- function(data, position, answer,
                            key, scored, group.by){

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
  DF <- merge(DF, key, by = "Answer")

  #calculate the number of times within window
  DF$Answered.Position <- as.numeric(DF$Answered.Position)
  DF$Tested.Position <- as.numeric(DF$Tested.Position)
  DF$Lag <- DF$Tested.Position - DF$Answered.Position
  DF$Serial.Yes <- NA

  #For the first and last item, you only count if it was first or last

  #first position, scored yes, within 0 lag
  DF$Serial.Yes[DF$Tested.Position == 1 & DF$Scored == 1 & DF$Lag == 0] <- 1

  #last position, same as above
  DF$Serial.Yes[DF$Tested.Position == length(key$Answer) &
                  DF$Scored == 1 & DF$Lag == 0] <- 1

  #all others can be + or - 1 away and scored yes
  DF$Serial.Yes[DF$Tested.Position > 1 & DF$Tested.Position < length(key$Answer) &
                  DF$Scored == 1 & abs(DF$Lag) == 1] <- 1

  #now aggregate by tested.position and any groups
  if (!is.null(group.by)){

    DF_serial <- aggregate(DF$Serial.Yes,
                           by = DF[ , c(group.by, "Tested.Position")],
                           sum, na.rm = T)
    colnames(DF_serial)[ncol(DF_serial)] <- "Sum"

    DF_unique <- unique(DF[ , c("Sub.ID", group.by)])
    group_sizes <- as.data.frame(table(DF_unique[ , group.by]))
    group_sizes$group_code <- paste(group_sizes[ , -ncol(group_sizes)])

    group_code <- paste(DF_serial[ , group.by])
    DF_serial$Proportion.Correct <- NA

    for (group in group_code){
      DF_serial$Proportion.Correct[group_code == group] <- DF_serial$Sum[group_code == group] / group_sizes$Freq[group_sizes$group_code == group]
      DF_serial$SE[group_code == group] <- sqrt((DF_serial$Proportion.Correct[group_code == group]*(1-DF_serial$Proportion.Correct[group_code == group])/
                                                   group_sizes$Freq[group_sizes$group_code == group]))
    }

  } else {

    DF_serial <- aggregate(DF$Serial.Yes,
                           by = list(DF[ , "Tested.Position"]),
                           sum, na.rm = T)
    colnames(DF_serial) <- c("Tested.Position", "Sum")
    group_sizes <- length(unique(DF$Sub.ID))
    DF_serial$Proportion.Correct <- DF_serial$Sum / group_sizes
    DF_serial$SE <- sqrt((DF_serial$Proportion.Correct*(1-DF_serial$Proportion.Correct)/
                            group_sizes))

    }

  return(DF_serial)

}

#' @rdname serial_position
