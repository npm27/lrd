#' Proportion Correct Free Recall
#'
#' This function computes the proportion of correct responses
#' per participant. Proportions can either be separated by
#' condition or collapsed across conditions.
#'
#' Note: This function returns mean values when used with non-binary data.
#'
#' @param responses a vector containing participant answers for each item
#' @param key a vector containing the scoring key
#' @param id a vector containing participant ID numbers
#' @param cutoff a numeric value that determines the criteria for
#' scoring (i.e., 0 = strictest, 5 = is most lenient). The scoring
#' criteria uses a Levenshtein distance measure to match participant
#' responses to the answer key.
#' @param flag a logical argument if you want to flag participant scores
#' that are outliers using z-scores away from the mean score for group
#' @param group.by an optional argument that can be used to group the
#' output by one condition column
#' @param other an optional argument to combine other columns with the
#' output from this function.
#'
#' @return
#' \item{Sub.ID}{The participant id number}
#' \item{response}{The participant response}
#' \item{position}{The position number of the response listed}
#' \item{other}{Any additional columns included}
#'
#' @keywords data, arrange, wide, long
#' @export
#' @examples
#' DF_test <- read.csv("data/wide_data.csv")
#' DF_answer <- read.csv("data/answer_key_free.csv")
#'
#' DF_long <- arrange_data(responses = DF_test$Response,
#'                         sep = ",",
#'                         id = DF_test$Sub.ID,
#'                         other = DF_test$Disease.Condition)
#'
#' #change other name to real column name
#' colnames(DF_long)[4] <- "Disease.Condition"
#'
#' scored_output <- prop_correct_free(responses = DF_long$response,
#'                                    key = DF_answer$Answer_Key,
#'                                    id = DF_long$Sub.ID,
#'                                    cutoff = 1,
#'                                  flag = TRUE,
#'                                    group.by = DF_long$Disease.Condition)
#'
#' head(scored_output$DF_Scored)
#'
#' head(scored_output$DF_Participant)
#'
#' head(scored_output$DF_Group)
#'
prop_correct_free <- function(responses, key, id,
                         cutoff = 0, flag = FALSE,
                         group.by = NULL, other = NULL){

  #score the data ----

  #create a dataframe of the data
  DF <- data.frame(id, responses)

  #create a scoring key, score each response once
  answer_key <- data.frame(responses = as.character(),
                           answer = as.character())
  for (i in unique(responses)) {

    #Get the leven score
    lev_score <- adist(i, key)
    names(lev_score) <- key

    #Find the minimum value for best match
    #Figure out if the min score is within the cut off
    if(min(lev_score) <= cutoff) {

      #put that into the answer key
      answer_key <- rbind(answer_key,
                          c(i, attr(which.min(lev_score), "names")))

    } else {

      answer_key <- rbind(answer_key,
                          c(i, NA))

    }

  }

  #fix answer key
  colnames(answer_key) <- c("responses", "answer")

  #with that answer key, score the data
  DF <- merge(DF, answer_key, by = "responses")
  DF$Scored <- 1 - as.numeric(is.na(DF$answer))

  #merge back other data
  if (!is.null(other)){

    other <- as.data.frame(other)

    if(nrow(other) != length(id)) {
      stop("Your other variables are not the same length as the participant
           IDs. Please check your data.")
    }

    other <- cbind(other,id)
    other_unique <- unique(other)
    DF <- merge(DF, other_unique, by = "id")
  }

  #create participant summary ----
  k <- length(key)

  DF_participant <- aggregate(DF$Scored, list(DF$id), function(x){sum(x)/k})
  colnames(DF_participant) <- c("id", "Proportion.Correct")

  if (!is.null(other)){
    DF_participant <- merge(DF_participant, other_unique, by = "id")
  }

  #if they want to flag participants
  if (flag) {

    #flag by group
    if (!is.null(group.by)){
      group.by <- as.data.frame(group.by)
      if(nrow(group.by) != length(id)) {
        stop("Your grouping variables are not the same length as the participant
           IDs. Please check your data.")
      }
      group.by_unique <- unique(cbind(group.by, id))
      DF_participant <- merge(DF_participant, group.by_unique, by = "id")

      DF_participant$Z.Score.Group <- ave(DF_participant$Proportion.Correct,
                                    DF_participant$group.by,
                                    FUN = scale)

    }
      DF_participant$Z.Score.Participant <- scale(DF_participant$Proportion.Correct)

  }

  #group summary ----
  colnames(DF) <- c("Responses", "Sub.ID", "Answer.Key", "Scored")
  colnames(DF_participant)[c(1,3)] <- c("Sub.ID", "Grouping.Variable")

  #if they want a grouping variable
  if (!is.null(group.by)){

    #summarize participant scores by group
    DF_group_person <- aggregate(DF$Scored, list(group.by$group.by, DF$id),
                          function(x){sum(x)/k})
    DF_group <- aggregate(DF_group_person$x, list(DF_group_person$Group.1), mean)
    DF_group$SD <- aggregate(DF_group_person$x, list(DF_group_person$Group.1), sd)$x
    DF_group$N <- aggregate(DF_group_person$x, list(DF_group_person$Group.1), length)$x
    DF_group <- rbind(DF_group,
                      c("overall", mean(DF_group_person$x),
                        sd(DF_group_person$x), length(DF_group_person$x)))
    colnames(DF_group) <- c("Group.1", "Mean", "SD", "N")

    return(list(DF_Scored = DF,
                DF_Participant = DF_participant,
                DF_Group = DF_group))

  } else {
    return(list(DF_Scored = DF,
                DF_Participant = DF_participant))
  }

}

#' @rdname prop_correct_free
