#' Proportion Correct Free Recall
#'
#' This function computes the proportion of correct responses
#' per participant. Proportions can either be separated by
#' condition or collapsed across conditions.
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
#' @param responses a column name in the dataframe that containts
#' the participant answers for each item in quotes (i.e., "column")
#' @param key a vector containing the scoring key or data column name.
#' This column does not have to be included in the original dataframe.
#' @param id a column name containing participant ID numbers from
#' the original dataframe
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
#' \item{DF_group}{A dataframe of the summary scores by any optional
#' grouping variables, along with overall total proportion correct
#' scoring.}
#'
#' @keywords proportion correct, scoring, free recall
#' @export
#' @examples
#' DF_test <- read.csv("data/wide_data.csv")
#' DF_answer <- read.csv("data/answer_key_free.csv")
#'
#' DF_long <- arrange_data(responses = DF_test$Response,
#'                         sep = ",",
#'                         id = DF_test$Sub.ID,
#'                         other = DF_test$Disease.Condition,
#'                         other.names = "Disease.Condition")
#'
#' scored_output <- prop_correct_free(data = DF_long,
#'                                    responses = "response",
#'                                    key = DF_answer$Answer_Key,
#'                                    id = "Sub.ID",
#'                                    cutoff = 1,
#'                                    flag = TRUE,
#'                                    group.by = "Disease.Condition")
#'
#' head(scored_output$DF_Scored)
#'
#' head(scored_output$DF_Participant)
#'
#' head(scored_output$DF_Group)
#'
prop_correct_free <- function(data,
                         responses, key, id,
                         cutoff = 0, flag = FALSE,
                         group.by = NULL){

  #create data from inputs ----

  #grab the input dataframe  and convert to our names
  DF <- as.data.frame(data)
  colnames(DF)[grepl(responses, colnames(DF))] <- "Responses"
  colnames(DF)[grepl(id, colnames(DF))] <- "Sub.ID"

  #create the scored data ----

  #create a scoring key, score each response once
  answer_key <- data.frame("Responses" = as.character(),
                           "Answer" = as.character())

  #no need to check the same word twice
  key <- unique(key)

  #find the key-response pairs
  for (i in unique(DF$Responses)) {

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
  colnames(answer_key) <- c("Responses", "Answer")

  #with that answer key, score the data
  DF <- merge(DF, answer_key, by = "Responses")
  DF$Scored <- 1 - as.numeric(is.na(DF$Answer))

  #create participant summary ----
  k <- length(key)

  #create participant data frame ----
  if (!is.null(group.by)){

      DF_participant <- aggregate(DF$Scored,
                                  by = DF[ , c(group.by, "Sub.ID")],
                                  function(x){sum(x)/k})
      colnames(DF_participant) <- c(group.by, "Sub.ID", "Proportion.Correct")
      } else {

        DF_participant <- aggregate(DF$Scored, list(DF$Sub.ID), function(x){sum(x)/k})
        colnames(DF_participant) <- c("Sub.ID", "Proportion.Correct")
  }

  #add back in other columns that are one to one
  other.columns <- setdiff(colnames(DF),
                           c("Responses", "Sub.ID", "Answer", "Scored",
                             colnames(DF_participant)))
  for (col in other.columns){
    DF_temp <- unique(DF[ , c("Sub.ID", col)])
    if (sum(duplicated(DF_temp$Sub.ID)) == 0){
      DF_participant <- merge(DF_participant, DF_temp, by = "Sub.ID")
    }
  }

  #if they want to flag participants ----
  if (flag) {

    #flag by group
    if (!is.null(group.by)){

      DF_participant$Z.Score.Group <- ave(DF_participant$Proportion.Correct,
                                          DF_participant[ , group.by],
                                          FUN = scale)

    }
      DF_participant$Z.Score.Participant <- scale(DF_participant$Proportion.Correct)

  }

  #group summary ----
  #if they want a grouping variable
  if (!is.null(group.by)){

    #summarize participant scores by group
    DF_group_person <- aggregate(DF$Scored,
                                 by = DF[ , c(group.by, "Sub.ID")],
                                 FUN = function(x){sum(x)/k})
    colnames(DF_group_person) <- c(group.by,"Sub.ID", "Mean")

    #why does aggregate do this
    #if one variable by has to be a list
    #if more than one, no list allowed
    if (length(group.by) > 1){
      DF_group <- aggregate(DF_group_person$Mean,
                            by = DF_group_person[ , group.by], mean)
      DF_group$SD <- aggregate(DF_group_person$Mean,
                               by = DF_group_person[ , group.by], sd)$x
      DF_group$N <- aggregate(DF_group_person$Mean,
                              by = DF_group_person[ , group.by], length)$x
    } else {
      DF_group <- aggregate(DF_group_person$Mean,
                            by = list(DF_group_person[ , group.by]), mean)
      DF_group$SD <- aggregate(DF_group_person$Mean,
                               by = list(DF_group_person[ , group.by]), sd)$x
      DF_group$N <- aggregate(DF_group_person$Mean,
                              by = list(DF_group_person[ , group.by]), length)$x
    }

    colnames(DF_group) <- c(group.by, "Mean", "SD", "N")

    return(list(DF_Scored = DF,
                DF_Participant = DF_participant,
                DF_Group = DF_group))

  } else {
    return(list(DF_Scored = DF,
                DF_Participant = DF_participant))
  }

}

#' @rdname prop_correct_free
