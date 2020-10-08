#' Proportion Correct Cued Recall
#'
#' This function computes the proportion of correct responses
#' per participant. Proportions can either be separated by
#' condition or collapsed across conditions. You will need to ensure
#' each trial is marked with a unique id to correspond to the answer
#' key.
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
#' @param key.trial a vector containing the trial numbers for each answer.
#' Note: If you input long data (i.e., repeating trial-answer responses),
#' we will take the unique combination of the responses. If a trial number
#' is repeated, you will receive an error. Key and key.trial can also be
#' a separate dataframe, depending on how your output data is formatted.
#' @param id a column name containing participant ID numbers from
#' the original dataframe
#' @param id.trial a column name containing the trial numbers
#' for the participant data from the original dataframe
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
#'
#' #This data contains cued recall test with responses and answers together.
#' #You can use a separate answer key, but this example will show you an
#' #embedded answer key. This example also shows how you can use different
#' #stimuli across participants (i.e., each person sees a randomly selected
#' #set of trials from a larger set).
#'
#' DF_test <- read.csv("data/cued_data.csv")
#'
#' scored_output <- prop_correct_free(responses = DF_test$response,
#'                                    key = DF_test$key,
#'                                    key.trial = DF_test$trial,
#'                                    id = DF_test$id,
#'                                    id.trial = DF_test$trial,
#'                                    cutoff = 1,
#'                                    flag = TRUE,
#'                                    group.by = DF_test$condition,
#'                                    group.by.names = "condition",
#'                                    other = c(rep("stuff", nrow(DF_test))),
#'                                    other.names = "fake_column")
#'
#'head(scored_output$DF_Scored)
#'
#'head(scored_output$DF_Participant)
#'
#'head(scored_output$DF_Group)
#'
prop_correct_free <- function(data, responses,
                              key, key.trial, id, id.trial,
                              cutoff = 0, flag = FALSE,
                              group.by = NULL){

  #create data from inputs ----

  #grab the input dataframe  and convert to our names
  DF <- as.data.frame(data)
  colnames(DF)[grepl(responses, colnames(DF))] <- "Responses"
  colnames(DF)[grepl(id, colnames(DF))] <- "Sub.ID"
  colnames(DF)[grepl(id.trial, colnames(DF))] <- "Trial.ID"

  #create the answer key
  #if the length > 1, assume it's a vector to merge together
  if (length(key) > 1){
    answer_key <- data.frame("Answer" = key, "Trial.ID" = key.trial)
  } else { #assume it is in the original dataframe
    answer_key <- data.frame("Answer" = data[ , key],
                             "Trial.ID" = data[ , key.trial])
  }

  #find unique keys
  answer_key <- unique(answer_key)

  #make sure no trial IDs are repeated because then bork
  dups <- duplicated(answer_key$Trial.ID)
  if(sum(dups) > 0){
    stop("You have duplicate trial ids for your answer key. Please check your data.")
  }

  #now merge key and data
  DF <- merge(DF, answer_key, by = "Trial.ID")

  #create the scored data ----

  #get all the distances
  lev_score <- mapply(adist, DF$Responses, DF$Answer)

  #find if they are less than the cutoff
  DF$Scored <- as.numeric(lev_score <= cutoff)

  #create participant summary ----
  k <- tapply(DF$Trial.ID, DF$Sub.ID, length)
  if(min(k) != max(k)){
    warning("The number of trials is not the same for every participant.
            We will use the max value of trials to calculate proportion
            correct. Check your data if this is not intended.")
  }
  k <- max(k)

  #create participant data frame ----
  DF_participant <- aggregate(DF$Scored, list(DF$Sub.ID), function(x){sum(x)/k})
  colnames(DF_participant) <- c("Sub.ID", "Proportion.Correct")

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

#' @rdname prop_correct_cued
