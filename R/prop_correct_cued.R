#' Proportion Correct Cued Recall
#'
#' This function computes the proportion of correct responses
#' per participant. Proportions can either be separated by
#' condition or collapsed across conditions. You will need to ensure
#' each trial is marked with a unique id to correspond to the answer
#' key.
#'
#' Note: This function returns mean values when used with non-binary data.
#'
#' @param responses a vector containing participant answers for each item
#' @param key a vector containing the scoring key
#' @param key.trial a vector containing the trial numbers for each answer.
#' Note: If you input long data (i.e., repeating trial-answer responses),
#' we will take the unique combination of the responses. If a trial number
#' is repeated, you will receive an error. Key and key.trial can also be
#' a separate dataframe, depending on how your output data is formatted.
#' @param id a vector containing participant ID numbers
#' @param id.trial a vector containing the trial numbers for the participant data
#' @param cutoff a numeric value that determines the criteria for
#' scoring (i.e., 0 = strictest, 5 = is most lenient). The scoring
#' criteria uses a Levenshtein distance measure to match participant
#' responses to the answer key.
#' @param flag a logical argument if you want to flag participant scores
#' that are outliers using z-scores away from the mean score for group
#' @param group.by an optional argument that can be used to group the
#' output by one condition column
#' @param group.by.names an optional argument that can be used to
#' name the grouping variables in your output
#' @param other an optional argument to combine other columns with the
#' output from this function.
#' @param other.names an optional argument that can be used to
#' name the other variables in your output
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
prop_correct_free <- function(responses, key, key.trial, id, id.trial,
                              cutoff = 0, flag = FALSE,
                              group.by = NULL,
                              group.by.names = NULL,
                              other = NULL,
                              other.names = NULL){

  #create data from inputs ----

  #create a dataframe of the data
  DF <- data.frame("Sub.ID" = id, "Responses" = responses, "Trial.ID" = id.trial)

  #create the answer key
  answer_key <- data.frame("Answer" = key, "Trial.ID" = key.trial)

  #find unique keys
  answer_key <- unique(answer_key)

  #make sure no trial IDs are repeated because then bork
  dups <- duplicated(answer_key$Trial.ID)
  if(sum(dups) > 0){
    stop("You have duplicate trial ids for your answer key. Please check your data.")
  }

  #now merge key and data
  DF <- merge(DF, answer_key, by = "Trial.ID")

  #merge back other data
  if (!is.null(other)){

    #convert to data frame
    other <- as.data.frame(other)

    #add in column names
    if (!is.null(other.names)){
      if(ncol(other) != length(other.names)){
        stop("Your other columns and other.names arguments do not have the
             same number of items. Please check your code.")
      }
      colnames(other) <- other.names
    } else {
      colnames(other) <- paste0("other", c(1:ncol(other)))
    }

    #make sure long enough
    if(nrow(other) != length(id)) {
      stop("Your other variables are not the same length as the participant
           IDs. Please check your data.")
    }

    other <- cbind(other,id)
    other_unique <- unique(other)
    colnames(other)[ncol(other)] <- colnames(other_unique)[ncol(other)] <- "Sub.ID"
    DF <- merge(DF, other_unique, by = "Sub.ID")
  }

  #merge back grouping data
  if (!is.null(group.by)){

    #convert to data frame
    group.by <- as.data.frame(group.by)

    #add in column names
    if (!is.null(group.by.names)){
      if(ncol(group.by) != length(group.by.names)){
        stop("Your group.by columns and group.by.names arguments do not have the
             same number of items. Please check your code.")
      }
      colnames(group.by) <- group.by.names
    } else {
      colnames(group.by) <- paste0("group.by", c(1:ncol(group.by)))
    }

    #make sure long enough
    if(nrow(group.by) != length(id)) {
      stop("Your group.by variables are not the same length as the participant
           IDs. Please check your data.")
    }

    group.by <- cbind(group.by,id)
    group.by_unique <- unique(group.by)
    colnames(group.by)[ncol(group.by)] <- colnames(group.by_unique)[ncol(group.by)] <- "Sub.ID"
    DF <- merge(DF, group.by_unique, by = "Sub.ID")
  }

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

  if (!is.null(other)){
    DF_participant <- merge(DF_participant, other_unique, by = "Sub.ID")
  }

  if (!is.null(group.by)){
    DF_participant <- merge(DF_participant, group.by_unique, by = "Sub.ID")
  }

  #if they want to flag participants ----
  if (flag) {

    #flag by group
    if (!is.null(group.by)){

      DF_participant$Z.Score.Group <- ave(DF_participant$Proportion.Correct,
                                          DF_participant[ , colnames(group.by)[1:ncol(group.by)-1]],
                                          FUN = scale)

    }
      DF_participant$Z.Score.Participant <- scale(DF_participant$Proportion.Correct)

  }

  #group summary ----
  #if they want a grouping variable
  if (!is.null(group.by)){

    #summarize participant scores by group
    DF_group_person <- aggregate(DF$Scored,
                                 list(DF[ , colnames(group.by)[1:ncol(group.by)-1]],
                                      DF$Sub.ID),
                          function(x){sum(x)/k})
    colnames(DF_group_person) <- c(colnames(group.by)[1:ncol(group.by)-1],
                                   "Sub.ID", "Mean")
    DF_group <- aggregate(DF_group_person$Mean,
                          list(DF_group_person[ , colnames(group.by)[1:ncol(group.by)-1]]), mean)
    DF_group$SD <- aggregate(DF_group_person$Mean,
                             list(DF_group_person[ , colnames(group.by)[1:ncol(group.by)-1]]), sd)$x
    DF_group$N <- aggregate(DF_group_person$Mean,
                            list(DF_group_person[ , colnames(group.by)[1:ncol(group.by)-1]]), length)$x
    DF_group <- rbind(DF_group,
                      c("overall", mean(DF_group_person$Mean),
                        sd(DF_group_person$Mean), length(DF_group_person$Mean)))
    colnames(DF_group) <- c(colnames(group.by)[1:ncol(group.by)-1], "Mean", "SD", "N")

    return(list(DF_Scored = DF,
                DF_Participant = DF_participant,
                DF_Group = DF_group))

  } else {
    return(list(DF_Scored = DF,
                DF_Participant = DF_participant))
  }

}

#' @rdname prop_correct_cued
