#' Proportion Correct for Sentences
#'
#' This function computes the proportion of correct sentence responses
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
#' @param token.split an optional argument that can be used to delineate
#' how to separate tokens. The default is a space after punctuation and
#' additional spacing is removed.
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
#' @keywords proportion correct, scoring, recall, sentences
#' @import stats
#' @import utils
#' @export
#' @examples
#'
#' #This data contains sentence recall test with responses and answers together.
#' #You can use a separate answer key, but this example will show you an
#' #embedded answer key. This example also shows how you can use different
#' #stimuli across participants (i.e., each person sees a randomly selected
#' #set of trials from a larger set).
#'
#' load("data/sentence_data.Rdata")
#'
#' scored_output <- prop_correct_sentence(data = sentence_data,
#'  responses = "Response",
#'  key = "Sentence",
#'  key.trial = "Trial.ID",
#'  id = "Sub.ID",
#'  id.trial = "Trial.ID",
#'  cutoff = 1,
#'  flag = TRUE,
#'  group.by = "Condition",
#'  token.split = " ")
#'
#' head(scored_output$DF_Scored)
#'
#' head(scored_output$DF_Participant)
#'
#' head(scored_output$DF_Group)
#'
prop_correct_sentence <- function(data, responses,
                                  key, key.trial, id, id.trial,
                                  cutoff = 0, flag = FALSE,
                                  group.by = NULL,
                                  token.split = " "){

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

  #lower case everything
  DF$Answer <- tolower(DF$Answer)
  DF$Responses <- tolower(DF$Responses)

  #remove punctuation
  DF$Answer <- gsub("[[:punct:]]", "", DF$Answer)
  DF$Responses <- gsub("[[:punct:]]", "", DF$Responses)

  #crunch white space
  DF$Answer <- gsub("\\s+", " ", DF$Answer)
  DF$Answer <- trimws(DF$Answer)
  DF$Responses <- gsub("\\s+", " ", DF$Responses)
  DF$Responses <- trimws(DF$Responses)

  #create blank columns
  DF$Proportion.Match <- NA
  DF$Shared.Items <- NA
  DF$Corrected.Items <- NA
  DF$Omitted.Items <- NA
  DF$Extra.Items <- NA

  #dear lord a loop is the best idea
  for (i in 1:nrow(DF)){

    #cheap tokenization
    answer.tokens <- unlist(strsplit(DF$Answer[i], split = token.split))
    response.tokens <- unlist(strsplit(DF$Responses[i], split = token.split))

    #figure out number of words in each
    shared <- c()
    omitted <- c()
    extras <- c()
    corrected <- c()
    shared <- intersect(answer.tokens, response.tokens)
    omitted <- setdiff(answer.tokens, response.tokens)
    extras <- setdiff(response.tokens, answer.tokens)
    omitted_final <- omitted
    extras_final <- extras

    #figure out if extra words are actually omitted words
    #need both to check this
    if (length(extras) > 0 & length(omitted) > 0){

      for (j in 1:length(extras)){

        lev_score <- adist(extras[j], omitted)
        names(lev_score) <- omitted

        #Find the minimum value for best match
        #Figure out if the min score is within the cut off
        if(min(lev_score) <= cutoff) {

          corrected <- c(corrected, extras[j])
          extras_final <- extras_final[!grepl(extras[j], extras_final)]
          omitted_final <- omitted_final[!grepl(attr(which.min(lev_score), "names"), omitted_final)]

        } #figure out min lev

      } #going through extra words

    } #only if extras is a thing

    if(length(shared) > 0){ DF$Shared.Items[i] <- paste(shared, collapse = " ") } else {
      DF$Shared.Items[i] <- NA
    }
    if(length(omitted_final) > 0){ DF$Omitted.Items[i] <- paste(omitted_final, collapse = " ") } else {
      DF$Omitted.Items[i] <- NA
    }
    if(length(corrected) > 0){ DF$Corrected.Items[i] <- paste(corrected, collapse = " ") } else {
      DF$Corrected.Items[i] <- NA
    }
    if(length(extras_final) > 0){ DF$Extra.Items[i] <- paste(extras_final, collapse = " ") } else {
      DF$Extra.Items[i] <- NA
    }

    DF$Proportion.Match[i] <- (length(shared) + length(corrected)) / length(answer.tokens)

  }

  #create participant summary ----
  k <- tapply(DF$Trial.ID, DF$Sub.ID, length)
  if(min(k) != max(k)){
    warning("The number of trials is not the same for every participant.
            This summary represents an average of the avaliable trials
            for each participant.")
  }

  #create participant data frame ----
  if (!is.null(group.by)){

    DF_participant <- aggregate(DF$Proportion.Match,
                                by = DF[ , c(group.by, "Sub.ID")],
                                mean)
    colnames(DF_participant) <- c(group.by, "Sub.ID", "Proportion.Correct")
  } else {

    DF_participant <- aggregate(DF$Proportion.Match,
                                list(DF$Sub.ID), mean)
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
    DF_group_person <- aggregate(DF$Proportion.Match,
                                 by = DF[ , c(group.by, "Sub.ID")],
                                 mean)
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

#' @rdname prop_correct_sentence
