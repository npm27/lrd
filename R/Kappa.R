#' Cohen's Kappa
#'
#' This function returns Cohen's Kappa k for two raters. Kappa indicates
#' the inter-rater reliability for categorical items. High scores (closer
#' to one) indicate agreement between raters, while low scores (closer
#' to zero) indicate low agreement between raters. Negative numbers indicate
#' they don't agree at all!
#'
#' Note: All missing values will be ignored. This function calculates kappa
#' for 0 and 1 scoring. If you pass categorical variables, the
#' function will return a percent match score between these values.
#'
#' @param rater1 Rater 1 scores or categorical listings
#' @param rater2 Rater 2 scores or categorical listings
#' @param confidence Confidence interval proportion for the kappa interval
#' estimate. You must supply a value between 0 and 1.
#' @return
#' \item{p_agree}{Percent agreement between raters}
#' \item{kappa}{Cohen's kappa for yes/no matching}
#' \item{se_kappa}{Standard error for kappa wherein standard error
#' is the square root of: (agree \* (1-agree)) / (N \* (1 - random
#' agreement)^2)}
#' \item{kappa_LL}{Lower limit for the confidence interval of kappa}
#' \item{kappa_UL}{Upper limit for the confidence interval of kappa}
#'
#' @keywords rating, kappa, reliability
#' @export
#' @examples
#'
#' #This dataset includes two raters who wrote the word listed by
#' the participant and rated if the word was correct in the recall
#' experiment.
#'
#' load("data/rater_data.Rdata")
#'
#' #Consider normalizing the text if raters used different styles
#' #Calculate percent match for categorical answers
#' kappa(rater_data$rater1_word, rater_data$rater2_word)
#'
#' kappa(rater_data$rater1_score, rater_data$rater2_score)
#'

kappa <- function(rater1, rater2, confidence = .95) {

  #if rater1 is a string
  if(is.character(rater1)|is.factor(rater1)){

    #if rater2 is also a string
    if(is.character(rater2)|is.factor(rater2)){

      rater1 <- as.character(rater1)
      rater2 <- as.character(rater2)

      match <- rater1 == rater2
      p_agree <- sum(match) / length(match) * 100
      return(p_agree)
    }
  }

  #if both numeric
  if(is.numeric(rater1) & is.numeric(rater2)){

    temp <- na.omit(data.frame(rater1, rater2))
    match_table <- table(temp$rater1, temp$rater2)

    #exit if bigger than a two by two
    if (length(match_table) > 4) {
      stop("You have more than 0 and 1 values, please check your data.")
    }

    #exit if smaller than a two by two
    if (length(match_table) < 4) {
      stop("Your raters either had perfect agreement or one rater did
           not use both categories. Please check your data. ")
      }

    a <- match_table[4] #yes,yes
    b <- match_table[2] #yes,no
    c <- match_table[3] #no,yes
    d <- match_table[1] #no,no

    p_agree <- (a+d)/(a+b+c+d)
    p_yes <- ((a+b)/(a+b+c+d)) * ((a+c)/(a+b+c+d))
    p_no <- ((c+d)/(a+b+c+d)) * ((b+d)/(a+b+c+d))
    p_error <- p_yes + p_no
    kappa <- (p_agree - p_error) / (1 - p_error)
    se_kappa <- sqrt( (p_agree*(1-p_agree)) / (nrow(temp)*(1-p_error)^2) )

    if (confidence >= 1 | confidence <= 0){
      stop("Confidence values must be between 0 and 1.")
    } else {

        kappa_LL <- kappa - qnorm((1-confidence)/2, lower.tail = FALSE) * se_kappa
        kappa_UL <- kappa + qnorm((1-confidence)/2, lower.tail = FALSE) * se_kappa

    }

    return(list(
      p_agree = p_agree*100, #percent agree
      kappa = kappa,
      se_kappa = se_kappa,
      kappa_LL = kappa_LL,
      kappa_UL = kappa_UL
      ))


  } else {
      stop ("Please check both variables are characters/factors or numeric.")
    }

}

#' @rdname kappa
