#' Score free recall data
#'
#' Use this function to score free data based on percent match
#'
#' @param x a dataframe object containing the scored data
#' @param cutoff a value used to specify the scoring cutoff. Must be a value between 0 and 1.
#' @return a .csv file containing the scored output
#' @export

score.recall.f = function(x, key = y, id = z, cutoff = g){

  df = cbind(id, x)
  df = data.frame(df)
  scores = data.frame() #Make an empty dataframe for storage

  for (i in x) { #i = individual participant responses


    for (k in 1:nrow(df)) {

      scores2 = levenshteinDist(i, key) #Get a matrix of levenshtein distances. Could do any type of lexical overlap measure here.
      ##Really just trying to find a way to match up responses with key since subjects aren't going to recall things in the same order

    }


    scores = rbind(scores, scores2)

    #Adds things back to the dataframe

    colnames(scores)[1:length(scores)] = key[1:length(key)]

    #Now that I have the key arranged: Pull the key item that is closest to each response.

    output = c() #Make an empty vector for storage

    scores2 = apply(scores, 1, min) #Compute the minimum score (would need max depending on the overlap measure being used)

    for (n in 1:nrow(scores)){

      for (m in 1:length(scores)){

        if (scores2[n] <= cutoff){ #find all the minimums that meet the threshold

          if (scores[n,m] <= cutoff){ #The sign would need to be flipped if doing a percent match measure.

            output2 = colnames(scores)[m] #Not sure what to do for ties yet. colnames are the items in the key. So pull the key item matching the response

          }

        }

        else {

          output2 = NA #If there are no close matches, return an NA. NA responses could be saved to an intrusion list or something

        }

      }

      output = c(output, output2)

    }

  }

  ##combine the output with the sub IDs and participant responses
  df2 = cbind(df, output)

  ###Score data####
  df2$Scored = is.na(df2$output)
  df2$Scored = as.numeric(df2$Scored)

  df2$Scored = (df2$Scored - 1) * -1 #Invert 1's and 0's (0 should equal incorrect, 1 = correct)

  colnames(df2)[2] = "Response"

  return(df2)

}
