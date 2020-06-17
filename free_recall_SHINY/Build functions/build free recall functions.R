####Set up####
dat = read.csv("sample data.csv")
key = read.csv("test key.csv")

library(RecordLinkage)
library(lrd)

match(dat$Response, key$Key) #This does something, but what are these values?
#Okay, I think this is giving me the position of the response items within the key. Basket is marked as 3 because it's the third item in the key
#Could be useful, but this isn't going to account for typos.

#converts things to phonetic codes
soundex(dat$Response)
soundex(key$Key)

#Computes the levenshtein distance between two words
levenshteinSim(dat$Response[1], key$Key[3])

#Could use this to match the participant response to the correct item in the key
#Would need to find a way though to control for memory intrusions/participants recalling non-presented items or guessing

#But essentially, have a function that computes the distance between the response and each target to find the closest match
#Would need to return this match (ideally in a dataframe column next to the corresponding participant response)
#If no match, it could return na for that cell. Then na's could be used to flag intrusions/non-presented items

#Could a loop accomplish what I want to do? Let's try!
key = key$Key
response = dat$Response
id = dat$Sub.ID

df = cbind(id, response)
df = data.frame(df)
scores = data.frame() #Make an empty dataframe for storage

for (i in response) { #i = individual participant responses


  for (k in 1:nrow(df)) {

    scores2 = levenshteinDist(i, key) #Get a matrix of levenshtein distances. Could do any type of lexical overlap measure here.
                                      ##Really just trying to find a way to match up responses with key since subjects aren't going to recall things in the same order

    }


  scores = rbind(scores, scores2)

  #Adds things back to the dataframe

  colnames(scores)[1:length(scores)] = key[1:length(key)]

#Now that I have the key arranged: Pull the key item that is closest to each response. I could then use lrd's percent match

  output = c() #Make an empty vector for storage

  scores2 = apply(scores, 1, min) #Compute the minimum score (would need max depending on the overlap measure being used)

  for (n in 1:nrow(scores)){

    for (m in 1:length(scores)){

      if (scores2[n] <= 2){ #find all the minimums that meet the threshold

        if (scores[n,m] <= 2){ #The sign would need to be flipped if doing a percent match measure. The two is a placeholder value and ideally would be user specified

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

##Function would end here

####Now I can use the output to compute each participant's proportion of correct responses, proportion of intrusions, and probability of an item being recalled first
