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

levenshteinSim(dat$Response[1], key$Key[3])

#Could a loop accomplish what I want to do?
key = key$Key
response = dat$Response
id = dat$Sub.ID

df = cbind(id, response)
df = data.frame(df)
scores = data.frame() #Make an empty dataframe for storage

for (i in response) { #i = individual strings in the response

  for (j in id){

    output = subset(df, df$id == j) #isolate each participant's responses

    for (k in 1:nrow(df)) {

      scores2 = levenshteinDist(i, key) #Get a matrix of levenshtein distances. Could do any type of lexical overlap measure here.
                                      ##Really just trying to find a way to match up responses with key since subjects aren't going to recall things in the same order

    }

  }

  scores = rbind(scores, scores2)

  #Adds things back to the dataframe

  colnames(scores)[1:length(scores)] = key[1:length(key)]

#Now that I have the key arranged: Pull the key item that is closest to each response. I could then use lrd's percent match

  output = c()

  scores2 = apply(scores, 1, min)

  for (n in 1:nrow(scores)){

    for (m in 1:length(scores)){

      if (scores2[n] <= 2){

        if (scores[n,m] <= 2){ #The sign would need to be flipped if doing a percent match measure. The two is a placeholder value and ideally would be user specified

          output2 = colnames(scores)[m] #Not sure what to do for ties yet. colnames are the items in the key. So pull the key matching the response

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

####Now I can use the output to compute each participant's proportion of correct responses, proportion of intrusions, and probability of an item being recalled first
