####Lrd Sentence processing####
#Load some libraries
library(dplyr) #Can't remember if I ended up using this package...
library(koRpus)
library(koRpus.lang.en)
library(rapportools)
#library(quanteda)
source("percent_match_s.r")

##Okay, I need this to work at the dataframe level...
#But, let's focus on something a smaller for now....
#s1 = "This is a sentence."
#s2 = "This is also a sentencee."

##First thing I should do is figure out how to split each into their respective tokens
#S1 is going to be the cue (so the value stored in the key)
#S2 will represent the participant's response.

##Basically, step 1 break things into tokens
##Step 2: Figure out the percent match
  ##2a: Number of words
  ##2b: Spelling errors
##Step 3: Figure out words omitted and return them
##Step 4: Figure out extra words and return them

#Read in some data
dat = read.csv("sentences example.csv")

##For the function, inputs will need to be:
#participant responses (x)
#the answer key (y)
#the subject number (id)
#and a percent match cutoff for the spelling stuff (z)

##The function below seems to work pretty well.
#But I still need to figure out how to suppress those 50 or more warnings coming from the is.empty stuff

sentence_match = function(x, key = y, id = z, cutoff = q){

  s1 = key
  s2 = x

  ##Lower case everything
  s1 = tolower(s1)
  s2 = tolower(s2)

  ##remove punctuation
  s1 = gsub("[[:punct:]]", "",s1)
  s2 = gsub("[[:punct:]]", "", s2)

  ##Now get stuff set up
  input = data.frame(id, s1, s2)

  #Create blank containers for the output
  s_match = c()
  shared3 = c()
  omitted3 = c()
  extras3 = c()
  corrected = c()

  ##Loop through subject
  for (i in unique(input$id)) {

    temp = subset(input,
                  input$id == i)
    #print(temp)

    #Loop through each subject's responses
    for (j in temp$s1){

      temp2 = subset(temp,
                    temp$s1 == j)

      #Step 1: Get tokens
      s1.tokens = tokenize(temp2$s1, format = "obj", lang = "en")
      s1.tokens = s1.tokens[ , 2]

      s2.tokens = tokenize(temp2$s2, format = "obj", lang = "en")
      s2.tokens = s2.tokens[ , 2]

      ##Step 2a: Figure out number of words in each
      shared = intersect(s1.tokens, s2.tokens) #This works... assuming that participants always spell things correctly
      omitted = setdiff(s1.tokens, s2.tokens) #Everything omitted
      extras = setdiff(s2.tokens, s1.tokens)

      ##step 2b: Spelling
      #Can use omitted as an answer key. Basically just check each extra against each omitted and find the thing with the highest match.
      #Can probably solve my problems with a loop...
      #Make some empty things for storage

      temp_i2 = c()
      temp_j2 = c()
      temp_b2 = c()

      #No extra words or omitted words -- sentences match perfectly

      if (is.empty(omitted) == TRUE & is.empty(extras) == TRUE){

        temp_i2 = c(temp_i2, " ")
        temp_j2 = c(temp_j2, " ")
        temp_b2 = c(temp_b2, 1)

        #print(temp_i)

        output = cbind(temp_i2, temp_j2, temp_b2)

        output = data.frame(output)

        colnames(output)[1:3] = c("Key", "Response", "Percent_Match")
        #print(output)

      #No omitted words but extra words are included

      } else if (is.empty(omitted) == TRUE & is.empty(extras) == FALSE){

        temp_i3 = c()
        temp_j3 = c()
        temp_b3 = c()

        temp_i3 = c(temp_i3, " ")

        for (q in extras){

          temp_j3 = c(temp_j3, q)

        }

        b = length(s1.tokens) / length(s2.tokens)

        temp_b3 = c(temp_b3, b)

        output = cbind(temp_i3, temp_j3, temp_b3)

        output = data.frame(output)

        colnames(output)[1:3] = c("Key", "Response", "Percent_Match")

        ##No extra words but some words are omitted
      } else if (is.empty(omitted) == FALSE & is.empty(extras) == TRUE) {

        temp_i4 = c()
        temp_j4 = c()
        temp_b4 = c()

        temp_j4 = c(temp_j4, " ")

        for (xx in omitted){

          temp_i4 = c(temp_i4, xx)

        }

        b = length(s2.tokens) / length(s1.tokens)

        temp_b4 = c(temp_b4, b)

        output = cbind(temp_i4, temp_j4, temp_b4)

        output = data.frame(output)

        colnames(output)[1:3] = c("Key", "Response", "Percent_Match")

      #Other situations where sentences don't match perfectly
      } else {

        temp_i = c()
        temp_j = c()
        temp_b = c()

        for (k in omitted){

          for (p in extras){

            bb = percent_match.s(k, p)
            #print(k)
            #print(p)
            #print(b)

            temp_b = c(temp_b, bb)
            temp_i = c(temp_i, k)
            temp_j = c(temp_j, p)

          }

          #temp_b = data.frame(temp_b)
          #temp_i = data.frame(temp_i)
          #temp_j = data.frame(temp_j)

          #colnames(temp_b)[1] = "Percent_Match"
          #colnames(temp_i)[1] = "Key"
          #colnames(temp_j)[1] = "Response"

          output_sp = cbind(temp_i, temp_j, temp_b)


          #print(output)

        }

        output_sp = data.frame(output_sp)

        colnames(output_sp)[1:3] = c("Key", "Response", "Percent_Match")

        ##Now fix the spelling errors
        ##output8 is all the spelling errors I want fixed
        output8 = subset(output_sp,
                         output_sp$Percent_Match >= cutoff)

        ##Output 9 is all of the extra words participants typed
        output9 = subset(output_sp,
                         output_sp$Percent_Match < cutoff)

        ##If output 8 has stuff in it (basically if tokens in the participant response need correcting):
        if (is.empty(output8) == FALSE){

          ##The spelling replacement worked great but only when only one word needed replacing...
          ##So need to loop through row number

          for (n in 1:nrow(output8)){

            #Fix the spelling errors
            temp2$s2 = gsub(output8[n, 2], output8[n, 1], temp2$s2)

          }

          s2.tokens = tokenize(temp2$s2, format = "obj", lang = "en")
          s2.tokens = s2.tokens[ , 2]

        ##If there's nothing that's a close enough match

        } else if (is.empty(output8) == TRUE){

          temp2$s2 = temp2$s2
          s2.tokens = s2.tokens

        }

      }

      ###Start slapping together the output file.
      ##Basically what I'd like is input, a sentence match column, an extra word column, and an omitted word column

      #Get final percentage of word match
      shared2 = intersect(s1.tokens, s2.tokens)
      s_match2 = length(shared2) / max(c(length(s1.tokens), length(s2.tokens)))

      omitted2 = toString(setdiff(s1.tokens, s2.tokens)) #Final omitted
      extras2 = toString(setdiff(s2.tokens, s1.tokens))

      shared3 = c(shared3, shared2)
      s_match = c(s_match, s_match2)
      omitted3 = c(omitted3, omitted2)
      extras3 = c(extras3, extras2)

      ##Also think it would be good to return the corrected responses
      corrected2 = toString(s2.tokens)
      corrected2 = gsub(",", "", corrected2)

      corrected = c(corrected, corrected2)

    }

  }

  final = data.frame(id, s1, s2, corrected, s_match, omitted3, extras3)

  colnames(final)[1:7] = c("ID", "Key", "Response", "Corrected_Response", "Percent_Match", "Omitted_Items", "Extra_Items")

  final

}

final_output = sentence_match(dat$Response, key = dat$Sentence, id = dat$Sub.ID, cutoff = .75)
