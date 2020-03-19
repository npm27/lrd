#set up
subID = c(1, 1, 1, 2, 2, 2)
subID = as.character(subID)

Response = c("hom", "windsheld", "pepper",
             "homme", "windsheild", "pepper")
key = c("home", "windshield", "pepper",
        "home", "windshield", "pepper")

dat = data.frame(subID, key, Response)

#now score it!
library(lrd)

weighted = percent_match(dat$Response, key = dat$key,
              id = dat$subID, weight = TRUE,
              weight.by = .5)
