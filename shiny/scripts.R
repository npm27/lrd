# Scripts for shiny lrd app ------------------------------------------------

# ggplot cleanup
cleanup <- theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

# App functions -----------------------------------------------------------

## Percent Match
Percent_Match <- function(x, key = y, id = z, weight = FALSE, weight.by = NULL,
                         other = NULL, cutoff = qqq){

  nb = is.null(other)

  xx = weight.by

  input = data.frame(id, x, key)

  input$id = as.character(input$id)
  input$x = as.character(input$x)
  input$key = as.character(input$key)

  input$length_x = nchar(input$x)
  input$length_key = nchar(input$key)
  input$diff = input$length_x - input$length_key

  sub1 = subset(input,
                input$diff >= 1)

  sub2 = subset(input,
                input$diff == 0)

  sub3 = subset(input,
                input$diff < 0)

  sub1 = sub1[ , -c(4:6)]
  sub2 = sub2[ , -c(4:6)]
  sub3 = sub3[ , -c(4:6)]

  char.x1 = vector(mode = "character", length = nrow(sub1))
  char.x2 = vector(mode = "character", length = nrow(sub2))
  char.x3 = vector(mode = "character", length = nrow(sub3))

  char.y1 = vector(mode = "character", length = nrow(sub1))
  char.y2 = vector(mode = "character", length = nrow(sub2))
  char.y3 = vector(mode = "character", length = nrow(sub3))

  percent_match1 = vector(mode = "character", length = nrow(sub1))
  percent_match2 = vector(mode = "character", length = nrow(sub2))
  percent_match3 = vector(mode = "character", length = nrow(sub3))

  for (i in 1:nrow(sub1)) {

    char.x1[i] = strsplit(sub1$x[i], "")
    char.y1[i] = strsplit(sub1$key[i], "")

  }

  for (k in 1:nrow(sub1)) {

    c = char.x1[[k]]
    d = char.y1[[k]]

    percent_match1[k] = length(vintersect(c, d)) /
      max(length((c)), length((d)))

    sub1$percent_match = percent_match1

  }

  for (h in 1:nrow(sub2)) {

    char.x2[h] = strsplit(sub2$x[h], "")
    char.y2[h] = strsplit(sub2$key[h], "")

  }

  for (j in 1:nrow(sub2)) {

    e = char.x2[[j]]
    f = char.y2[[j]]

    percent_match2[j] = length(vintersect(e, f)) /
      max(length((e)), length((f)))

    sub2$percent_match = percent_match2

  }

  for (q in 1:nrow(sub3)) {

    char.x3[q] = strsplit(sub3$x[q], "")
    char.y3[q] = strsplit(sub3$key[q], "")

  }

  for (p in 1:nrow(sub3)) {

    r = char.x3[[p]]
    s = char.y3[[p]]

    percent_match3[p] = length(vintersect(r, s)) /
      max(length((r)), length((s)))

    sub3$percent_match = percent_match3

  }

  output = rbind(sub1, sub2, sub3)
  colnames(output)[2] = "Response"
  output = output[order(as.numeric(rownames(output))),,drop = FALSE]


  if (nb == FALSE){

    output$Scored = as.numeric(output$percent_match >= cutoff)
    other = data.frame(other)
    output3 = cbind(output, other)
    print(output3)
  }

  else if (nb == TRUE){

    output$Scored = as.numeric(output$percent_match >= cutoff)
    print(output)

  }

}

## Describe condition
describe.condition <- function(x, group.by = y, id = z){

  input = data.frame(id, x, group.by)

  #Get participant level means
  tap1 = tapply(input$x, list(input$id, input$group.by), mean, na.rm = T)
  tap1 = data.frame(tap1)

  #Put everything in a dataframe
  names = row.names(tap1)
  temp = cbind(names, tap1)

  #Now get all the condition level descriptives
  Mean = apply(temp[2:length(temp)], 2, mean) #mean
  SD = apply(temp[2:length(temp)], 2, sd)#sd
  SE = SD / sqrt(length(unique(temp$names))) #se
  CI.95 = SE * 1.96 #95% CI
  Upper = Mean + CI.95 #upper Limit
  Lower = Mean - CI.95 #Lower Limit

  #Get everything in a dataframe for output
  names = data.frame(c("Mean", "SD", "SE", "95% CI", "Upper", "Lower"))

  labels = colnames(temp[ , -1])

  Mean = t(data.frame(Mean))
  SD = t(data.frame(SD))
  SE = t(data.frame(SE))
  CI.95 = t(data.frame(CI.95))
  Upper = t(data.frame(Upper))
  Lower = t(data.frame(Lower))

  temp2 = rbind(Mean, SD, SE, CI.95, Upper, Lower)
  temp2 = cbind(names, temp2)
  colnames(temp2)[1] = "names"

  condition.means = rbind(temp, temp2)

  colnames(condition.means)[1] = " "

  print(condition.means[ c((nrow(condition.means) - 5):nrow(condition.means)), ], row.names = F)

}

## Proportion Correct
prop.correct <- function(x, group.by = NULL, id = z, flag = FALSE){

  a = is.null(group.by)

  if (a == TRUE & flag == FALSE) {

    input = data.frame(id, x)

    #Get participant level means
    tap1 = tapply(input$x, input$id, mean, na.rm = T)
    tap1 = data.frame(tap1)

    colnames(tap1)[1] = "Value"

    tap1$z = scale(tap1$Value)

    print(tap1)

  }

  else if (a == TRUE & flag == TRUE) {

    input = data.frame(id, x)

    #Get participant level means
    tap1 = tapply(input$x, input$id, mean, na.rm = T)
    tap1 = data.frame(tap1)

    colnames(tap1)[1] = "Value"

    tap1$z = scale(tap1$Value)

    tap1$Flagged = rep(" ")
    tap1$Flagged[tap1$z >= 3] = "*"
    tap1$Flagged[tap1$z <= -3] = "*"

    colnames(tap1)[3] = " "

    print(tap1)

  }

  else if (a == FALSE & flag == FALSE) {

    input = data.frame(id, x, group.by)

    #Get participant level means grouped by condition
    tap2 = tapply(input$x, list(input$id, input$group.by), mean, na.rm = T)
    tap2 = data.frame(tap2)

    print(tap2)

  }

}


