####Write a function that can generate a basic bar chart####
library(ggplot2)

#Erin's code for making the plots look nicer
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

proportions = prop.correct(dat$Recall, id = dat$Subject)

names = data.frame(row.names(proportions))
colnames(names)[1] = "ID"

temp = cbind(names, proportions)

bar = ggplot(temp, aes(ID, Value))
bar + 
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  cleanup +
  xlab("ID") +
  ylab("Proportion")
