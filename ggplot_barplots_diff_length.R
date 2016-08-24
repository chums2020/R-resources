# table(df$x) and table(df$y) have different lengths
# table(df$x) and table(df$y) represents counts of each category
# instead, we want the 'percentage' of each category
# we want to create a barplot that plots both percentage tables

library(ggplot2)

# create table data, each element is the percentage of that category
tab1 <- table(df$x)*100/sum(table(df$x))
tab2 <- table(df$y)*100/sum(table(df$y))

df <- data.frame(group=rep(c("Estimated", "Actual"), times=c(length(tab1), length(tab2))), 
                 values=c(tab1, tab2), xval=as.numeric(c(names(tab1), names(tab2))))
ggplot(df,aes(xval,values,fill=group))+
  geom_bar(stat="identity",position="dodge")+ 
  xlab("Days") +
  ylab("%") +
  ggtitle("Waiting Time")
