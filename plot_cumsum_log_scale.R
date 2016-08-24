############## plot revenue (%) against purchase payment ############
############## add horizontal lines at 25%, 50%, 75% ################
############## log scale purchase amount (x axis) ###################

df <- data.frame(purchase_amount_per_transaction$TOTAL)
colnames(df) <- 'x'
df$x <- sort(df$x)
df$y <-cumsum(df$x)*100/sum(df$x)
df$DisplayName <- 0
df$DisplayName[min(which(df$y >50))] <- 'T'
df$DisplayName[min(which(df$y >25))] <- 'T'
df$DisplayName[min(which(df$y >75))] <- 'T'

plot(df$x, df$y, log="x" , 'l', col = 'blue',
     xlab='Purchase Payment', ylab='Cumulative revenue (%)', axes=FALSE)
abline(h= 25, lty = 2)
abline(h= 50, lty = 2)
abline(h= 75, lty = 2)
axis(side=2, at=seq(0, 100, by=10), cex.axis =0.7)
axis(side=1, at=c(1,10,100,1000,10000,100000,100000, 1000000), cex.axis=0.7, labels=format(c(1,10, 100,1000,10000,100000,100000, 1000000), big.mark=",", scientific=FALSE)) 
with(subset(df,DisplayName=="T"),text(x,y,format(x, big.mark=",", scientific=FALSE), cex =0.8, adj=c(0,1)))
