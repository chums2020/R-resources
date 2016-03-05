library(psych)
library(Hmisc)
library(tempdisagg)
library(zoo)
library(corrplot)
library(xts)

data1 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/taiwan pc unit sold 10 yr.csv', sep=',',header=TRUE)
y <- data.frame(data1[,2])
n <- dim(y)[1]

data2 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/gdp.csv', sep=',',header=TRUE)
data3 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/indus.csv', sep=',',header=TRUE)
data4 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/taiex.csv', sep=',',header=TRUE)
data5 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/cpi.csv', sep=',',header=TRUE)
data6 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/export.csv', sep=',',header=TRUE)
data7 <- read.table('C:/Users/Claire/Desktop/WebCrawl/data/ExchangeRate.csv', sep=',',header=TRUE)

low <- which(data2[,1]== '2004Q1')
high <- which(data2[,1]== '2015Q4')
gdp_q <- data.frame(data2[low:high,2])

low <- which(data3[,1]== '2004M01')
high <- which(data3[,1]== '2015M12')
indus <- data.frame(data3[low:high,2])

low <- which(data4[,1]== '2004/1/15')
high <- which(data4[,1]== '2015/12/1')
taiex_price <- data.frame(data4[low:high,5])
taiex_volume <- data.frame(data4[low:high,6])

###############################FIX##########################################
oil <- data.frame(data6[])
###############################FIX##########################################

low <- which(data5[,1]== '2004M01')
high <- which(data5[,1]== '2015M12')
cpi <- data.frame(data5[low:high,2])

low <- which(data6[,1]== '01(2003/2004)')
high <- which(data6[,1]== '12(2014/2015)')
export <- data.frame(data6[low:high,3])

low <- which(data7[,1]== '2004M01')
high <- which(data7[,1]== '2015M12')
rate <- data.frame(data7[low:high,2])

t_lag <- data.frame(seq(as.Date("2005/1/15"), as.Date("2015/12/15"), "month"))
t <-data.frame(seq(as.Date("2004/1/15"), as.Date("2014/12/15"), "month"))

describe(y[,1])

#plot y
plot(t_lag , y[,1]/1000, type = "o", xaxt="n", yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Passenger vehicle unit sold in Taiwan", xlab=list("Month",cex=1),ylab=list("Unit (in thousand)",cex=1))
axis(2, at=seq(5,65,length = 13))
axis.Date(1, at = seq(as.Date("2005/1/31"), max(as.Date("2015/12/31")), "years"))
abline(v = seq(as.Date("2005/1/31"), max(as.Date("2015/12/31")), "years"), col = "green")


#disaggregate quarterly data into monthly data
#GDP interpolation
gdp.q <- ts(gdp_q, frequency = 4, start = c(2004, 1))
export.m <- ts(export, frequency = 12, start = c(2004, 1))
gdp_int <- td(gdp.q ~ 0 + export.m, method = "denton-cholette")
gdp <- predict(gdp_int)
gdp_int2 <- td(gdp.q ~ 1, to = "monthly", method = "denton-cholette")
gdp2 <- predict(gdp_int2)

dev.off()
ts.plot(gdp,gdp2, xlab="", ylab="",col=c("blue","red"))
title(main = "Monthly GDP after Interpolation", xlab=list("Month",cex=1),ylab=list("Million NT$, at Current Price",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))
legend("bottomright",c("with exports data","without exports data"),col=c("blue","red"), lty=c(1,1))

gdp <- data.frame(gdp=as.matrix(gdp))
gdp2 <- data.frame(gdp2=as.matrix(gdp2))

###############################MAYBE INCLUDE##########################################
par(mfrow = c(2,1))
plot(t,gdp[,1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Monthly GDP after Interpolation", xlab=list("Month",cex=0.8),ylab=list("Million N.T.$, at Current Price",cex=0.8))
axis.Date(1, at = seq(as.Date("2005/1/31"), max(as.Date("2015/12/31")), "years"))
axis(2, at=seq(5,80,length = 3))

q=as.Date(gdp.q)
plot(q,gdp_q[,1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Quarterly GDP", xlab=list("Quarter",cex=0.8),ylab=list("Million N.T.$, at Current Price",cex=0.8))
axis.Date(1, at = seq(as.Date("2005/1/31"), max(as.Date("2015/12/31")), "years"))
axis(2, at=seq(100,170,length = 3),cex=0.5)

##
dev.off()
z <- na.approx(cbind(as.zoo(gdp), as.zoo(gdp.q)))
time(z) <- as.yearmon(time(z))
plot(z, screen = 1, col = 2:3, xaxt="n",  xlab="", ylab="")
title(main = "GDP in Taiwan", xlab=list("Month",cex=1),ylab=list("Million N.T.$, at Current Price",cex=1))
axis.Date(1, at = seq(as.Date("2005/1/31"), max(as.Date("2015/12/31")), "years"))
###############################MAYBE INCLUDE##########################################


#plot X
par(mfrow = c(3,2))

#gdp
plot(t,gdp[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "GDP", xlab=list("Month",cex=1),ylab=list("Million NT$",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#industrial index
plot(t, indus[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Industrial Production Index", xlab=list("Month",cex=1),ylab=list("index",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#CPI
plot(t, cpi[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Consumer Price Index", xlab=list("Month",cex=1),ylab=list("index",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#TAIEX price
plot(t, taiex_price[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "TAIEX Close Price", xlab=list("Month",cex=1),ylab=list("Price",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#TAIEX volume
plot(t, taiex_volume[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "TAIEX Volume", xlab=list("Month",cex=1),ylab=list("Volume",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#exchange rate
plot(t, rate[1:nrow(y),1], type = "l", xaxt="n",  yaxt="n",  xlab="", ylab="", col = "blue")
title(main = "Exchange Rate", xlab=list("Month",cex=1),ylab=list("NT$/US$",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#Payroll
plot()
title(main = "GDP", xlab=list("Month",cex=1),ylab=list("Million N.T.$, at Current Price",cex=1))
axis.Date(1, at = seq(as.Date("2004/1/31"), max(as.Date("2014/12/31")), "years"))

#Oil price
plot()


X <- cbind(y[,1], gdp[1:nrow(y),1], indus[1:nrow(y),1], cpi[1:nrow(y),1], taiex_price[1:nrow(y),1],taiex_volume[1:nrow(y),1],rate[1:nrow(y),1])
colnames(X) <-c("Sales","GDP","Indus. Index","CPI", "TAIEX P", "TAIEX V", "Ex Rate")


#Correlation matrix
dev.off()
corr <- cor(X)
colnames(corr) <-c("Sales","GDP","Indus. Index","CPI", "TAIEX P", "TAIEX Vol", "Ex Rate")
rownames(corr) <-c("Sales","GDP","Indus. Index","CPI", "TAIEX P", "TAIEX Vol", "Ex Rate")
corrplot(corr, type = "upper", order="original", tl.col ="black",tl.srt=0,tl.cex =0.8,addCoef.col="black",method="color",diag=FALSE)


#Add monthly dummies
m1<- data.frame(ifelse(format(t, format="%m")=='01', 1,0))
m2<- data.frame(ifelse(format(t, format="%m")=='02', 1,0))
m3<- data.frame(ifelse(format(t, format="%m")=='03', 1,0))
m4<- data.frame(ifelse(format(t, format="%m")=='04', 1,0))
m5<- data.frame(ifelse(format(t, format="%m")=='05', 1,0))
m6<- data.frame(ifelse(format(t, format="%m")=='06', 1,0))
m7<- data.frame(ifelse(format(t, format="%m")=='07', 1,0))
m8<- data.frame(ifelse(format(t, format="%m")=='08', 1,0))
m9<- data.frame(ifelse(format(t, format="%m")=='09', 1,0))
m10<- data.frame(ifelse(format(t, format="%m")=='10', 1,0))
m11<- data.frame(ifelse(format(t, format="%m")=='11', 1,0))

#data from 2005M1 - 2014M12
X<-as.matrix(X[,-1]) #exclude the sales data
size <- nrow(y)-12
X <-cbind(X[13:nrow(X),],y[1:size,],m1[13:nrow(X),],m2[13:nrow(X),],m3[13:nrow(X),],m4[13:nrow(X),],m5[13:nrow(X),],m6[13:nrow(X),],m7[13:nrow(X),],m8[13:nrow(X),],m9[13:nrow(X),],m10[13:nrow(X),],m11[13:nrow(X),])

#closed-form solution (Ridge regression)
#w = (XX'+lambda^2 I)^(-1)Xy'
#Check Andrew Ng ML HW5

y<-as.matrix(y)

#define a function for ridge regression
Ridge_Reg <- function(X,y,lambda){
  d <- dim(X)[2]
  I <- diag(d)
  w <-(solve(t(X)%*%X+(lambda^2)*I))%*%t(X)%*%y
  return(w)
}

Ridge_Reg(X,y,lambda=1)

#Auto regressive term(s)
cor(y[13:nrow(y),],y[1:size,])
cor(y[14:nrow(y),],y[1:119,])
cor(y[15:nrow(y),],y[1:118,])
cor(y[16:nrow(y),],y[1:117,])
cor(y[17:nrow(y),],y[1:116,])

Y_ar<-cbind(y[16:132,],y[4:120,],y[3:119,],y[2:118,],y[1:117,])
dev.off()
corr <- cor(Y_ar)
colnames(corr) <-c("Sales_t+12","Sales_t","Sales_(t-1)","Sales_(t-2)", "Sales_(t-3)")
rownames(corr) <-c("Sales_t+12","Sales_t","Sales_(t-1)","Sales_(t-2)", "Sales_(t-3)")
corrplot(corr, type = "upper", order="original", tl.col ="black",tl.srt=0,tl.cex =0.8,addCoef.col="black",method="color",diag=FALSE)

#define a loss function for ridge regression
Loss <- function(X, y, lambda, w, N){
  loss <- (1/N)*t(X%*%w-y)%*%(X%*%w-y)
  return(loss)
}

#K-fold Cross Validation
K <- 90 #minimum data length for fitting a model
step <- 12 
range <- seq(0,3,0.20)
loss <- matrix(NA,nrow=120-K,ncol=1)
l_mean <- matrix(NA,nrow=length(range),ncol=1)
l_var <- matrix(NA,nrow=length(range),ncol=1)
for (i in 1:length(range)){
    for(j in 1:30) {   #120-K
        end<-j+89       #K-1
        X_j <- as.matrix(X[j:end,])
        y_j <- as.matrix(y[j:end,])
        w <- Ridge_Reg(X_j,y_j, range[i])
        start1 <- j+90     #K
        start2 <- j+102    #K+12
        loss[j,] <- Loss(X[start1:nrow(X),],y[start2:nrow(y),],range[i], w, N=nrow(X)-start1+1)
    }
    l_mean[i,] <- mean(loss)
}

#print the error w.r.t. lambda
dev.off()
plot(range, l_mean,yaxt="n",  xlab="lambda", ylab="Mean Square Error", col = "blue")
title(main = "Validation Curve")
abline(v=range[which(l_mean == min(l_mean))], col = "red")


which(l_mean == min(l_mean))
Optimal_lambda <- range[which(l_mean == min(l_mean))]
print(Optimal_lambda)

NRMSD <- sqrt(min(l_mean))/(max(y)-min(y))
print(NRMSD)

#estimate the model with optimal lambda
w <- Ridge_Reg(X,y[13:nrow(y),],Optimal_lambda)

#define the prediction model
y_pred <- function(X,w,lambda){
  y <- X%*%w
  return(y)
}
predict <- y_pred(X,w,Optimal_lambda)

#plot the prediction & actual series
y_pred.m <- ts(predict, frequency = 12, start = c(2006, 1))
y.m <- ts(y[13:nrow(y),], frequency = 12, start = c(2006, 1))

dev.off()
ts.plot(y_pred.m ,y.m, xlab="", ylab="",col=c("blue","red"))
title(main = "Actual v.s. Predicted series by Month", xlab=list("Month",cex=1),ylab=list("Unit",cex=1))
axis.Date(1, at = seq(as.Date("2006/1/31"), max(as.Date("2015/12/31")), "years"))
legend("topright",c("Predicted sales","Actual sales"),col=c("blue","red"), lty=c(1,1))

#Annual sales prediction
predict_y <- aggregate(predict, list(format(t_lag[13:nrow(t_lag),], format="%Y")), sum)
y_y <- aggregate(y[13:nrow(t_lag),], list(format(t_lag[13:nrow(t_lag),], format="%Y")), sum)

y_pred.y <- ts(predict_y[,2] , frequency = 1, start = c(2006, 1))
y.y <- ts(y_y[,2], frequency = 1, start = c(2006, 1))

dev.off()
ts.plot(y_pred.y ,y.y, xlab="", ylab="",col=c("blue","red"))
title(main = "Actual v.s. Predicted series by Year", xlab=list("Year",cex=1),ylab=list("Unit",cex=1))
axis.Date(1, at = seq(as.Date("2006/1/31"), max(as.Date("2015/12/31")), "years"))
legend("bottomright",c("Predicted sales","Actual sales"),col=c("blue","red"), lty=c(1,1))


###############################predict 2016 annual sales##########################################
start <- nrow(gdp)-11
end <- nrow(gdp)
T <- data.frame(seq(as.Date("2015/1/15"), as.Date("2015/12/15"), "month"))
m1<- data.frame(ifelse(format(T, format="%m")=='01', 1,0))
m2<- data.frame(ifelse(format(T, format="%m")=='02', 1,0))
m3<- data.frame(ifelse(format(T, format="%m")=='03', 1,0))
m4<- data.frame(ifelse(format(T, format="%m")=='04', 1,0))
m5<- data.frame(ifelse(format(T, format="%m")=='05', 1,0))
m6<- data.frame(ifelse(format(T, format="%m")=='06', 1,0))
m7<- data.frame(ifelse(format(T, format="%m")=='07', 1,0))
m8<- data.frame(ifelse(format(T, format="%m")=='08', 1,0))
m9<- data.frame(ifelse(format(T, format="%m")=='09', 1,0))
m10<- data.frame(ifelse(format(T, format="%m")=='10', 1,0))
m11<- data.frame(ifelse(format(T, format="%m")=='11', 1,0))


X_pred <- as.matrix(cbind(gdp[start:end,], indus[start:end,], cpi[start:end,], taiex_price[start:end,],taiex_volume[121:132,],rate[start:end,], y[121:132,],m1[1:12,],m2[1:12,],m3[1:12,],m4[1:12,],m5[1:12,],m6[1:12,],m7[1:12,],m8[1:12,],m9[1:12,],m10[1:12,],m11[1:12,]))
X_pred <-as.matrix(X_pred[1:nrow(X_pred),])
predict <- y_pred(X_pred,w,Optimal_lambda)
print(predict)
sum(predict)

dev.off()
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
plot(T , predict[,1], type = "o",  xlab="", ylab="", col = "blue")
title(main = "Projected Passenger Vehicle Sales in 2016", xlab=list("Month",cex=1),ylab=list("Unit",cex=1))
#axis(2, at=seq(5,65,length = 13))
axis.Date(1, at = seq(as.Date("2016/1/31"), max(as.Date("2016/12/31")), "month"))


#testing past sales series
dev.off()
plot(y[1:12,],type="o")
plot(y[13:24,],type="o")
plot(y[25:36,],type="o")
plot(y[37:48,],type="o")
plot(y[49:60,],type="o")
plot(y[61:72,],type="o")
