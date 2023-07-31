#SP500 returns analysis
SP500short<-read.csv("SP500.csv") #data
SP500<-read.csv("SP500long.csv") #data from Yahoo Finance
na.omit(as.numeric(SP500[,2])) #we eliminate blank returns (Holidays,etc.)
complete.cases(SP500) #shows if all lines are complete
ret <-(SP500[,5]-SP500[,2]) / (SP500[,2]) #return = closing - opening / opening
retshort <-(SP500short[,5]-SP500short[,2]) / (SP500short[,2]) 
which.max(ret) ; which.min(ret)
max(ret); min(ret); mean(ret); median(ret)
library(moments)
sd(ret); kurtosis(ret); skewness(ret)
which.max(retshort) ; which.min(retshort)
max(retshort); min(retshort); mean(retshort); median(retshort)
library(moments)
sd(retshort); kurtosis(retshort); skewness(retshort)
kurtosis(retshort)
dateret <- as.Date(as.character(SP500[,1]),format="%Y-%m-%d") #for time series format
dateretshort <- as.Date(as.character(SP500short[,1]),format="%Y-%m-%d") #for time series format

Sys.setlocale(category = "LC_ALL", locale = "english")
#plots SP500index evolution
png(filename="SP500_0.png",  res=300, width = 2400, height = 1500)
plot(dateret, SP500[,5], xlab="Date", ylab="Closing price", type="l")
while (!is.null(dev.list()))  dev.off()

png(filename="SP500short_0.png",  res=300, width = 2400, height = 1500)
plot(dateretshort, SP500short[, 5], xlab="Date", ylab="Closing price", type="l")
while (!is.null(dev.list()))  dev.off()

#return analysis
#plots return
png(filename="SP500_1.png",  res=300, width = 2400, height = 1500)
plot(dateret,ret, xlab = "Date", ylab="Return", type="l")
dev.off()

png(filename="SP500short_1.png",  res=300, width = 2400, height = 1500)
plot(dateretshort, retshort, xlab="Date", ylab="Closing price", type="l")
while (!is.null(dev.list()))  dev.off()

#return histogram
png(filename="SP500_2.png",  res=300, width = 2600, height = 1500)
par(mfrow=c(1,2))  
hist(ret, breaks=20, xlim = c(-0.04, 0.03), xlab = "", ylab="", main =
     "Distribution of S&P 500 returns 2010-2021")
hist(retshort, breaks=20, xlim = c(-0.04, 0.03), xlab = "", ylab="", main =
       "Distribution of S&P 500 returns 2020-2021")
dev.off()

#ACF and PACF for returns and absolute returns (long sample)
retabs <- abs(ret)
png(filename="SP500_3.png",  res=300, width = 2400, height = 1500)
par(mfrow=c(1,2))  
acf(ret, lag.max=50, main="ACF Yt", ylim=c(-0.4, 0.4))
acf(retabs, lag.max=50, main="ACF |Yt|", ylim=c(-0.4, 0.4))
dev.off()

#ACF and PACF for returns and absolute returns (short sample)
retshortabs <- abs(retshort)
png(filename="SP500_3short.png",  res=300, width = 2400, height = 1500)
par(mfrow=c(1,2))  
acf(retshort, lag.max=50, main="ACF Yt", ylim=c(-0.4, 0.4))
acf(retshortabs, lag.max=50, main="ACF |Yt|", ylim=c(-0.4, 0.4))
dev.off()

#ACF^2 plotting
retsquared<-ret*ret
retsquaredshort<-retshort*retshort

png(filename="SP500_6.png",  res=300, width = 2400, height = 1500)
par(mfrow=c(1,2))  
acf(retsquared, lag.max=50, main="ACF Yt^2", ylab="ACF Long Sample")
acf(retsquaredshort, lag.max=50, main="ACF Yt^2", ylab="ACF Short Sample")
dev.off()


#Trying to fit linear regression y = b0 + b1*t + error
fitreg<-lm(ret~dateret)
fitSP500<-lm(SP500[, 5]~dateret)
summary(fitSP500)
plot(fitSP500)
summary(fitreg)

png(filename="SP500_4.png",  res=300, width = 2400, height = 1500)
plot(dateret,ret, xlab = "Date", ylab="Return", type="l")
abline(lm(ret ~ dateret), col="blue")
dev.off()

plot(dateret, SP500[,5], xlab = "Date", ylab="Return", type="l")
abline(lm(SP500[, 5] ~ dateret))


library(dynlm)
r<-numeric(250)
SP500mean <- dynlm(ret~1, data=ret)
summary(SP500mean)
SP500meanresid <- ts(resid(SP500mean))
SP500meansq<- ts(resid(SP500mean)^2)
SP500arch1<-dynlm(SP500meansq~ L(SP500meansq))
SP500arch2<-dynlm(SP500meansq~ L(SP500meansq) + L( L(SP500meansq)) )
SP500arch3<-dynlm(SP500meansq ~ L(SP500meansq) + L( L(SP500meansq)) + L(L(L(SP500meansq))))
summary(SP500arch1)
summary(SP500arch2)
summary(SP500arch3)

#Testing for ARCH effects
library(FinTS)
SP500.archTest1 <- ArchTest(ret, lags=1, demean=TRUE)
SP500.archTest1
SP500.archTest2 <- ArchTest(ret, lags=2, demean=TRUE)
SP500.archTest2
SP500.archTest3 <- ArchTest(ret, lags = 3, demean = TRUE)
SP500.archTest3

library(FinTS)
SP500.archTest1 <- ArchTest(retshort, lags=1, demean=TRUE)
SP500.archTest1
SP500.archTest2 <- ArchTest(retshort, lags=2, demean=TRUE)
SP500.archTest2
SP500.archTest3 <- ArchTest(retshort, lags = 3, demean = TRUE)
SP500.archTest3


#Fitting an ARCH(2)
library(fGarch)
fita2= garchFit(~garch(2, 0), data=ret)
summary(fita2)
fita2short = garchFit(~garch(2, 0), data=retshort)
summary(fita2short)

#Fitting a GARCH(1,1)
library(fGarch)
fitg = garchFit(~garch(1, 1), data=ret)
summary(fitg)

quest <-coef(fitg)[1] + residuals(fitg) #gives us the original ret


#Predictions volatility GARCH(1,1)
# We create two cond var vectors
condvarGARCH1<-numeric(length(ret) + 5)
condvarGARCH1true<-numeric(length(ret) + 5)
sigma<-coef(fitg)[2] /(1-coef(fitg)[3]-coef(fitg)[4]) #uncond var.
condvarGARCH1[1] <-sigma
condvarGARCH1true[1] <-sigma

for(i in 2:length(ret))
{
  condvarGARCH1[i] <-coef(fitg)[2] + coef(fitg)[3]*retsquared[i-1] + coef(fitg)[4]*condvarGARCH1[i]
  condvarGARCH1true[i]<-coef(fitg)[2] + coef(fitg)[3]*retsquared[i-1] + coef(fitg)[4]*condvarGARCH1true[i]
}

#Prediction for next 5 values
diffsigma <- condvarGARCH1[length(ret)] - sigma
for(i in 1:5){
  sum1 <- (coef(fitg)[3] + coef(fitg)[4])
  l<-1
  while(l<i){
    print(l)
    sum1<- sum1*(coef(fitg)[3] + coef(fitg)[4]) 
    l<- (l+1)
  }
  
  condvarGARCH1[length(ret)+i] <- sigma + sum1*diffsigma
}
condvarGARCH1[2770]
sigma
tail(condvarGARCH1true)
retaheadsq <-retahead*retahead
retaheadsq[1]
lengthret<-length(ret)
for(i in (lengthret+1):(lengthret+5)){
  print(i)
  condvarGARCH1true[i] <- coef(fitg)[2] + coef(fitg)[3]*retaheadsq[-lengthret+i] + coef(fitg)[4]*condvarGARCH1true[i-1]
}

tail(condvarGARCH1true)

SP500ahead<-read.csv("SP500ahead2.csv") #data from Yahoo Finance
retahead <- (SP500ahead[, 5]- SP500ahead[, 2]) / SP500ahead[, 2]
dateretahead <- as.Date(as.character(SP500ahead[,1]),format="%Y-%m-%d") #for time series format

volatilitydates<-c(dateret[2756:2770], dateretahead[1:5])
length(volatilitydates)


Sys.setlocale(category = "LC_ALL", locale = "english")
png(filename="SP500_5.png",  res=300, width = 2400, height = 1500)
plot(volatilitydates, tail(sqrt(condvarGARCH1), 20), type="l" , ylab="sigma_t", xlab="Date", col="blue")
lines(volatilitydates,tail(sqrt(condvarGARCH1true), 20), type="l", col="black")
dev.off()


# EEEEEEEND OF CODE



#Prediction ARCH(2)
condvarARCH2 <-numeric(2775)
condvarARCH2[1]<-coef(fita2)[2] /(1-coef(fita2)[3]-coef(fita2)[4])
for(i in 2:2770){
  condvarARCH2[i]<- (coef(fita2)[2] + coef(fita2)[3]*retsquared[i-1] + coef(fita2)[4]*condvarARCH2[i-1])
  
}
head(condvarARCH2)
tail(condvarARCH2)


SP500ahead<-read.csv("SP500ahead2.csv") #data from Yahoo Finance
retahead <- (SP500ahead[, 5]- SP500ahead[, 2]) / SP500ahead[, 2]
dateretahead <- as.Date(as.character(SP500ahead[,1]),format="%Y-%m-%d") #for time series format

retARCH <-numeric(5)
for(l in 1:5){ 
  condvarARCH2[2770+l] <- coef(fita2)[2] + coef(fita2)[3]*ret[2771-l]*ret[2771-l]+ 
    coef(fita2)[4]*ret[252-l]*ret[252-l]
  retARCH[l] <-coef(fita2)[2]
  for(i in 1:2){
    if(l-i <= 0){
      retARCH[l] <- retARCH[l]+ coef(fita2)[2+i]*ret[2771-i]*ret[2771-i] 
      
    } else {
      retARCH[l] <- retARCH[l] + coef(fita2)[2+i]*condvarARCH2[2770+l-i]
    }
    
  }  
}
sqrt(retARCH)



#Prediction GARCH(1,1)
ret2771g<- coef(fitg)[2] + coef(fitg)[3]*retsquared[2770] + coef(fitg)[4]*retsquared[2769]
sqrt(ret2771g)
tail(ret)

plot(sqrt(condvarARCH2), type="l")


#prediction step by step
for(i in 1:5){
  
  condvarGARCH1[2770+i] <- coef(fitg)[2] + (coef(fitg)[3]+ coef(fitg)[4])*condvarGARCH1[2770+i-1]
}

volatilitydates<-c(dateret[2756:2770], dateretahead[1:5])
length(volatilitydates)




head(retahead)

plot(dateret, volatility(fita2, type="sigma"), type="l")
volatility(fita2)[251]
ret[252]
ret[249]
retahead <- (SP500ahead[, 5]- SP500ahead[, 2]) / SP500ahead[, 2]
retahead
coef(fita2)
retARCH <-numeric(5)
for(l in 1:5){ 
  condvarARCH2[252+l] <- coef(fita2)[2] + coef(fita2)[3]*ret[253-l]*ret[253-l]+ 
  coef(fita2)[4]*ret[252-l]*ret[252-l]
  retARCH[l] <-coef(fita2)[2]
  for(i in 1:2){
    if(l-i <= 0){
      retARCH[l] <- retARCH[l]+ coef(fita2)[2+i]*ret[253-i]*ret[253-i] 
      
    } else {
    retARCH[l] <- retARCH[l] + coef(fita2)[2+i]*condvarARCH2[252+l-i]
    }

}  
}

retARCH
retsquared
retaheadsq<-retahead*retahead
retaheadsq
for(i in 1:5){
  diffARCH[i] <- abs(retARCH[i]-retaheadsq[i])
}
diffARCH
#Fitting GARCH(1,1)
ret
library(fGarch)
fitg = garchFit(~garch(1, 1), data=ret)

summary(fitg)
par(mfrow=c(1,1))  


fore =predict(fitg, n.ahead=10)
#volatility plot


condvarARCH2 <-numeric(258)
condvarARCH2[1]<-coef(fita2)[2] /(1-coef(fita2)[3]-coef(fita2)[4])
for(i in 2:252){
  condvarARCH2[i]<- (coef(fita2)[2] + coef(fita2)[3]*retsquared[i-1] + coef(fita2)[4]*condvarARCH2[i-1])
  
}
volcomp<-volatility(fita2) #vector of conditional variance calculated by garchFit
diffvol<-abs(volcomp-condvarARCH2)
par(mfrow=c(1,2))
plot(dateret, sqrt(condvarARCH2), type="l")
plot(dateret, volatility(fita2, type="sigma"), type="l")

ypred <- (coef(fita2)[2] + coef(fita2)[3]*condvarARCH2[250]) 
sqrt(ypred)
dateret
sd(ret)
tail(order(ret));head(order(ret)) #smallest and biggest returns
condvarARCH2[250]





library(forecast)
require('xts')
is.character(SP500[,1])
SP500[,1]
dateret <- as.Date(as.character(SP500[,1]),format="%Y-%m-%d")
dateret
SP500ts <-ts(SP500[,5],dateret)
SP500tsret <- ts(ret, dateret)
SP500ts<-xts(SP500[,5], dateret)
time(SP500ts)
fit <- lm(SP500ts ~ time(SP500ts))
summary(fit)


warnings()
fit<-lm(ret ~ dateret)
fit$residuals #Residuals from linear regression
fit.stdres = rstandard(fit)

par(mfrow=c(1,2))  

plot(fit$residuals, type="l")
volatility(fit)
plot(fit1, which=7)
fitted(fit)
resid(fit)
segments(dateret, fitted(fit), dateret, ret) #
plot(dateret, ret)
summary(fit)
abline(fit)
mean(fit$residuals)
var(fit$residuals)
sigma(fit)
sd(fit$residuals)
par(mfrow=c(2,2))  
par(mfrow=c(1,1))

stdev <-numeric(1:300)

plot(fit)
library(ggplot2)
time<-as.Date(SP500[,1])
time
Sys.setlocale("LC_ALL", "English")

plot(time,SP500[,5], 
     main = "S&P500", type="l", xlab = "", ylab = "Closing Price")

plot.ts(SP500[,5])
library(fGarch)
install.packages("rugarch")
library(rugarch)
par(mfrow=c(1,1))
fit1=garchFit(~garch(1,1), data=fit$residuals)
model<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                  distribution.model = "norm")
fit2 =ugarchfit(data=fit$residuals, spec=model, solver = 'hybrid')
coef(fit2)
plot(fit2)
plot(fit1, which=1)
forc1 = ugarchforecast(fit2, n.ahead = 50)
plot(fit$residuals, forc1)
plot(forc1, which=1)
fore = predict(fit1, n.ahead=10)
predict(fit1, n.ahead = 10, plot=TRUE, crit_val=2)
summary(fit1)
fit@fit$
fit1@fit$res[1]
fit1@fit$coef[2]
volatility(fit1)
plot(dateret, volatility(fit1), type="l")
coef(fit1)
condvar1 <- fit1@fit$coef[2] /(1-fit1@fit$coef[3]-fit1@fit$coef[4])
sqrt(condvar1)

condvar <-numeric(250)
length(dateret)
condvar[1]<-sd(fit$residuals)
for(i in 2:250){
  condvar[i]<- (fit1@fit$coef[2] + fit1@fit$coef[3]*retsquared[i-1] + fit1@fit$coef[4]*condvar[i-1])

}
volcomp<-volatility(fit1) #vector of conditional variance calculated by garchFit
diffvol<-abs(volcomp-condvar)
par(mfrow=c(1,2))
plot(dateret, sqrt(condvar), type="l")
plot(dateret, volatility(fit1, type="sigma"), type="l")
library(stats)
sd(fit1)
plot(sigma(fit1), ylab="sigma(t)", col="blue")