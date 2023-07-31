#document base per fer Prediccions
GNP<-read.csv("GNP.csv")
head(GNP)
tail(GNP)
data.class(GNP[,2])
GNPts <-ts(GNP[,2], frequency=4, start=c(1947,1), end=c(2002,3))
length(GNPts)
head(GNPts)
plot(GNPts)
GNPret <-diff(log(GNPts))
length(GNPret)
plot(GNPret)
GNPretdt <-data.frame(GNPret)
GNPretdt$GNPret
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
autoplot(GNPret)

str(GNPret)
time(GNPret)
mean(GNPret)
ggplot(data=GNPretdt, aes(x=time(GNPret), y=GNPret))+ geom_point() + geom_line() +geom_hline(yintercept=mean(GNPret), color="red", size=1) +labs(x="Time", y="Log Return")
ggsave("GNP2.png", dpi=320)
log(GNPret)
par(mfrow=c(1,1))
png(filename="GNP1.png",  res=300, width = 2400, height = 1000)
par(mfrow=c(1,2))
acf(GNPret, lag.max=50, main="")
pacf(GNPret, lag.max=50, main="", ylim=c(-0.4, 0.4))
dev.off()

install.packages("astsa")
library(astsa)
sarima(GNPret, 0,0,2)
regr = ar.ols(GNPts, order=1, demean=FALSE, intercept=TRUE)
regr$x.mean
regr$ar
regr$asy.se.coef[1]
regr$asy.se.coef[2]
regr$asy.se.coef[4]
library(stats)
fore = predict(regr, n.ahead=24)
fore$pred
fore$se
GNPahead<-read.csv("GNPpost2002.csv")
head(GNPahead)
tail(GNPahead)
GNPaheadts <-ts(GNPahead[,2], frequency=4, start=c(2002,4), end=c(2008,3))
ts.plot(GNPts, fore$pred, GNPaheadts, col=1:2, xlim=c(2000,2010), ylim = c(10000,16000), ylab="GNP")
U = fore$pred+fore$se; L = fore$pred-fore$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore$pred, type="p", col=2)
lines(GNPaheadts, type="l", col=3)
