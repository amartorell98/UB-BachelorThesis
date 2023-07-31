alpha0 <-0.5
alpha1 <-0.3
beta1 <- 0.7
y <-numeric(300)
sigma <-numeric(300)
y[1]<-rnorm(1)
sigma[1]<-sqrt( alpha0 / (1-alpha1-beta1)) # we initialize it to the uncond. variance
sigma[1] <- 1/0.6
control <-numeric(300)
for(i in 2:300)
{
  epsilon <-rnorm(1)
  sigma[i] <-alpha0 + alpha1*(y[i-1]*y[i-1]) + beta1*(sigma[i-1])
  y[i] <-epsilon*sqrt(sigma[i])
  
}

plot(y, type="l", main="GARCH(1,1)", xlab = "", ylab = "")

png(filename="GARCH1.png",  res=250, width = 1250, height = 750)
par(mfrow=c(1,1))
plot(y, type="l", main="GARCH(1,1)", xlab = "", ylab = "")
dev.off()

control[i] <-log(alpha1*y[i] + beta1)
if(is.nan(control[i])){
  na.omit(control)
}

