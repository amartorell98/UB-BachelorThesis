#Testing for ARCH effects
alpha0 <-0.25
alpha1 <-0.5
y <-numeric(300)
y[1]<-rnorm(1)
for(i in 2:300)
{
  epsilon <-rnorm(1)
  sigmasq <-alpha0 + alpha1*(y[i-1]*y[i-1])
  y[i] <-epsilon*sqrt(sigmasq)
  
}

library(FinTS)
y.archTest <- ArchTest(y, lags = 3, demean = TRUE)
y.archTest