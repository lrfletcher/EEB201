
ttmax<-10
RR<-1.2
N0<-100
K<-105
NN<-matrix(NA, nrow=1, ncol=ttmax+1)
NN[1]=N0
print(NN)
for(tt in 1:ttmax){
  NN[tt+1]<-NN[tt]*(1+RR*(1-(NN[tt]/K)))
}
plot(1:(ttmax+1),NN, xlab="Time", ylab="Population", type="b", col='red')

#Create discrete logistic growth function
discretelogFun<-function(RR,N0,ttmax,K)
  {NN<-matrix(NA, nrow=1, ncol=ttmax+1)
  NN[1]=N0
  for(tt in 1:ttmax){
    NN[tt+1]<-NN[tt]*(1+RR*(1-(NN[tt]/K)))
}
plot(1:(ttmax+1),NN, xlab="Time", ylab="Population", type="b", col='red')
}
discretelogFun(RR=0.7,N0=100,K=700, ttmax=10)

#Do final exercise
discretelogFun<-function(RR,N0,ttmax,K)
{NN<-matrix(NA, nrow=1, ncol=ttmax+1)
NN[1]=N0
for(tt in 1:ttmax){
  NN[tt+1]<-NN[tt]*(1+RR*(1-(NN[tt]/K)))}
plot(1:(ttmax+1),NN, xlab="Time", ylab="Population", type="b", col='darkorchid4')
}
discretelogFun(RR=-0.3,N0=10,K=100, ttmax=10)

discretelogFun(RR=0.3,N0=10,K=100, ttmax=10)
testnums<-c(-.3,.3,1.3,1.9,2.2,2.7) #make vector
par(mfrow = c(2,3))
for(num in testnums){discretelogFun(num,N0=10,K=100, ttmax=10);}

###################################################################
##Section 4: Writing a di↵erential equation model in R
library(deSolve)
expGrowthODE  <- function(tt, yy, pars) {
  derivs <- pars['rr'] * yy
  return(list(derivs))
}
init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.1)
output <- lsoda(init, tseq, expGrowthODE, pars)
expGrowthOutput <- lsoda( init, tseq, expGrowthODE, pars)
head(expGrowthOutput)
plot(expGrowthOutput[,1], expGrowthOutput[,2], col='blue', type='l')
#It does behave the way I expect an exponential growth model to behave.
#If I change the growth rate, rr, to a negatie value, the population declines
#With a positive growth rate, growth is not hindered by a carrying capacity.
#This makes this model unrealistic, but a good base model.


### Writing a Logistic Growth Model
logGrowthODE<- function(tt, yy, pars) {
  derivs <- pars['rr']*(1-yy/pars['K'])
  return(list(derivs))
}
init <- 1
tseq <- seq(0, 200, by=0.01)
pars <- c(rr = 5, K=100)
logGrowthOutput <- lsoda( init, tseq, logGrowthODE, pars)
head(logGrowthOutput)
plot(logGrowthOutput[,1], logGrowthOutput[,2], col='blue', type='l')
#The model does what I expect. As it approaches the carry capacity, K,
#the population levels off.
#To make the population look more like it's growing exponentially, I could 
#set a very high carrying capacity.
