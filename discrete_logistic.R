
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
testnums
num
