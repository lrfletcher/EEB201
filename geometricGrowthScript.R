#geometricGrowthScript.R
#a script to simulate and pot the discrete logistic model

#Setup
#none needed, since the program is so simple
#Set initial conditions and parameter values
N0<-100
RR<-1.05
tt<-10 #assign this so you don't have to change the number throughout, only once up here
#initialize variable to a vector of NA values
NN<-matrix(NA,11,1)
NN[1]=N0 #makes sure that the initial value is included in the plot # could have put 100 instead of N0
#use a loop to iterate the model the desired number of times
for(tt in 1:tt){
  NN[tt+1]<-NN[tt]*RR #fill in next element in the vector with the RR*NN equation #this is saying that the value in the matrix column for tt+1 will be specified as the N0*R that we wanted in the beginning
}
#plot the results
plot(1:11,NN)

#EXERCISE: PLAY WITH THIS MODEL
#DECLINE
N0<-100
RR<-0.8
tt<-10
NN<-matrix(NA,11,1)
NN[1]=N0 
for(tt in 1:tt){
  NN[tt+1]<-NN[tt]*RR}
plot(1:11,NN, xlab="Time", ylab="Population", type="b", col='red')

#Make it into a function
growthFun<-function(RR,N0,tt){NN<-matrix(NA,11,1)
NN[1]=N0 
for(tt in 1:tt){
 NN[tt+1]<-NN[tt]*RR
}

plot(1:(tt+1),NN, xlab="Time", ylab="Population", type="b", col='red')
}

growthFun(RR=0.9, N0=300, tt=50)
