##Second day of bootcamp

library(deSolve)
#output <- lsoda(init, tseq, ODEfunction, pars)
#This is the general equation for growth equations


##PLOTTING
#pdf(file="Normal_hist.pdf", width=4,height=7); #tells how to save a plot
#open the file
#par: parameters for plotting things
  #ex.par(mfrow=c(2,1) #two rows, one col of plots#, mar=c(4, 4, 3, 2))#specifies margins using vector for four diff sides of the plot
#hist(s1,col=2#specify color is red with number 2,xlab="Value",main="Sigma=1"#gives main title of plot)
#must put dev.off() function at the end, saying you are done sending output to the file

s1<-rnorm(1000,mean=0,sd=1)
s3<-rnorm(1000,mean=0,sd=3)
head(s1)
pdf(file="Normal_hist.fancy.pdf", width=4,height=7); #without this line, the plots will only be generated in R, and not saved in working directory
par(mfrow=c(2,1), mar=c(4, 4, 3, 2))
hist(s1,col=2,xlab="Value",main="Sigma=1")
hist(s3,col=4,xlab="Value",main="Sigma=3")
dev.off()

#hist(s1,col=2,xlab="Value",main=expression(paste(sigma,"=1"))) #the paste function with an unquoted Greek letter name will plot that letter, but you need quotes around the number to specify that it's text
#If you want to make a change, you need to change the file name, or you will overwrite the plot
#if you use "add(argument=TRUE) ?? it will plot the historgrams on top of each other


#plot smooth data sets on same axes
#density plots allow you to see data in same place to easily compare
pdf(file="Normal_density.pdf", width=6,height=6); #open the file
par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #default is 1x1 plot, so this could be ommitted
plot(density(s1),col=2,lwd=4,xlab="Value",xlim=c(-15,15),main="Normal distribution") #lim sets the range of the x/y axes
###NOT a scatterplot (default) beacuse you are using density function. For scatterplot with lines, do type=l
lines(density(s3),col=4,lwd=4) #add the blue values by calling LINES instead of PLOT function #lwd is line width
legend(-15,0.4,c("sigma=1","sigma=3"),lwd=4,col=c(2,4),cex=1.5) #put a legend on #first two numbers say WHERE on the x and y axes to put the legend
#the text in the legend is a vector of character string (sigma=1, etc.)
#you get the red and blue lines from lwd and color vector (the first text thing, ex. sigma=1, will get the first color, here, a red line). Check consistency!!
#cex controls the relative size of the legnd to the plot. is cex is less than one, it will be smaller than normally
abline(v=quantile(s1,0.9),lty=2,lwd=3,col=2) #puts a vertical line onto the plot.
#highlights the upper 1-% (90% below line, 10% above)
#for s1 (above)
abline(v=quantile(s3,0.9),lty=2,lwd=3,col=4) #puts a vertical line onto the plot
#for s3 (above)
dev.off()


#boxplot!
pdf(file="Normal_boxplot.pdf", width=6,height=6)
par(mfrow=c(1,1), mar=c(4, 4, 3, 2))
boxplot(cbind(s1,s3),names=c("Sigma=1","Sigma=3"),main="Draws from a normal distribution",col=c(2,4))
dev.off()
#cbind puts vectors together for the boxplots
#for boxplots, names is like xlab. Use a name for each dataset



hist(s1,plot=F)
#the counts output shows what the tick-mark labels will be
#you can specify where breaks will be, rather than using defaults

#SO
bins<-seq(-10,10,by=1) #sets range of -10 to 10, with units of 1 as breaks. #use same breaks for BOTH datasets
hist(s1,breaks=bins)$breaks #check and see that breaks are as set (see console)
hist(s3,breaks=bins)$breaks #use hist to get counts, but not to make a plot
counts_s1<-hist(s1,breaks=bins)$counts
counts_s3<-hist(s3,breaks=bins)$counts
#use diff fxn to make a plot!
pdf(file="normal_barplot.pdf", width=6,height=6); #open the file
par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins
barplot(rbind(counts_s1,counts_s3),col=c(2,4),beside=T,names.arg=
            seq(-10,9.5,by=1),xlab="Value",ylab="Count")
#rbind says you combine s1 and s3 together into rows. 
#beside=T, plots bars next to each other
#names.arg is names for x-axis, here specified by the seq he wrote
legend(6,350,c(expression(paste(sigma,"=3")),expression(paste(sigma,"=6"))),col=c(2,4),lwd=4)
dev.off()


#Try on your own
s1=rnorm(1000,mean=0,sd=1)
s3=rnorm(1000,mean=0,sd=3)
bins<-seq(-15,15,by=1)
hist(s1,breaks=bins)$breaks
hist(s3,breaks=bins)$breaks
counts_s1<-hist(s1,breaks=bins)$counts
counts_s3<-hist(s3,breaks=bins)$counts
pdf(file="My_practice_plot.pdf", width=6, height=6);
par(mfrow=c(1,1), mar=c(4, 4, 3, 2))
barplot(rbind(counts_s1, counts_s3),col=c(2,4),beside=T,names.arg=seq(-15,14.5,by=1),xlab="Value",ylab="Count")
legend(6,350,c(expression(paste(sigma,"=3")),expression(paste(sigma,"=6"))),col=c(2,4),lwd=4)
dev.off()
#yay, it works!



#Finding Extreme Values
#We can find the % of values in s1 that are >3:
#mean(s1>3)
#[1] 0.001

#just for mean(s1) you would get about 0 because it is a normal distribution
mean(s1>3)


#Scatterplot
pdf(file="scatter_large.pdf",width=10,height=10); #open the file
par(mfrow=c(1,1), mar=c(5, 5, 3, 2)) #sets plotting area and margins
x<-rnorm(100)
y<-x+rnorm(100,sd=0.2)
plot(x,y,pch=19,cex.lab=2,cex.axis=2) #cex.lab changes axis lable size. 
#cex.axis changes axis number size
#pch changes symbol used (not open circles)
dev.off()

#Make our own scatterplot
plot(s1) #plots the y-value in a scatter plot
points(s3,pch=4,col=2) #plot s3 info with different symbol

#sample command ex.
sample(s1,10) #picks 10 random numbers from the set of 1000 in s1

#apply commands
x=(1:10)
y<-matrix(x,nrow=2,ncol=5)
y
apply(y,1,mean)
mean(y[1,]) #check that mean of row 1 is 5
mean(y[2,]) #check that mean of row 2 is 6
#it works! apply gave you those two means
apply(y,2,mean) #here you get five means (for the mean of each of the five col.s in the matrix)


