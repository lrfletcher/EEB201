##Plotting and genetic variation data analysis exercises
##Boot camp day two (9/17/2015)


###QUESTION 1 PART a###
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)
head(snpsDataFrame)
snps=as.matrix(snpsDataFrame)
#DO I NEED THIS?####
testSNP=snps["rs218206_G",]
testSNP
table(testSNP)
het=sum(testSNP==1)/length(testSNP)
####
calc_freq=function(x){
  return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
  return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}
freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)
plot(freq,het,xlab="Frequency",ylab="Heterozygosity")
p=seq(0,0.5,by=0.05)
points(p,2*p*(1-p),type="l",col=2)

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))) #allele freq
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  #cnts are # of inds with 0,1,2 alleles (table would work here, too)
  obscnts=c(cnt0,cnt1,cnt2) #observed counts
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n #1-freq expected heteros, freq^2 is expected homos
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)}

chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)
warnings()
head(chisqs)
pvals=pchisq(chisqs,1,lower.tail=FALSE)
head(pvals)


###QUESTION 1 PART b###
signifthres<-0.05
pvals<0.05 #this equals 4014
sum(pvals<0.05) #This is 181
#181/4014=0.045 and gives the same output as:
mean(pvals<0.05) 
#because pvals>0.05 are FALSE and given a value of 0, and pvals<0.05 are TRUE and given a value of 1
#The proportion of pvals<0.05 is 0.04509218

mean(pvals<0.01)
#Proportion of pvals<0.01 is 0.01021425

mean(pvals<0.001)
#Proportion of pvals<0.01 is 0.00124564


###QUESTION 1 PART c###
length(pvals)
num_pvals<-length(pvals)


###QUESTION 1 PART d###
exp_pvals<-(1:num_pvals)/num_pvals
exp_pvals


###QUESTION 1 PART e###
sort_pvals<-sort(pvals)
sort_pvals


###QUESTION 1 PARTS f-i###
log_sort_pvals=-log10(sort_pvals)
log_exp_pvals=-log10(exp_pvals)

plot(log_exp_pvals, log_sort_pvals)
lines(1:5,col=2,lty=2,lwd=3)
#ideally, there would be a 1:1 ratio of observed and expected p-values





#####################################################################
###QUESTION 2a###
zz=read.table('pheno.sim.2014-1.txt',header=T)
head(zz)
###part b
maxVal<-quantile(zz[,2],0.25)
rows=which(zz[,2]<maxVal)
controls<-zz[rows,]
controls

###part c
minVal<-quantile(zz[,2],0.75)
rows2=which(zz[,2]>minVal)
cases<-zz[rows2,]
cases

###part d
par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) 
plot(density(zz[,2]),col=2,lwd=4,main="Blood Glucose Levels")
abline(v=quantile(zz[,2],0.25),lty=2,lwd=3,col=1)
abline(v=quantile(zz[,2],0.75),lty=2,lwd=3,col=1)

###part e
testSNP=snpsDataFrame["rs7584086_T",(cases[,1])]
Val<-quantile(testSNP,0.75)

###part f
testSNP2=snpsDataFrame["rs7584086_T",(controls[,1])]
Val2<-quantile(testSNP2,0.25,na.rm=T)

###part g
table(testSNP)
het=sum(testSNP==1)
het
homo=sum(testSNP==2)
homo
none=sum(testSNP==0)
none

###part h
testSNP3=testSNP2[which(!is.na(testSNP2))]
table(testSNP3)
het2=sum(testSNP3==1)
het2
homo2=sum(testSNP3==2)
homo2
none2=sum(testSNP3==0)
none2
