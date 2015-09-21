
#EXERCISE 1
for(ii in 1:9){
  if (ii == 9) {
    cat("*"); }
  if(ii<9){
    cat("
        ")
  }
}


#EXERCISE 2
for(ii in 1:9){
    cat("*&"); }


#EXERCISE 3
dogs <- 10;
for (i in 1:5){
  dogs <- dogs + 1; 
}
###
meatloaf <- 0; 
for (i in 5:9){
  meatloaf <- meatloaf - i + 1;
  cat(meatloaf) 
}
###
bubbles <- 12;
for (i in -1:-4){
  bubbles <- i;
}

#dogs began at 10 and ended at 15
#ex. for dogs, 10+1=11, 11+1=12, 12+1=13, 13+1=14, 14+1=15
#check with: print(dogs)

#meatloaf began at 0 and ended at -30
#ex. meatloaf-5+1--->0-5+1=-4. -4-6+1=-9. -9-7+1=-15. -15-8+1=-22. -22-9+1=-30
#check with: print(meatloaf)

#bubbles began at value of 12, and ended at -4, because i is just the specified number in -1:-4.
#so bubbles was -1, then -2, then -3, then -4.
#check with: print(bubbles)



#EXERCISE 4
###you can use the if statement with the modulus operator to conditionally perform operations
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  }
  if(years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)
  }
  
}
#added an or statement that the message will print every 2 years (for congressional election) or every 4 years (for presidential eleciton)



#EXERCISE 5
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }
rep(interestRate)
#problem: "compounded" has not been initialized. Compounded needs to be defined.
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
compounded<-rep(bankAccounts)
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }

compounded



#EXERCISE 6
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this
compounded<-rep(bankAccounts, 1)
for (j in 1:5) {
  for (i in 1:length(bankAccounts)){
    bankAccounts[i]<-bankAccounts[i]-house[i]-food[i]-fun[i]+income[i];
    compounded[i] <- interestRate*bankAccounts[i];
    bankAccounts[i]<-bankAccounts[i]+compounded[i];}
}



#EXERCISE 7
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this
compounded<-rep(bankAccounts, 1)
years <- c(2015:2020)
for (j in years) {
  for (i in 1:length(bankAccounts)){
    bankAccounts[i]<-bankAccounts[i]-house[i]-food[i]-fun[i]+income[i];
    if(j%%2==1){
      bankAccounts[1]<-bankAccounts[1]+5;
      bankAccounts[3]<-bankAccounts[3]+5;
    }
    compounded[i] <- interestRate*bankAccounts[i]; #equations are in this order because I'm assuming trust fund disbursements get interest
    bankAccounts[i]<-bankAccounts[i]+compounded[i];}
}




#EXERCISE 8
xx<-0
total<-0
while(xx <= 17) {
  total <-xx + total;
  xx<-xx+1;
}
print(total)
#check using sum(1:17)=153


  
#EXERCISE 9
Sizefunc<-function(xx){
  if (xx<=-1){
    cat("small", "\n")}
  if (xx>-1&xx<1) {
    cat("medium", "\n")}
  if (xx>=1){
    cat("large","\n")
  }
}
#test with
Sizefunc(2)
Sizefunc(-3)
Sizefunc(0)