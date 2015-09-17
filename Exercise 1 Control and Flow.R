
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
print(dogs)

#meatloaf began at 0 and ended at -30
#ex. meatloaf-5+1--->0-5+1=-4. -4-6+1=-9. -9-7+1=-15. -15-8+1=-22. -22-9+1=-30
print(meatloaf)

#bubbles began at value of 12, and ended at -4.
print(bubbles)



#EXERCISE 4
###you can use the if statement with the modulus operator to conditionally perform operations
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0|years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
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
compounded<-rep(bankAccounts, 1)
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }

compounded


