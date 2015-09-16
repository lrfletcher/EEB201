# This is my scratch script for practicing R

library(ape)
library(geiger)
library(ggplot2)

#trying in class exercise
cat("hello world")
setwd("/Users/Leila/GitHub/EEB201")
getwd()
?lm()
?plot()
tips<-c("learn R", "love R")
tips
#ls() #see what you have named different variables
#rm(variable name) #removes a single variable's label
#rm(list=ls()) #can get rid of all variables in workspace

source("source.example.R") #working directory is already set, so you can directly source the file
all.I.know.about.life.I.learned.in.grad.school
all.I.know.about.life.I.learned.in.grad.school->dumbFunction #change the function's name because you think it's dumb
dumbFunction()




#Now we will use the ape package: library(ape)
#read in tree
tt<-read.tree("tree.tre")
str(tt) #see structure of tree
head(tt$tip.label) #see first few names in tree
dd<-read.table("data.txt", header=T, sep='\t', as.is = T) #read.table is like read.csv #\t indicates tabs, not commas, separate data. #R doesn't care ' or " #as.is keeps characters from becoming factors?
attributes(dd)
head(dd)
dim(dd) #tells number of rows and columns, stands for dimensions
dim(dd)[1] #gives number of rows
dflength<-dim(dd)[1] #doens't matter how long dataframe is. () enclose function's arguments. []asks for second elemtn of the vector.
dim(dd)[1] #gives first number in vector (here # of rows)
dim(dd)[2] #same for second number
#size<-runif(92) #want the right number of random fish sizes, but will only work if it's 92 rows long SO
size<-runif(dflength)
size
#use cbind to add this column to the data
cbind(dd,size)
head (cbind(dd,size)) #you have added size to this species
#Now, R needs to remember that we have bound these things. We must name them.
dd<-cbind(dd,size) #overwrite dd so the new column is added
#look at specific columns with $. two ex.s
head(dd$species) 
head(dd$size)
dd[1,] #shows row 1, all columns
dd[,1] #shows column 1, all rows
dd[1:10,] #looks at range of things
which(dd$mode=='MPF') #== indicates T/F (ex. 1==2 is False. 1==1 is True. 1=1 thinks you are assigning a variable name on the left side and variable name on the right. Numbers can't be used to start variable names, so it says Error)

#Assignning values
xx=100
xx
#change assignment in either direction
xx<-101
xx
101-> xx
xx

dd$mode=='MPF' #Goes through all 92 mode values and sees if they equal MPF
which(dd$mode=='MPF') #Gets ONLY rows for which the above statement is TRUE
dd[which(dd$mode=='MPF'),] #Gives the rows for which that is true
#examples of subsetting
#dd[1:10,2] #rows 1-10, column 2
#dd[,1] #all rows, column 1
#dd[13,] #arow 13, all columns

#turn subset into new data frame #see which() powerpoint page
just_mpf<-dd[which(dd$mode=='MPF'),]
head(just_mpf)




#Control Statements
#for (ii in 1:5) {cat("\nthe number is ", ii)} #is saying, for ii in 1:5, do the following (in {})
notfish<-c("bat", "dolphin","toad","solder")
for(animal in notfish){cat(animal, "fish\n",sep="")} #animal ges value of list from above 
for(animal in notfish){cat(animal, "fish\n")}#without sep, a space will automatically be put between animal name and "fish"
#note that "animal" could be anything, just makes sure you change it everywhere in the loop. ex, for(x in notfish){cat(x, "fish\n")} will still work
#tweaking the code
for(animal in notfish){cat(animal, "fish\n", " are tasty", sep="")} #need the \n after tasty, to indicate a space
for(animal in notfish){cat(animal, "fish", " are tasty\n", sep="")}

#while statements
#while (SOME CONDITION is TRUE){ do something }
#example
xx<-1
while(xx < 5) {
  xx <- xx+1; #most basic command would end here
  cat("value of xx", xx, "\n")
  if (xx == 3) {
    break; } #break command ends the "while" loop before xx reaches 5
}
print(xx)
#let's modify that example
xx<-1
while(xx < 10) {
  xx <- xx+1;
  cat("value of xx", xx, "\n")
  if (xx == 7) {
    cat("lucky number ", xx, "\n")}
  else if (xx==2){cat(" the number ", xx, "is unlucky\n")}
else {cat("not excited about the number",xx, "\n")} #catches all the other condiitons that are not 7
}
print(xx)

#with this, only "lucky number 7" and output 10 will print because the rest is commented out
xx<-1
while(xx < 10) {
  xx <- xx+1;
  cat("value of xx", xx, "\n")
  if (xx == 7) {
    cat("lucky number ", xx, "\n")}}
