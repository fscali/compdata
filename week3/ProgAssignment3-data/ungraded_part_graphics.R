outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
hist(outcome[,11], main="Heart Attack 30-day Death Rate", xlab = "30-day Death Rate", ylab="Frequency")
outcome[,11] <- as.numeric(outcome[,11])
outcome[,17] <- as.numeric(outcome[,17])
outcome[,23] <- as.numeric(outcome[,23])



myXlab <- "30-day Death Rate"
myRange <- range(outcome[,c(11,17,23)], na.rm=T)

#one above the other
par(mfrow=c(3,1))
hist(outcome[,11], xlab=myXlab,ylab="Frequency", main="Heart Attack", xlim=myRange)
hist(outcome[,17], xlab=myXlab,ylab="Frequency", main="Heart Failure", xlim=myRange)
hist(outcome[,23], xlab=myXlab,ylab="Frequency", main="Pneumonia", xlim=myRange)

#all in a row
par(mfrow=c(1,3))
hist(outcome[,11], xlab=myXlab,ylab="Frequency", main="Heart Attack", xlim=myRange)
hist(outcome[,17], xlab=myXlab,ylab="Frequency", main="Heart Failure", xlim=myRange)
hist(outcome[,23], xlab=myXlab,ylab="Frequency", main="Pneumonia", xlim=myRange)


#write the mean in the title and 
#draw a vertical line at the location of the median for the outcome
#note: for the title there where problems in R-Studio..I had to restart R from the menu, reload and prepare the data and call the below stuff
par(mfrow=c(3,1))
hist(outcome[,11], xlab=myXlab,ylab="Frequency", main=substitute("Heart Attack (" * bar(x) == k * ")", list(k=mean(outcome[,11],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,11], na.rm=T), col="red")
hist(outcome[,17], xlab=myXlab,ylab="Frequency", main=substitute("Heart Failure (" * bar(x) == k * ")", list(k=mean(outcome[,17],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,17], na.rm=T), col="red")
hist(outcome[,23], xlab=myXlab,ylab="Frequency", main=substitute("Pneumonia (" * bar(x) == k * ")", list(k=mean(outcome[,23],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,23], na.rm=T), col="red")


#same as above, but now plot an estimate of the density (and normalize all to probability)
par(mfrow=c(3,1))
hist(outcome[,11], prob=T, xlab=myXlab,ylab="Frequency", main=substitute("Heart Attack (" * bar(x) == k * ")", list(k=mean(outcome[,11],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,11], na.rm=T), col="red")
lines(density(outcome[,11], na.rm=T))
hist(outcome[,17], prob=T, xlab=myXlab,ylab="Frequency", main=substitute("Heart Failure (" * bar(x) == k * ")", list(k=mean(outcome[,17],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,17], na.rm=T), col="red")
lines(density(outcome[,17], na.rm=T))
hist(outcome[,23], prob=T, xlab=myXlab,ylab="Frequency", main=substitute("Pneumonia (" * bar(x) == k * ")", list(k=mean(outcome[,23],na.rm=T))), xlim=myRange)
abline(v=median(outcome[,23], na.rm=T), col="red")
lines(density(outcome[,23], na.rm=T))





#boxplot part

#check how many hospitals per state
table(outcome$State)
#keep only states with more than 20 deaths
outcome2 <- outcome[(table(outcome$State)>=20)[outcome$State],]

#basic boxplot
par(mfrow=c(1,1))
death <- outcome2[,11]
state <- outcome2$State
boxplot(death~state, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State", las=2)


#reorder by median
statesOrdered<-reorder(outcome2$State, outcome2[,11], median, na.rm=T) 
boxplot(death~statesOrdered, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State", las=2)


#shring the x-tick labels and insert the number of hospitals in each state 
boxplot(death~statesOrdered, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State",  xaxt='n')
axis(1,statesOrdered,paste0(statesOrdered,"(",table(outcome2$State)[outcome2$State],")"), las=2)




#lattice part
library(lattice)
hospital <- read.csv("hospital-data.csv", colClasses="character")
outcome.hospital <- merge(outcome,hospital, by="Provider.Number")
death <- as.numeric(outcome.hospital[,11])
npatient <- as.numeric(outcome.hospital[,15])
owner <-factor(outcome.hospital$Hospital.Ownership)
xyplot(death~npatient | owner, 
       xlab="Number of Patients Seen", 
       ylab="30-day Death Rate",
       main="Heart Attack 30-day Death Rate by Ownership",
       layout=c(3,3),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,col=2)
       })





