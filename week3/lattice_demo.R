library(lattice)
package ? lattice
#library(help=lattice)
data(environmental)
head(environmental)
xyplot(ozone~radiation, data=environmental)
#xyplot(ozone~radiation, data=environmental, main= "Ozone vs. Radiation")
summary(environmental$temperature)
#cut temperature in different ranges and
#then plot ozone vs radiation relation 
#in those ranges
temp.cut <- equal.count(environmental$temperature, 4)
temp.cut
xyplot(ozone~radiation | temp.cut, data=environmental)

#change (from bottom to top)
xyplot(ozone~radiation | temp.cut, data=environmental, layout=c(1,4))

#change (from top to bottom)
xyplot(ozone~radiation | temp.cut, data=environmental, layout=c(1,4), as.table=TRUE)

xyplot(ozone~radiation | temp.cut, data=environmental, layout=c(1,4), as.table=TRUE)

#fit a linear model
xyplot(ozone~radiation | temp.cut, data=environmental, layout=c(2,2), as.table=TRUE, pch=20,
       panel=function(x,y,...){
          panel.xyplot(x,y,...)
          fit<-lm(y~x)
          panel.abline(fit, lwd=2)
         
         })


#loess smoothing and custom labeling
xyplot(ozone~radiation | temp.cut, data=environmental, layout=c(2,2), as.table=TRUE, pch=20,
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         
         panel.loess(x,y)
         
       }, xlab="Solar radiation", ylab="Ozone (ppb)", main="Ozone vs Solar Radiation")


#now let's analyze ozone vs radiation relation varying
#both on temperature and wind
wind.cut <- equal.count(environmental$wind,4)

xyplot(ozone~radiation | temp.cut * wind.cut, data=environmental, as.table=TRUE, pch=20,
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         
         panel.loess(x,y)
         
       }, xlab="Solar radiation", ylab="Ozone (ppb)", main="Ozone vs Solar Radiation")


#scatterplot of the relation of every variable
#of a dataset with each other
splom(~ environmental)

#histogram (used similarly to xyplot)
histogram(~ temperature | wind.cut, data=environmental)
histogram(~ ozone | wind.cut, data=environmental)

histogram(~ ozone | temp.cut*wind.cut, data=environmental)
