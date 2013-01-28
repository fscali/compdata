#part 1

library(kernlab)
#load data set
data(spam)

dim(spam)
#[1] 4601   58

#obtain a training and test dataset
set.seed(3435)
trainIndicator <- rbinom(4601, 1, prob=0.5)
table(trainIndicator)
# trainIndicator
# 0    1 
# 2314 2287 

trainSpam = spam[trainIndicator==1,]
testSpam  = spam[trainIndicator==0,]
dim(trainSpam)
#[1] 2287   58


#explore data (focus on training set)
names(trainSpam)
# 
# [1] "make"              "address"           "all"               "num3d"             "our"              
# [6] "over"              "remove"            "internet"          "order"             "mail"             
# [11] "receive"           "will"              "people"            "report"            "addresses"        
# [16] "free"              "business"          "email"             "you"               "credit"           
# [21] "your"              "font"              "num000"            "money"             "hp"               
# [26] "hpl"               "george"            "num650"            "lab"               "labs"             
# [31] "telnet"            "num857"            "data"              "num415"            "num85"            
# [36] "technology"        "num1999"           "parts"             "pm"                "direct"           
# [41] "cs"                "meeting"           "original"          "project"           "re"               
# [46] "edu"               "table"             "conference"        "charSemicolon"     "charRoundbracket" 
# [51] "charSquarebracket" "charExclamation"   "charDollar"        "charHash"          "capitalAve"       
# [56] "capitalLong"       "capitalTotal"      "type" 

head(trainSpam)



# make address  all num3d  our over remove internet order mail receive will people report addresses free business email  you credit your
# 1  0.00    0.64 0.64     0 0.32 0.00   0.00        0  0.00 0.00    0.00 0.64   0.00      0         0 0.32        0  1.29 1.93   0.00 0.96
# 7  0.00    0.00 0.00     0 1.92 0.00   0.00        0  0.00 0.64    0.96 1.28   0.00      0         0 0.96        0  0.32 3.85   0.00 0.64
# 9  0.15    0.00 0.46     0 0.61 0.00   0.30        0  0.92 0.76    0.76 0.92   0.00      0         0 0.00        0  0.15 1.23   3.53 2.00
# 12 0.00    0.00 0.25     0 0.38 0.25   0.25        0  0.00 0.00    0.12 0.12   0.12      0         0 0.00        0  0.00 1.16   0.00 0.77
# 14 0.00    0.00 0.00     0 0.90 0.00   0.90        0  0.00 0.90    0.90 0.00   0.90      0         0 0.00        0  0.00 2.72   0.00 0.90
# 16 0.00    0.42 0.42     0 1.27 0.00   0.42        0  0.00 1.27    0.00 0.00   0.00      0         0 1.27        0  0.00 1.70   0.42 1.27
# font num000 money hp hpl george num650 lab labs telnet num857 data num415 num85 technology num1999 parts pm direct cs meeting original
# 1     0      0  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0
# 7     0      0  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0
# 9     0      0  0.15  0   0      0      0   0    0      0      0 0.15      0     0          0    0.00     0  0   0.00  0       0      0.3
# 12    0      0  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0
# 14    0      0  0.00  0   0      0      0   0    0      0      0 0.00      0     0          0    0.00     0  0   0.00  0       0      0.0
# 16    0      0  0.42  0   0      0      0   0    0      0      0 0.00      0     0          0    1.27     0  0   0.42  0       0      0.0
# project re edu table conference charSemicolon charRoundbracket charSquarebracket charExclamation charDollar charHash capitalAve
# 1        0  0   0     0          0         0.000            0.000                 0           0.778      0.000    0.000      3.756
# 7        0  0   0     0          0         0.000            0.054                 0           0.164      0.054    0.000      1.671
# 9        0  0   0     0          0         0.000            0.271                 0           0.181      0.203    0.022      9.744
# 12       0  0   0     0          0         0.022            0.044                 0           0.663      0.000    0.000      1.243
# 14       0  0   0     0          0         0.000            0.000                 0           0.000      0.000    0.000      2.083
# 16       0  0   0     0          0         0.000            0.063                 0           0.572      0.063    0.000      5.659
# capitalLong capitalTotal type
# 1           61          278 spam
# 7            4          112 spam
# 9          445         1257 spam
# 12          11          184 spam
# 14           7           25 spam
# 16          55          249 spam

table(trainSpam$type)

# nonspam    spam 
# 1381     906

plot(trainSpam$capitalAve~trainSpam$type)

#better visualization
plot(log10(trainSpam$capitalAve+1)~trainSpam$type)
plot(trainSpam$capitalAve~trainSpam$type)

#relationships between predictors
plot(log10(trainSpam[,1:4]+1))

#clustering
hCluster <- hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

#statistical prediction/modeling
trainSpam$numType <- as.numeric(trainSpam$type) - 1 #values are 1 and 2, so here I have 0 and 1
costFunction <- function(x,y){
  sum(x!=(y>0.5))
  
}

cvError <- rep(NA,55)
library(boot)
for (i in 1:55){
  lmFormula <- as.formula(paste("numType~", names(trainSpam)[i], sep=""))
  glmFit <- glm(lmFormula, family="binomial", data=trainSpam)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction,2)$delta[2]
}

names(trainSpam)[which.min(cvError)]

#[1] "charDollar" it's the variable that gives you the minimum error rate


#get a measure of uncertainty
predictionModel <- glm(numType ~ charDollar, family="binomial", data=trainSpam)
predictionTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5]  <-"spam"
table(predictedSpam, testSpam$type)
# 
# predictedSpam nonspam spam
# nonspam    1346  458
# spam         61  449

#error rate
(61+458)/(1346+458+61+449)
#[1] 0.2242869 