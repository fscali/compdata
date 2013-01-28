fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
download.file(fileUrl, destfile="./data/earthquakeData.csv", method="curl")
dateDownloaded <- date();
dateDownloaded
eData <- read.csv("./data/earthquakeData.csv")

#whole data set
eData

#dimensions (rows and columns): to check if the number of observations and variables are correct (if I know them a priori)
dim(eData)

#names
names(eData)

#number of rows and cols
nrow(eData)
ncol(eData)


quantile(eData$Lat)
#      0%       25%       50%       75%      100% 
#-61.30050  35.41083  38.78405  52.59743  66.69820 
#for example 50% of the values is less than 38.78

summary(eData)
#it gives quantiles for quantitative columns, and counters for qualitative columns

class(eData)
#dataframe

#trick to get classes for all columns
sapply(eData[1,],class)


#Src      Eqid   Version  Datetime       Lat       Lon Magnitude     Depth       NST    Region 
#"factor"  "factor"  "factor"  "factor" "numeric" "numeric" "numeric" "numeric" "integer"  "factor" 

#unique: for summarizing qualitative variables
unique(eData$Src)

#how many unique values
length(unique(eData$Src))

#table (for qualitative variables)
table(eData$Src)
#for each unique values, we have how many times it appears
# ak  ci  hv  mb  nc  nm  nn  pr  se  us  uu  uw 
# 301 136  28   6 241   4  80  31   1  85  29  34 

#relations between variables
table(eData$Src, eData$Version)
# for example we have 176 observations with source "ak" and version "2"
#      0   1   2   3   4   5   6   7   8   9   A   B   C   D
# ak   0 107 176  18   0   0   0   0   0   0   0   0   0   0
# ci  84   0  46   2   1   2   1   0   0   0   0   0   0   0
# hv   0  15   9   0   1   3   0   0   0   0   0   0   0   0
# mb   0   0   6   0   0   0   0   0   0   0   0   0   0   0
# nc 103  45  45  29   7   5   2   1   1   1   1   1   0   0
# nm   0   0   0   0   0   0   0   0   0   0   4   0   0   0
# nn   0   0   0   0   0   0   0   0   0  80   0   0   0   0
# pr  31   0   0   0   0   0   0   0   0   0   0   0   0   0
# se   0   0   0   0   0   0   0   0   0   0   1   0   0   0
# us   0   0   2   0  14  12  21  12  11   5   4   2   1   1
# uu   0   0  10   6   9   2   2   0   0   0   0   0   0   0
# uw   0  17  11   6   0   0   0   0   0   0   0   0   0   0

#any
eData$Lat[1:10] > 40
#[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
any(eData$Lat[1:10] > 40)
#[1] TRUE
#all
all(eData$Lat[1:10] > 40)
#[1] FALSE


#subset
#extract Latitude and Longitude for observations where both Latitude and Longitude > 0
eData[eData$Lat > 0 & eData$Lon > 0, c("Lat", "Lon")]
#extract Latitude and Longitude for observations where  Latitude OR Longitude > 0
eData[eData$Lat > 0 | eData$Lon > 0, c("Lat", "Lon")]





#Peer Review Experiment Data
fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile="./data/reviews.csv", method="curl")
download.file(fileUrl2, destfile="./data/solutions.csv", method="curl")
reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv")
head(reviews,2)
#id solution_id reviewer_id      start       stop time_left accept
#1  1           3          27 1304095698 1304095758      1754      1
#2  2           4          22 1304095188 1304095206      2306      1

head(solutions,2)
# id problem_id subject_id      start       stop time_left answer
# 1  1        156         29 1304095119 1304095169      2343      B
# 2  2        269         25 1304095119 1304095183      2329      C

#find if there are missing values
is.na(reviews$time_left[1:10 ])
sum(is.na(reviews$time_left))
table(is.na(reviews$time_left))
# FALSE  TRUE 
# 115    84

#important table()/NA issue:
table(c(0,1,2,3,NA,3,3,2,2,3))
# 0 1 2 3 
# 1 1 3 4
table(c(0,1,2,3,NA,3,3,2,2,3), useNA="ifany")
# 0    1    2    3 <NA> 
# 1    1    3    4    1 

#summarization
colSums(reviews)
# id solution_id reviewer_id       start        stop   time_left      accept 
# 19900       19929        5064          NA          NA          NA          NA 

#ignore any values that are equal to NA
colSums(reviews, na.rm=T)

rowMeans(reviews, na.rm=T)