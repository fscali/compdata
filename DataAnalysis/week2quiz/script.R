#q2
con <- url("http://simplystatistics.tumblr.com/", "r")
simplyStats <- readLines(con,n=150)
close(con)
print(nchar(simplyStats)[c(2,45,122)])
#[1] 918   5  24

#q3
fileUrl <- "https://dl.dropbox.com/u/7710864/data/csv_hid/ss06hid.csv"
download.file(fileUrl, destfile="survey.csv", method="curl")
survey <- read.csv("survey.csv")
names(survey)
table(survey$VAL)

# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24 
# 75  42  33  30  26  29  23  70  99 119 152 199 233 495 483 486 357 502 232 312 164 159  47  53 

#q4
head(survey$FES)

#q5
table(survey$BDS,survey$RMS)
# 1   2   3   4   5   6   7   8   9
# 0  41   8   0   0   0   0   0   0   0
# 1   0 133 204  63  21   5   0   0   0
# 2   0   0 164 737 386 112  49  15  10
# 3   0   0   0 148 972 825 398 170 115
# 4   0   0   0   0  43 167 331 293 290
# 5   0   0   0   0   0   8  50  80 319

#q6
agricultureLogical <- (survey$ACR == 3 & survey$AGS==6)
indexes <- which(agricultureLogical)
print(indexes)

#q7
subsetDataFrame <- survey[indexes,]
sum(is.na(subsetDataFrame$MRGX))

#Q8
strsplit(names(survey),"wgtp")[123]

#q9
quantile(survey$YBL, na.rm=T)

#q10
fileUrl <- "https://dl.dropbox.com/u/7710864/data/csv_hid/ss06pid.csv"
download.file(fileUrl, destfile="population.csv", method="curl")
housingData <- read.csv("survey.csv")
populationData <- read.csv("population.csv")
names(populationData)
mergedData <- merge(housingData, populationData, by.x="SERIALNO", by.y="SERIALNO", all=TRUE)
dim(mergedData)
#[1] 15451   426