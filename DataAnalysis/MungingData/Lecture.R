reviews<-read.csv("./data/reviews.csv")
solutions<-read.csv("./data/solutions.csv")
head(reviews,2)
# id solution_id reviewer_id      start       stop time_left accept
# 1  1           3          27 1304095698 1304095758      1754      1
# 2  2           4          22 1304095188 1304095206      2306      1

head(solutions,2)
# id problem_id subject_id      start       stop time_left answer
# 1  1        156         29 1304095119 1304095169      2343      B
# 2  2        269         25 1304095119 1304095183      2329      C

names(reviews)
#[1] "id"          "solution_id" "reviewer_id" "start"       "stop"        "time_left"   "accept"

sub("_","",names(reviews),)
#[1] "id"         "solutionid" "reviewerid" "start"      "stop"       "timeleft"   "accept"    

testName <- "this_is_a_test"
sub("_","",testName)
#[1] "thisis_a_test"
gsub("_","",testName)
#[1] "thisisatest"

#sometimes you wanna take quantitative variables and see their ranges
reviews$time_left[1:10]
#[1] 1754 2306 2192 2089 2043 1999 2130   NA 1899 2024

timeRanges <- cut(reviews$time_left,seq(0,3600, by=600))
timeRanges[1:10]

# it has divided my data in 6 ranges (every 600 units) and I can see for example that the firs element is in the third range
#, the second is in the 4th range and so on
# [1] (1.2e+03,1.8e+03] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03] <NA>             
#   [9] (1.8e+03,2.4e+03] (1.8e+03,2.4e+03]
# Levels: (0,600] (600,1.2e+03] (1.2e+03,1.8e+03] (1.8e+03,2.4e+03] (2.4e+03,3e+03] (3e+03,3.6e+03]

class(timeRanges)
#[1] "factor"

#so now timeRanges is a qualitative variable and I can table it
table(timeRanges, useNA="ifany")
#timeRanges
#(0,600]     (600,1.2e+03] (1.2e+03,1.8e+03] (1.8e+03,2.4e+03]   (2.4e+03,3e+03]   (3e+03,3.6e+03]              <NA> 
#  30                32                25                28                 0                 0                84 

#we can break the dataset  not only on ranges but even on quantiles
library(Hmisc)
timeRanges <- cut2(reviews$time_left,g=6)
table(timeRanges, useNA="ifany")
# timeRanges
# [  22, 384) [ 384, 759) [ 759,1150) [1150,1496) [1496,1909) [1909,2306]        <NA> 
#   20          19          19          19          19          19          84

#we can attach it as an extra variable
timeRanges <- cut2(reviews$time_left,g=6)
reviews$timeRanges <- timeRanges
head(reviews,2)
# id solution_id reviewer_id      start       stop time_left accept  timeRanges
# 1  1           3          27 1304095698 1304095758      1754      1 [1496,1909)
# 2  2           4          22 1304095188 1304095206      2306      1 [1909,2306]

#merging data
names(reviews)
#[1] "id"          "solution_id" "reviewer_id" "start"       "stop"        "time_left"   "accept"      "timeRanges" 
names(solutions)
#[1] "id"          "solution_id" "reviewer_id" "start"       "stop"        "time_left"   "accept"      "timeRanges" 
mergedData <- merge(reviews, solutions, all=TRUE)
head(mergedData)
#it reconciles data sets based on id in this case, but it is wrong, they are different ids!!
# id      start       stop time_left solution_id reviewer_id accept  timeRanges problem_id subject_id answer
# 1  1 1304095119 1304095169      2343          NA          NA     NA        <NA>        156         29      B
# 2  1 1304095698 1304095758      1754           3          27      1 [1496,1909)         NA         NA   <NA>
#   3  2 1304095119 1304095183      2329          NA          NA     NA        <NA>        269         25      C
# 4  2 1304095188 1304095206      2306           4          22      1 [1909,2306]         NA         NA   <NA>
#   5  3 1304095127 1304095146      2366          NA          NA     NA        <NA>         34         22      C
# 6  3 1304095276 1304095320      2192           5          28      1 [1909,2306]         NA         NA   <NA>

mergedData2 <- merge(reviews,solutions, by.x="solution_id", by.y="id",all=TRUE)
head(mergedData2[,1:6],3)
#for every solution_id in the reviews data set I search for an id in the solution data set and I merge them
#   solution_id id reviewer_id    start.x     stop.x time_left.x
# 1           1  4          26 1304095267 1304095423        2089
# 2           2  6          29 1304095471 1304095513        1999
# 3           3  1          27 1304095698 1304095758        1754

#sorting
mergedData2$reviewer_id[1:10]
#[1] 26 29 27 22 28 22 29 23 25 29
sort(mergedData2$reviewer_id)[1:10]
#[1] 22 22 22 22 22 22 22 22 22 22
sort(mergedData2$reviewer_id, decreasing=T)[1:10]
#[1] 29 29 29 29 29 29 29 29 29 29

order(mergedData2$reviewer_id)[1:10]
#[1]  4  6 14 22 23 24 27 32 37 39
#note the difference with respect to the sort function: here it says: if you want put this in order,
#the first element must be the 4th, then the 6th an so on

mergedData2$reviewer_id[order(mergedData2$reviewer_id)][1:10]
#[1] 22 22 22 22 22 22 22 22 22 22

#using the order function you can reorder a dataset
head(mergedData2[,1:6],3)
#   solution_id id reviewer_id    start.x     stop.x time_left.x
# 1           1  4          26 1304095267 1304095423        2089
# 2           2  6          29 1304095471 1304095513        1999
# 3           3  1          27 1304095698 1304095758        1754
sortedData <- mergedData2[order(mergedData2$reviewer_id),]
head(sortedData[,1:6],3)
# solution_id id reviewer_id    start.x     stop.x time_left.x
# 4            4  2          22 1304095188 1304095206        2306
# 6            6 16          22 1304095303 1304095471        2041
# 14          14 12          22 1304095280 1304095301        2211
#and order by multiple variables: reviewer_id and then id
sortedData <- mergedData2[order(mergedData2$reviewer_id, mergedData2$id),]
head(sortedData[,1:6],3)
#   solution_id id reviewer_id    start.x     stop.x time_left.x
# 4            4  2          22 1304095188 1304095206        2306
# 14          14 12          22 1304095280 1304095301        2211
# 6            6 16          22 1304095303 1304095471        2041


#reshaping data (in order to have tidy data)
misShaped <- as.data.frame(matrix(c(NA,5,1,4,2,3), byrow=TRUE, nrow=3))
names(misShaped) <- c("treatmentA", "treatmentB")
misShaped$people <- c("John", "Jane", "Mary")
misShaped
#   treatmentA treatmentB people
# 1         NA          5   John
# 2          1          4   Jane
# 3          2          3   Mary
library(reshape2)
melt(misShaped, id.vars="people", variable.name="treatment", value.name="value")

# people  treatment value
# 1   John treatmentA    NA
# 2   Jane treatmentA     1
# 3   Mary treatmentA     2
# 4   John treatmentB     5
# 5   Jane treatmentB     4
# 6   Mary treatmentB     3