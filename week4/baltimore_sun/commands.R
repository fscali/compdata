homicides <- readLines("homicides.txt")
homicides[1]
length(grep("iconHomicideShooting", homicides))
length(grep("iconHomicideShooting|icon_homicide_shooting", homicides))
length(grep("Cause: [Ss]hooting", homicides))
length(grep("[Ss]hooting", homicides))

i<-grep("[cC]ause: [Ss]hooting", homicides)
j<-grep("[Ss]hooting", homicides)

#take this vector and subtract out all the elements of the other vector
#that are the same 
#this return 0
setdiff(i,j) #so all indices that are in i are also in j
#return 318 and 859
setdiff(j,i) #there are two leftovers that are in j but not in i


#by default grep returns the indices into the character vector
#where the regex pattern matches
grep("^New", state.name)

#Setting value=TRUE returns the actual elements of the character
#vector that match
grep("^New", state.name, value=TRUE)

#grepl returns a logical vector indicating which element matches
grepl("^New", state.name)

homicides[1]
length(grep("[Cc]ause: *[Ss]tabbing", homicides))

#grep doesn't tell where the pattern matches
#regexpr function gives you the index into each string
# where the match begins and the length of the match for
#that string

#regexpr only gives you the first match of the string (reading
#left to right).
#grexexpr will give you all of the matches in a 
#given string

#can we find the date of the homicide?
homicides[1]
#<dd>Found on January 1, 2007
regexpr("<dd>[F|f]ound(.*)</dd>", homicides[1:10])

#it shows this:
#character where each match begins
# [1] 177 178 188 189 178 182 178 187 182 183
#the lenght of each match
# attr(,"match.length")
# [1] 93 86 89 90 89 84 85 84 88 84
# attr(,"useBytes")
# [1] TRUE

substr(homicides[1],177, 177+93-1)
#it shows this:
#<dd>Found on January 1, 2007</dd><dd>Victim died at Shock Trauma</dd><dd>Cause: shooting</dd>
#it is more than I wanted becaus the * is greedy
#so:
regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:10])
substr(homicides[1],177,177+33-1)
#<dd>Found on January 1, 2007</dd>
  
#one handy function is regmatches which extracts the matches
#in the strings for you without you having to use substr
r<-regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
regmatches(homicides[1:5],r)


#to strip the <dd>
x<-substr(homicides[1], 177, 177+33-1)
#matches only the first
sub("<dd>[F|f]ound on |</dd>", "",x)
#matches all the matches
gsub("<dd>[F|f]ound on |</dd>" ,"",x)

#sub/gsub can take vector arguments
r<-regexpr("<dd>[Ff]ound(.*?)</dd>", homicides[1:5])
m<-regmatches(homicides[1:5],r)
d<-gsub("<dd>[Ff]ound on |</dd>","",m)
as.Date(d, "%B %d, %Y")

#the regexec function works lige regxepr except it gives you the
#indices for parenthesized sub expressions
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])
# [[1]]
# [1] 177 190 --> 177 is where the matches start, 190 is where the parenthesized match start
# attr(,"match.length")
# [1] 33 15
substr(homicides[1], 190, 190+15-1)
#[1] "January 1, 2007"

#even easier with the regmatches function
r<-regexec("<dd>[Ff]ound on (.*?)</dd>", homicides[1:2])
regmatches(homicides[1:2],r)
# [[1]]
# [1] "<dd>Found on January 1, 2007</dd>"
# [2] "January 1, 2007"                  <------
# 
# [[2]]
# [1] "<dd>Found on January 2, 2007</dd>"
# [2] "January 2, 2007"                   <------

#let's make a plot of monthly homicide counts
r<-regexec("<dd>[Ff]ound on (.*?)</dd>", homicides)
m<-regmatches(homicides,r)
dates<-sapply(m, function(x) x[2])
dates<-as.Date(dates, "%B %d, %Y")# <--NAs ..need to change the locale
hist(dates, "month",freq=TRUE)
