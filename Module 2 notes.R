gap <- read.csv("R Bootcamp 2020/r-bootcamp-fall-2020-master/data/gapminder-FiveYearData.csv")

## R has functions for metadata
v1 <- gap$year
v2 <- gap$continent
v3 <- gap$lifeExp

length(v1)
str(v1)
class(v1)
typeof(v1)
class(v2)
typeof(v2)
class(v3)
typeof(v3)
is.vector(v1)
is.list(v1)

myList <- list(3, c("uganda", "bulgaria"), matrix(1:4, 2))
myList
## Made 3 components to list. First component is the number 3, second component
## is two separate words, third component is a 2X2 matrix with numbers 1:4
is.list(myList)
is.vector(myList)
is.data.frame(myList)
## myList is a list and also has vectors within the list, but isn't a dataframe

##POLL 2A: Which of these is true?
  
##1) `gap` is a data frame
##2) `gap` is a matrix
##3) `gap` is a vector
##4) `gap` is a list
##5) `gap` is a function
is.data.frame(gap)
is.matrix(gap)
is.vector(gap)
is.list(gap)
is.function(gap)

##Storing in R

x <- rnorm(5)
y <- c(5L, 2L, 7L)
z <- list(a = 3, b = c('sam', 'yang'))
z
is.vector(z)
ls()  # search the user workspace (global environment)
rm(x)    # delete a variable
ls()
ls.str() # list and describe variables

#Saving
ls()
save.image('module2.Rda')
?save.image
rm(list = ls()) ##Removes entire global environment
ls()
load('module2.Rda') 
# the result of this may not be quite right in the slide version
ls()