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

##Making your own R packages
##See the *devtools* package and `package.skeleton()` 
##for some useful tools to help you create a package. 
##And there are lots of tips/tutorials online, in particular 
##[Hadley Wickham's R packages book](https://r-pkgs.had.co.nz/).



## get working directory
getwd()

##For R 4.0 and later, when reading in data StringsasFactors is false by default
## First try using `stringsAsFactors=FALSE` as is the default in R versions 3.6 and older:
rta <- read.table("../data/RTAData.csv", sep = ",",
                  head = TRUE, stringsAsFactors = TRUE)
rta[1:5, 1:5]
dim(rta)
## great, we're all set, right?
## Not so fast...needs to be FALSE because they are numbers, not categorical variables
unlist(lapply(rta, class))[1:5]

## Second Try
rta <- read.table("../data/RTAData.csv", sep = ",",
                  head = TRUE, stringsAsFactors = TRUE)
rta[1:5, 1:5]
dim(rta)

unlist(lapply(rta, class))[1:5]
##now they are all factors

levels(rta[ , 2])[c(1:5, 3041:3044)]
##Now see problem: missing values treated as 'x', which isn't treated as numerical
help(read.table)

##Treat X as missing values with na.strings
rta3 <- read.table("../data/RTAData.csv", sep = ",", head = TRUE, 
                   stringsAsFactors = FALSE, na.strings = c('NA', 'x'))
unlist(lapply(rta3, class))[1:5] ##takes class of organisms from rta3 rows 1-5
##Now numbers are intergers, as they should be
?unlist
unlist(rta3)

is.list(rta3)
?lapply

# Breakout

### Basics

##1) Make sure you are able to install packages from CRAN. E.g., try to install *lmtest*. And make sure you've installed all the packages we need for the bootcamp (see slide 4). 

##2) Figure out what your current working directory is.

### Using the ideas

##3) Put the *data/cpds.csv* file in some other directory on your computer, such as *Downloads*. Use `setwd()` to set your working directory to be that directory. Read the file in using `read.csv()`.  Now use `setwd()` to point to a different directory such as *Desktop*. Write the data frame out to a file without any row names and without quotes on the character strings.

##4) Make a plot with the gapminder data. Save it as a PDF in *Desktop*. Now see what happens if you set the *width* and *height* arguments to be very small and see how it affects the resulting PDF. Do the same but setting width and height to be very large.

##5) Figure out where (what directory) the *graphics* package is stored on your machine. Is it the same as where the *fields* package is stored?

### Advanced

##6) Load the *spam* package. Note the message about `backsolve()` being masked from package:base. Now if you enter `backsolve`, you'll see the code associated with the version of `backsolve()` provided by the *spam* package. Now enter `base::backsolve` and you'll see the code for the version of `backsolve()` provided by base R. Explain why typing `backsolve` shows the *spam* version rather than the *base* version. 