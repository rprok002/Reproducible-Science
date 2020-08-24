## R as a calculator

2 + 2 # add numbers

2 * pi # multiply by a constant
7 + runif(1) # add a random number
3^4 # powers
sqrt(4^4) # functions

## scientific notation
5000000000 * 1000
5e9 * 1e3

## Assigning values in R
val <- 3
val

myseq <- 1:6
myseq

## Assign years, only every 5 and count # years in sequence
years <- seq(1952,2007, by=5)
years
length(years)

## Repeat "Afghanastan" 12 times in a sequence
country <- rep("Afghanastan", 12)
country

## Vectors by "c()"
## Numeric
nums <- c(1.1, 3, -5.7)
devs <- rnorm(5)
devs

## Integer, force as an integer not a decimal, "L" is for long integer
ints <- c(1L, 5L, -3L)
ints

## \n means new line for each character
chars <- c('hi', 'hello', "mother's", 'father\'s', "She said, 'hi'", "He said, \"hi\"")
chars
cat(chars, sep="\n")

## logical vector
bools <- c(TRUE, FALSE, TRUE)
bools
cat(bools, sep="\n")


## get work directory, set work directory 
getwd()
setwd('C:\\Users\rprokopi\\AppData\\Local\\Programs\\Git\\bin\\Reproducible-Science')

## get file if don't know the extension
gap <- read_csv("r-bootcamp-fall-2020-master/data/gapminder-FiveYearData.csv")                

## Take out specific column
gdp <- gap$gdpPercap
gdp
gdp[1:10]

## Alter dataset
gdp[4]<- 822.9711
gdp[1:10]

## Generate 100 values in a normal distribution
vals <- rnorm(100)
vals

## Make first two of those values 0
vals[1:2] <- 0
vals

## Make all values less than 0 to be 0
vals[vals < 0]<- 0
vals[1:8]
vals

## Subset further
wealthy <- gdp [gdp > 100000]
wealthy
gdp[1:100]

## Calculations on R vectors
gdpTotal <- gap$gdpPercap * gap$pop

## Pull out only gdptotals for 2007
tmp <- gdpTotal[gap$year == "2007"]
tmp
gdpSubset <- tmp[1:10]
gdpSubset

## Determine if all values are greater than 1e6
gdpSubset <= 1e6

## Look at different operators
vec1 <- rnorm(5)
vec2 <- rnorm(5)
vec1 > vec2
vec1 == vec2
vec1 != vec2
vec1 = vec2 #makes equal to
vec1
vec2

## Using "or"
gdpSubset >= 1e12 | gdpSubset <= 1e10

## Using "and"
gap$lifeExp[1:10]< 75 & gap$continent[1:10]== "Americas"

## How would you extract "china" from this list?
myList <- list(stuff = 3, mat = matrix(1:4, nrow = 2), 
               moreStuff = c("china", "japan"), list(5, "bear"))
myList

## Meaning: in moreStuff, there is a vector with china and japan, china being first
myList$moreStuff[1]
myList$moreStuff[[1]] ## not standard for a list, but in this case still works
myList[['moreStuff']][1]
myList[[3]][1]
