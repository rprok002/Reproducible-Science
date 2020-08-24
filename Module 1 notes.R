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

## Basic Questions

##1) Create a variable called 'x' that contains the mean life expectancy.
gap <- read.csv("R Bootcamp 2020/r-bootcamp-fall-2020-master/data/gapminder-FiveYearData.csv")                
x <- mean(gap$lifeExp)
##2) Use functions in R to round 'x' to two decimal places and to two significant digits.
round(x, digits=2)
##3) Create a vector of GDP per capita in units of Euros rather than dollars. Put first of each into a vector.
Eurosperdollar <- 0.85
Eurostodollar <- gap$gdpPercap*Eurosperdollar
c(Eurostodollar[1], gap$gdpPercap [1])
##4) Create a boolean (TRUE/FALSE) vector indicating whether total country GDP is greater than 1 trillion dollars. When entering 1 trillion, use R's scientific notation.
gdpbool <- gap$gdpPercap * gap$pop > 1e12
gdpbool
head(gdpbool)

##5) Use the boolean vector from problem 4 to produce a new vector containing the per capita GDP only from the biggest economies.
gdplargest <- gap$gdpPercap[gdpbool]
gdplargest
gdplargestcountry <- gap$country[gdpbool]
gdplargestcountry
##6) Plot life expectancy against gdpPercap with gdpPercap values greater than 40000 set to 40000.
gdp_sub <- gap$gdpPercap
cens_value <- 4000
gdp_sub[gdp_sub > cens_value] <- cens_value
plot(gap$lifeExp ~ gdp_sub)
##7) Make a histogram of the life expectancy values for the year 2007.  Explore the effect of changing the number of bins in the histogram using the 'breaks' argument.
lifeexp_2007 <- gap$lifeExp[gap$year == 2007]
hist(lifeexp_2007)
hist(lifeexp_2007, breaks = 3)
hist(lifeexp_2007, breaks = 12)
hist(lifeexp_2007, breaks = 20)
##8) Subset the data to those for the year 2007 (there is a way to do this all at once, but using what we've seen already, you can pull out and subset the individual columns you need). Plot life expectancy against GDP per capita. Add a title to the plot. Now plot so that data for Asia are in one color and those for all other countries are in another color and those for all other continents are in another, using the 'col' argument. Hint: 'col' can take a vector of colors such as "black","red","black", ...
gap2007 <- gap[gap$year == 2007,]
plot(gap2007$lifeExp~ gap2007$gdpPercap, main = "Life Expextancy vs. GDP")
gap2007$color <- "blue" ## added a color colomn to the whole thing
gap2007$color[gap2007$continent == "Asia"] <- "red"
plot(gap2007$lifeExp ~ gap2007$gdpPercap, main = "Life Expectancy vs. GDP", col = gap2007$color)
##9) Consider the following regression model.  Figure out how to extract the $R^2$ and residual standard error and store in new R variables. 
mod <- lm(lifeExp ~ log(gdpPercap), data = gap)
summ <- summary(mod)
R2 <- summ$r.squared
RSE <- summ$sigma
##10) Take your plot from problem 8. Now modify the size of the points. Add a legend. Rotate the numbers on the y-axis so they are printed horizontally. Recall that `help(par)` will provide a lot of information.
help(par)
plot(gap2007$lifeExp ~ gap2007$gdpPercap, main = "Life Expectancy vs. GDP", col = gap2007$color, cex = 0.75, las = 1)
legend("bottomright",
       legend = c('Asia', 'All other'),
       col = c('red', 'blue'),
       cex = 0.7)
