#is.vector can test wheather an object is a vector or not and return a boolean value
#R recognizes size types of atomic vectors doubles, integers, characters, logicals, complex, and raw.
int <- 1L#Utilizing the letter L after an int creates an int vector
text <- "ace"
#This establishes a list of integers
int <- c(1L, 5L)
text <- c("ace", "hearts")
sum(int) #This will sum the vector of items
sum(text) #Type is invalid and cannot be computed

die <- c(1,2,3,4,5,6) # This is a vector fo type Double
typeof(die)#This is type double
#Double often referred to as numeric
#Difference between 4 and 4L is how R saves the numbers in your computers' memory.
#Integers are defined more precisely than doubles
sqrt(2)^2-2
#this should be 0 but it is not
#This is an example of a floating point error
text <- c("Hello", 'World')
text #Thus prints out Hello World and is of type "Character"
typeof(text)
3>4 #Booleans or logicals
4>3
logic <- c(TRUE,FALSE,TRUE)
typeof(logic)


#Other less useful types of data raws and compositions
#Raws store raw bytes of data
raw(3)
typeof(raw(3))
comp <- c(1+1i, 1+2i, 1+3i)
#Comp store complex values i.e. i
comp


#Creates suits
hand <- c("ace", "king", "queen", "jack", "ten")
hand #This creates the hand and has type of character.
attributes(die)# This creates metadata that inlcudes, names, dimensions, and classes.

## ATTRIBUTES

names(die)# This will not be defined yet and we need to create it for ourselves.

names(die) <- c("one", "two", "three", "four", "five", "six")
die # This shows how we have intialized die to have the names as metadata. 
names(die) <- c("uno", "dos", "tres", "quatro", "cinco", "seis")
names(die) # Names is not tied to anything and can be changed
die + 1 #Names do not correlate with with numbers and can be changed
names(die) <- NULL
names(die) #Names becomes NULL or is removed
die # Die still reatins the values

### Dim
## Changing the dimensions of an Array
dim(die) <- c(2,3)
die
#Creates a 3 x 2 matrix of the same values
dim(die) <- c(3,2)
die

#Creates a hypercube of 1 x 2 x 3
dim(die) <- c(1,2,3)
die

#Matrices!!!!
m <- matrix(die, nrow=2)
m #This creates a matrix from teh die with 2 rows
m <- matrix(die, nrow=2,byrow=TRUE)
m #This creates a matrix from teh die with 2 rows

#Arrays :D yeah!
# To use array you have to provide an atomic vector as the first argument, and a vector of dimensions as teh second argument, now called dim:
ar <- array(c(11:14, 21:24, 31:34), dim=c(2,2,3))
ar

hand1 <- c("ace", "king","queen","jack","ten","spades","spades","spades","spades","spades")
matrix(hand1, nrow=5) #This creates matrices with the hands in different rows
matrix(hand1, nrow=5) #This creates matrices with the hands in different rows
dim(hand1) <- c(5,2) #This creates matrices with the hands in different rows

hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", 
           "spades", "ten", "spades")
#Creates two matrixes that are filled by row and column
matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)

#5.5 class
dim(die) <- c(2,3)
typeof(die)
class(die)
attributes(die)

#Factors 
#Passing a vecotr into a factor R will recorde the data as integers and store the data. R adds levels as an attribute as well.


gender <- factor(c("male", "female", "female", "male"))

typeof(gender)
## "integer"

attributes(gender)
## $levels
## [1] "female" "male"  
## 
## $class
## [1] "factor"
unclass(gender)
# 1 is associates with male and 2 is associated with female this is what it means when it is unclassing the item.


#DATA TYPES IN VECTORS 
#R can only store a single tpye of data in a vector. If you input more R will convert the elemnets to a singel type of data.

#COERCION is how R converts between types
#Numeric is converted to charcter, and logical is eitehr converted to character or numeric.
sum(c(TRUE,TRUE,FALSE,FALSE))
#Is the same as
sum(c(1,1,0,0))
# direction conversion
as.character(1)# 1
as.logical(1)# True 
as.numeric(FALSE)#0

## LISTS 
list1 <- list(100:130, "R", list(TRUE,FALSE))
list1
#This prints out the list elements
#Lists can be comprised of different types of elements

card <- list("ace", "hearts", 1)
card # This creates a card that has three things included.

#DataFrame
df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"),
                 value = c(1,2,3))
df
#This creates a dataframe of dictionary of key value pairs and pritns them out

#DataFrame
df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"),
                 value = c(1,2,3),
                 stringsAsFactors = FALSE)
#This removes coercing by R
df

write.csv(deck, file = "cards.csv", row.names = FALSE)#This creates the csv and saves teh data.
getwd() # This prints the workign directory

#Data structuers in R aer vectors, matrices, arrays, lists, and data frames.

#Selecting Values in R
#deck[,]
head(deck) # This returns the head of the dataFrame
deck[1,1]#This returns the 1th and 1th entry
#deck[i,j] it is stored in teh ith and jth index similar to Linear Algebra.
deck[1,c(1,2,3)] # This returns the first row of the deck
deck[1,1:3] # this also returns the first row of the deck


vec <- c(6,1,2,6,10,5)

vec[1:3]

#deck[row, column]
deck[1:2,1:2]
#This returns a new dataframe
deck[1:2, 1]
# This selecting of a single column returns a vector
deck[1:2, 1, drop= FALSE]
#This makes it a data frame instead of vector.
deck[-1, 1:3] #retruns everything but the frist row of the deck
deck[-(2:52), 1:3] # This will return the first row but nothing else
#can't put negative numbers and positiven umbers in the same index 
deck[c(-1:1),1]#Will NOT WORK I REPEAT NOT WORK becuase negatie numbers are indexed with positive numbers.
#Don't index with zero it won't work in R
deck[1,]#This will subet 1 row and all the columns
deck[,1]#This will subset all columns and 1 row

## king spades

rows <- c(TRUE, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
          F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
          F, F, F, F, F, F, F, F, F, F, F, F, F, F)
deck[rows, ]
## face   suit value
## king spades    13

deck[1,c("face", "suit", "value")]#This gives first row of these columns

deck[,"value"]#this gives the entire values column
deal <- function(cards){
  cards[1,]
}
deal(deck)
#At this point it alwasy returns the same thing
deck2 <- deck[1:52,]
head(deck2)

deck3 <- deck[c(2, 1, 3:52), ]

head(deck3)
##   face   suit value
##  queen spades    12
##   king spades    13
##   jack spades    11
##    ten spades    10
##   nine spades     9
##  eight spades     8
random <- sample(1:52, size=52)
random

#This makes a random assortment of numbers and then returns random cards
deck4 <- deck[random, ]
head(deck4)
#This creates a shuffle method
shuffle <- function(cards){
  random <- sample(1:52, size = 52)
  cards[random, ]
}

#The cards are shuffled between each turn and then dealt.
deal(deck) # This deals a card from the deck
deck2 <- shuffle(deck) # Deck is shuffled
deal(deck2) #Another card is redealed

#Dollar Sign Notation
deck$value #This prints out all of the values from deck rather than indexing with []
#This is returned as a vector
mean(deck$value)
## 7

median(deck$value)
## 7v
lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst
## $numbers
## [1] 1 2

## $logical
## [1] TRUE

## $strings
## [1] "a" "b" "c"
## $numbers
## [1] 1 2

## $logical
## [1] TRUE

## $strings
## [1] "a" "b" "c"

lst[1]

#is.na(parameter) This returns a boolean if a value is missing i.e. NA is in place
# mean(is.na(M)) this returns the proportion of missin values