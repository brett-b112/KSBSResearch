#Computes 1 + 1 and prints the output to the console when highlighted.
1+1
#Computes the mean of built-in Nile River data
mean(Nile)
#Another way of typing it
print(mean(Nile))
#Prints out all the Nile data
Nile
#Prints out the length of the Nile dataset
length(Nile)
#Creates a histogram of the nile data in the plots tab
hist(Nile)
#Again creates a histogram with breaks of 20 though
hist(Nile,breaks=20)
#This prints the data from a specific year
Nile[2]
#This concatenates the dates and builds a single vector that includes indicie 2, 5, and 6
Nile[c(2,5,6)]
#This creates a vector of indices 2 - 4
Nile[2:4]
#This creates a vector of indices 81 - 100
Nile[81:100]
# This stores the variables inside n81100
n81100 <- Nile[81:100]
#Takes the mean of the object
mean(n81100)
#Takes the stadnard deviation of the object that we created
sd(n81100)

#This generates the mean from 1945-1960
mean((Nile[74:89]))

#This indexes the newly created variable making 81 the new index 1
n81100[1]
Nile[81]

#September 30th Lessons
#Day 3 – Oct 5: Objects II
#Work through fasteR Lessons 4, 5, 6, 7, 8, 11
#(finish any lessons left at home, they’re mostly a review)

sum(c(5,12,13))
#This sums the vector 5 + 12 + 13
sum(Nile > 1200)
#This sums the total number of years that it was higehr than 1200
x <- c(5,12,13)
x
x > 8 #This Determiens if x is greater than 8
sum(x>8) #This prints the total number of true values

sum(Nile > 1200) #Total number of years Nile water was above 1200

which(Nile > 1200) # This prints out what years had above 1200
which1200 <- which(Nile > 1200)
which1200
#This stores the variable
length(which1200) #This prints the length of the object
Nile[which1200]
Nile[Nile > 1200] #These are the same ^
x <- c(5,12,13,8)
x[-1]
x[c(-1,-4)]
#Negative indexing works different in R
#This drops that index

#LESSON 5: On to Data Frames!
head(ToothGrowth)
tg <- ToothGrowth # storing

mean(tg$len)
#Dollar signs are used to denote individual Columns.
#This gets the length column.

#Row column notation
tg[3,1]

#tg$len returns a vector
tg$len[3] # Returns the third element
#d[the rows we want, the columns we want]

mean(tg[,1])
#all rows and the first column
z <- tg[2:5,c(1,3)]
z#Row 2-5 and columns 1 and 3
y <- tg[ ,c(1,3)]
y# all rows

nrow(ToothGrowth) 
# This tells us teh number of rows
tg <- ToothGrowth#
length(tg$len)#
 length(tg$supp)# These al print 60 the length of the column
length(tg$dose)#

#heads second intpu allows you to specif the number of elements
head(ToothGrowth$len, 10)
x <- c(5,12,13)
y <-c('abc', 'de', 'z')
d <- data.frame(x,y)
d # This created a dataframe with X and Y
z <- tg[,-2]
head(z)
#this removes column 2

#LESSON 6: R Factor Class
class(tg)
#Class Data Frame
class(tg$supp)
#Class factor

#LESSON 7: Extracting Rows/Columns from Data Frames
which1200 <- which(Nile > 1200)
Nile[which1200] #This finds the columns meet the prereq

whichOJ <- which(tg$supp == 'OJ')# Records the rows which we need
whichVC <- which(tg$supp == 'VC')# Records the rows which we need
mean(tg[whichOJ, 1]) #Tooth length and OJ
mean(tg[whichVC,1])#Tooth length and OJ

tgoj <- tg[tg$supp == 'OJ',] #This stores
tgvc <- tg[tg$supp == 'VC',]
mean(tgoj$len)
#20.66333
mean(tgvc$len)
#16.96333
#This stores all rows with OJ and VC
#and then takes the mean of the lengths column

#LESSON 8: MORE EXAMPLES OF EXTRACTING ROWS, AND COLUMNS

#tg[row, column]
#tg[tg$supp=='OJ' & tg$len < 8.8,]

#AND operator &

tg[tg$supp=='OJ' & tg$len < 8.8,] #The & operator is and and can combine two
#   len supp dose
#37 8.2   OJ  0.5

#OR operator |
tg[tg$len > 28 | tg$dose == 1.0,]
#This is provides all rows and columns of either

w <- tg[tg$len > 28 | tg$dose == 1.0,]
head(w) # This prints the head of or 

lendose <- tg[,c(1,3)]
head(lendose)
#This prints the columns 1 and 3
# Or name tg[,c('len','dose')]
exts <- Nile[Nile < 800 | Nile > 1300]
head(exts) # It works on vectors as well 
length(exts) # OR operator

#nrow is number of rows and can be used with operators

#LESSON 11: The R List Class

head(mtcars)

mtmpg <- mtcars$mpg

mtl <- split(mtmpg, mtcars$cyl)
mtl #This splits the data into multiple vectors based on cyl
mtl$'4' # You can access by $
mtl[[1]] #Or row

m4 <- mtl[[1]]
m6 <- mtl[[2]]
m8 <- mtl[[3]]
#This stores all of the cylinders for each car

#You can mix data types in lists
l <- list(a = c(2,5), b = 'sky')
l # This fcreates a list with headers a and b and the objects inside of them

names(mtl) <- c('four', 'six', 'eight')
mtl
#You can change the naems of the columns

#MPG for the third car in teh 6-cylinder categroy
mtl[[2]][3]
#mtl[[2]] is a vector
#[3] is the third element

mtl$six[3]
#same but easier to read

#Dataframes are lists each column is one elemnt of the list
mtcars[[1]]
mtcars
