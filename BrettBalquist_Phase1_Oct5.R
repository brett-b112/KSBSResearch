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

##### LESSON 12: Another look at the Nile Data

plot(Nile)
# Plots the Nile Data

which(Nile < 600)
# This yields 1-0 TRUES and FALSES
# 43 is the index so we do 1871+41= 1913
#Year 1913 is the culprit

which(Nile > 1300)
# 1871 + 8 = 1879 is the culprit year

plot(AirPassengers)
which(AirPassengers < 100)
# There are none less than 100

plot(mtcars$wt,mtcars$mpg)
#This plots two vectors against each other
#car weight and miles per gallon and we can easily compare in this graph

is.numeric(Nile)
# [1] TRUE
class(Nile)
# [1] "ts"
#Plot is determining the types and actively working around it.
Nile[55]
# 1925 - 1871 = 54
#1871 is the first element in the Array
# flow for the year 1925 is 1+54 = 55
Nile[1 + 1925 - 1871]
load(url('https://github.com/matloff/fasteR/blob/master/data/prgeng.RData?raw=true'))
head(prgeng)
#This plots data from a source
plot(prgeng$age,prgeng$wageinc)
#This plot plots age against wage from that data

indxs <- sample(1:nrow(prgeng),2500)
#Takes a sample from row 1 to the end of the rows and takes a sample of 25
prgeng2500 <- prgeng[indxs,]
# look this up
#This takes 
prgeng2500 <- prgeng[sample(1:nrow(prgeng),2500),]

#breaking it up from 
#h(g(f(x), 3))
#into
# y <- f(x)
# z <- g(y,3)
# h(z)
plot(prgeng2500$age,prgeng2500$wageinc)
#This prints it more neatly and compares age against wage using our sample
plot(prgeng2500$age,prgeng2500$wageinc,col=prgeng2500$sex)

pregNum = as.numeric(prgeng2500$educ)
plot(prgeng2500$age,prgeng2500$wageinc, col= (1 + (pregNum > 9)),xlab='age',ylab='wage',cex=0.5)
#This labels the axis, changes the dot size to a percent of 1 and colors teh dots as well.

wageByGender <- split(prgeng$wageinc, prgeng$sex)
dm <- density(wageByGender[[1]])
dw <- density(wageByGender[[2]])
plot(dw, col='red')
#This plots the dw graph
points(dm, cex=0.2)
#This plots the poitns of the dm graph
#cex is 20% of full size

install.packages("mgcv")
library("mgcv") # loaded package
plot()
plot(prgeng2500$age,prgeng2500$wageinc)# Plots age vs. wage
smoothModel=gam(prgeng2500$wageinc ~ s(prgeng2500$age))# Uses gam to make a regression and then smooth with regards to age
points(prgeng2500$age, smoothModel$fitted.values, col=2)# Plots the regression as points using the smoothed value 
# y ~ x
# Stuff to the left is y and to the right is x

install.packages("ggplot2")
library(ggplot2)

data("msleep")
head(msleep)
data("EuStockMarkets")
head(EuStockMarkets)
par(mfrow=c(2,2))
stockTime = as.numeric(time(EuStockMarkets))
head(msleep)
##top left
matplot(stockTime, EuStockMarkets, type ="l", lty = 1, lwd = 2)

##top right
plot(msleep$bodywt, msleep$brainwt, xlim = c(0,100), ylim = c(0,0.5), col = "#03befc", pch = 18, main = "body vs brain weights \n across species", xlab = "Body Weight, kg", ylab = "Brain Weight, kg")
legend("topleft", legend=c("One Mammal Species "), pch = 18, box.col = "1", cex = 0.75, col = "#03befc")

## bottom left
plot(msleep$brainwt, msleep$sleep_total,xlim = c(0,5), ylim = c(0,20), col = "green",pch = 18,xlab = "Body Weight, kg", ylab = "Daily Sleep, hrs" )

## bottom right
plot(x = log(msleep$brainwt), y =(msleep$sleep_total), xlim = c(-10,2), ylim = c(0,20),col=2,pch=18,xlab="log Brain weight, kg",ylab="Sleep total, hrs")
par(new=TRUE)
plot(x=log(msleep$brainwt),y=msleep$sleep_rem,pch=18,col=5,yaxt="n",xaxt="n",xlab="",ylab="")
axis(4)
# chaning the color to show what it corresponds with you can do
#axis(4,col = 5)
legend("topright",c("Total sleep","REM sleep"),pch=18,col=c(2,5),cex = 0.75)

?plot

### Lesson 16: Writing Your Own Functions
sum(Nile > 1200)
#This gives us the count of the elements in the Nile data larger than 1200. Now, say we watn the mean of these elements:
gt1200 <- which(Nile > 1200) #We fidn the indicies in Nile for elements larger than 1200
nilesubsetGT1200 <- Nile[gt1200] # We extract the subset of Nile ocnsisteing of those elements
mean(nilesubsetGT1200) # We compute the desired mean
#This is a more compact versio
mean(Nile[Nile > 1200])

mgd <- function(x,d) mean(x[x > d])
#Mean of elements. great than d
# This is a function in a less compact version

mgd(Nile,1200)
#Output: 1250
#Out function works wooo!
mgd(tg$len,10.2)
#This function call eliminates the need to type: mean(tg$len[tg$len > 10.2])

#we are telling R, "R, I want to write my own function. I'd like to name it 'mgd'; it will have arguments 'x' and 'd', and it will do 'mean(x[x > d])'. Please build the function for me. Thanks in advance, R!"

mgd(Nile,1200)
#R execute mgd with Nile being x and 1200 being d. Nile and 1200 are the arguments 
mgd <- function(x,d) return(mean(x[x > d]))
#This now returns a number
save(mgd, file='mean_greater_than_d')
#This saved this to an indiicated file in our folder

#We cna load this later by saying
load('mean_greater_than_d')
#mgd will be restored
rng <- function(y) max(y) - min(y)
rng(Nile)

cgd <- function(x,d) return(sum(x>d))
cgd(Nile,1200)

cgd
?sum
#fucntions are objects
#prints it out 
mgd
