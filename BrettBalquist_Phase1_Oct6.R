#October 6th Aggregate Function

#3 possible input types for an aggregte function 
#data frame, formula, time sereis object
?aggregate

df <- chickwts
head(df)
group_mean <- aggregate(df$weight, list(df$feed), mean)
group_mean <- aggregate(weight ~ feed, data = df, mean) # Equivalent
group_mean #This prints out the groups means
colnames(group_mean) <- c("Group", "Mean")
group_mean #This assigned the new names to the columns

aggregate(chickwts$feed, by = list(chickwts$feed), FUN = length)
aggregate(weight ~ feed, data = chickwts, FUN = length) #equivalent

set.seed

#nstall.packages("lubridate")
library(lubridate)
Dates <- seq(dmy("01/01/2014"), dmy("01/01/2015"), by = "day")
Return <- rnorm(length(Dates))

#install.packages("xts")
#library(xts) 
tserie <- xts(Return, Dates) #This

head(tserie) #This provides the eXtensible Time Series (xts)

dat <- aggregate(tserie ~ month(index(tserie)), FUN = quantile, probs = c(0.05, 0.95)) #This creates the columns of quantiles
colnames(dat)[1] <- "Month" # This labels the first column month
dat

set.seed(1) #This creates random numbers which can be reproduced
cat_var <- sample(c("A", "B", "c"), nrow(df), replace = TRUE) #Creates sample variables with A, B, and C
df_2 <- cbind(df, cat_var) # This merges two data frames fiven that hte number of ros in both the data frames are equal
head(df_2) # Prints out the head

aggregate(df_2$weight, by = list(df_2$feed, df_2$cat_var), FUN = sum)
aggregate(weight ~ feed + cat_var, data = df_2, FUN = sum)
#second one is equivalent and they boh produce aggreagatres of the information.

set.seed(1)
num_var <- rnorm(nrow(df)) #This creaets a new numeric variable called num_var
df_3 <- cbind(num_var, df)
head(df_3)
aggregate(cbind(df_3$num_var, df_3$weight), list(df_3$feed), mean)
#This uses cbind to concatenate them


NOAA_ED <- read.csv("")

#X is a matrix you give to the function
#AggCats is what your aggregating
#AggTarg is what your aggregating by
serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
  if (is.null(AggTarg)) {
    if (is.numeric(AggCats)) 
      AggTarg = (1:ncol(x))[-AggCats]
    if (is.character(AggCats)) 
      AggTarg = colnames(x)[!colnames(x) %in% AggCats]
  }
  Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
  ncat = length(AggCats)
  if (ncat == 1) 
    Cats = as.character(x[, AggCats])
  else Cats = codify(x[, AggCats])
  agged = as.matrix(aggregate(x[, AggTarg], by = list(Cats), FUN = FUN))
  if (ncat > 1) 
    agged = cbind(matrix(unlist(strsplit(agged[, 1], 
                                         "_")), ncol = ncat, byrow = TRUE), matrix(agged[, 
                                                                                         -1], ncol = ncol(agged) - 1))
  if (Numbers) 
    agged = t(apply(agged, 1, as.numeric))
  colnames(agged) = colnames(cbind(x[, c(AggCats[1], AggCats)], 
                                   x[, c(AggTarg, AggTarg[1])]))[c(1, 3:(ncol(agged) + 1))]
  agged
}

