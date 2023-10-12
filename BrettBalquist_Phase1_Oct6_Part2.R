library(readr)
NOAA_EcosystemData_10_6_2023 <- read.csv("SophmoreYearKU/KSBSResearch/NOAA_EcosystemData_10-6-2023.csv")
View(NOAA_EcosystemData_10_6_2023)
head(NOAA_EcosystemData_10_6_2023)

#Using Northern Shortfin Squid

## Serial Agg Function
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
codify=function (x, cols = 1:ncol(x), sep = "_") 
  as.matrix(cbind(Index = apply(x[, cols], 1, paste0, collapse = sep),  x[, -cols]))

#Creates a subset of the data using Northern Shortfin Squid
NSSData <- NOAA_EcosystemData_10_6_2023[NOAA_EcosystemData_10_6_2023$spnm == "NORTHERN SHORTFIN SQUID",]

#Northern Shortfin Squid
#x is the data matrix
#Aggcats column name
#AggTarg column name
NSSDensity <- serialAgg(NSSData,AggCats = "GMT_YEAR",AggTarg = "EXPCATCHNUM")
plot(NSSDensity, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "Density")

NSSBiomass <- serialAgg(NSSData,AggCats = "GMT_YEAR",AggTarg = "EXPCATCHWT")
plot(NSSBiomass, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "Biomass" )
NSSBiomass

#This is the start of part 5 Using the Northern Shortfin Squid from above.
NSSData$latr <- round(NSSData$DECDEG_BEGLAT) #Makes the rounded degrees column and adds it to NSSData
NSSData # Prints out the graph with added data
unique(NSSData$latr) #prints unique latitudes

head(NSSData)
class(NSSData$GMT_YEAR)
par(mfrow = 1)
c = 1
#This is the color increment it each time
head(YrSpaceWt)
YrSpaceWt <- serialAgg(NSSData, AggCats = c("GMT_YEAR", "latr"), AggTarg = "EXPCATCHWT")
plot(YrSpaceWt[YrSpaceWt[,2] == 35,c(1,3)],xlab = "Year", ylab = "Biomass",type ="l", lty = 1, lwd = 1.5,ylim = c(min(YrSpaceWt[,3]),max(YrSpaceWt[,3]) ))
#lines(YrSpaceWt[YrSpaceWt[,2] == 41,c(1,3)], col = "red")

for (x in 36:44){
  lines(YrSpaceWt[YrSpaceWt[,2] == x,c(1,3)], col = x)
}

par(mfrow = c(3,3)) #sets up a 3x3 plot grid
#GMT_YEAR, fall, STRATUM, SVSPP, doy, DECDEG_BEGLAT, DECDEG_BEGLON, EXPCATCHWT
#This sets 
hist(NSSData$GMT_YEAR) # year
hist(NSSData$fall) # (fall = 0 is spring) (fall = 1 is fall)
hist(NSSData$STRATUM) #A predefined area where a net dredge, or other piece of gear was deployed.
hist(NSSData$SVSPP) #A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST
hist(NSSData$doy) #Day of the year sample was collected
hist(NSSData$DECDEG_BEGLAT) #Beginning decimal degree latitude 
hist(NSSData$DECDEG_BEGLON) #Beginning decimal degree longitude
hist(NSSData$EXPCATCHWT) #Expanded catch weight of all individuals captures of a given species measured to the nearest thousanth of a kilogram.

par(mfrow = c(2,2))
Yr_doy <- serialAgg(NSSData, AggCats = "GMT_YEAR", AggTarg = "doy")
plot(Yr_doy, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "doy" )

Yr_AVGDEPTH <- serialAgg(NSSData, AggCats = "GMT_YEAR", AggTarg = "AVGDEPTH")
plot(Yr_AVGDEPTH, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "AVGDEPTH" )

Yr_SVVESSEL <- serialAgg(NSSData, AggCats = "GMT_YEAR", AggTarg = "SVVESSEL")
plot(Yr_SVVESSEL, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "SVVESSEL" )

Yr_TOWDUR <- serialAgg(NSSData, AggCats = "GMT_YEAR", AggTarg = "TOWDUR")
plot(Yr_TOWDUR, type ="l", lty = 1, lwd = 1.5, main = "Northern Shortfin Squid",xlab = "Year", ylab = "TOWDUR" )
