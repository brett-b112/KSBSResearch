library(readr)
NOAA_EcosystemData_10_6_2023 <- read.csv("SophmoreYearKU/KSBSResearch/NOAA_EcosystemData_10-6-2023.csv")
View(NOAA_EcosystemData_10_6_2023)
head(NOAA_EcosystemData_10_6_2023)

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
#Codify that completes the function
codify=function (x, cols = 1:ncol(x), sep = "_") 
  as.matrix(cbind(Index = apply(x[, cols], 1, paste0, collapse = sep),  x[, -cols]))

#This creates a subset of the data with all the information except only for Atlantic Cod
species_data_agg_function = function(species_name, latitude){
  title <- paste0(species_name," at ",latitude," degrees latitude")#This creates the tile and increases it based on latitude.
  species_sub_data <- NOAA_EcosystemData_10_6_2023[NOAA_EcosystemData_10_6_2023$spnm == species_name,] #Creates the sub data
  head(species_sub_data) #Gets the head of sub data
  species_sub_data[,"EXPCATCHWT"] = log(1+species_sub_data[,"EXPCATCHWT"]) #This logs the specific column
  
  species_sub_data$latr <- round(species_sub_data$DECDEG_BEGLAT) #Makes the rounded degrees column and adds it to NSSData
  head(species_sub_data) # Prints out the graph with added data
  
  #This takes the aggregation based on the year and latitdue against the biomass
  YrSpaceWt <- serialAgg(species_sub_data, AggCats = c("GMT_YEAR", "latr", "fall"), AggTarg = "EXPCATCHWT")
  head(YrSpaceWt)
  plot(YrSpaceWt[YrSpaceWt[,2] == latitude & YrSpaceWt[,3] == 1,c(1,4)],xlab = "Year", ylab = "Biomass",main = title,type ="l", lty = 1, lwd = 2,ylim = c(0,max(YrSpaceWt[,4]) ))
  legend("topright", legend=c("Fall", "Not Fall"), pch = 18, box.col = "1", cex = 0.75, col = c(1,2))
  lines(YrSpaceWt[YrSpaceWt[,2] == latitude & YrSpaceWt[,3] == 0,c(1,4)], col = 2, lwd = 2)
  abline(v=2007.5,col=4,lty=2)
}

#This function takes in the species name and then creates the 3x3 plot.
species_9_plot = function(species_name){
  #This creates the nine plots and plots it for the fishes
  par(mfrow=c(3,3))#Creates a 3x3 plot grid
  for (x in 36:44){ # loops through and plots at each latitude from 36-44
    species_data_agg_function(species_name, x)
  } 
}

#Below are the selected species that has 9 plots of a triple aggregate against time.

#7 primary species
species_9_plot("ATLANTIC COD")
species_9_plot("SPINY DOGFISH")
species_9_plot("BUTTERFISH")
species_9_plot("NORTHERN SHORTFIN SQUID")
species_9_plot("BLACK SEA BASS")
species_9_plot("OCEAN POUT")
species_9_plot("BLUEBACK HERRING")

#2 Alternative co-lead species
species_9_plot("GOOSEFISH")
species_9_plot("WINTER SKATE")

