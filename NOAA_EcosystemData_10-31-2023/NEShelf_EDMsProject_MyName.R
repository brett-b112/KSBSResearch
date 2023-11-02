
###installing packages - only need to do once
install.packages("rio")
install.packages("devtools")
library(devtools)
devtools::install_github("tanyalrogers/GPEDM",force=TRUE)


###Loading data
library(rio); library(GPEDM);
alldat0=import("/Users/brettbalquist/SophmoreYearKU/KSBSResearch/NOAA_EcosystemData_10-31-2023/NOAA_EcosystemData_10-31-2023.csv")
SpeciesUse=c(103,104,105,106,107,108,13,131,135,141,143,15,156,163,164,171,172,193,194,197,23,24,26,301,33,34,35,401,502,503,72,73,74,76,77,78)
DataEntriesUse=alldat0[,"SVSPP"]%in%SpeciesUse
SpeciesNamesCodes=unique(alldat0[DataEntriesUse,c("SVSPP","spnm")]) #Table to look up species data
alldat=as.matrix(alldat0[DataEntriesUse, -c(16,17)])
head(alldat0)

###Functions we will use
#plotGPpredSimp plots a the output of a fitGP() call
#SpName - name of species analyzed, used in titles
#This function will save the plot as a PDF file in the working directory on your computer
#Saved plots have a consistent size/layout, but you'll need to open them, print screen, and then paste & crop into a document
plotGPpredSimp=function(fit,SpName){
	pred=fit$outsampresults; pred=pred[order(pred[,1]),]; up=head(unique(pred[,2]),15);
	info=round(c(E=fit$inputs$E,R2=fit$outsampfitstats[1]),2)
	pdf(paste0(SpName,"___",paste0(info,collapse="__"),".pdf"),width=12,height=9); 
	par(mfrow=c(3,3)+(length(up)>8),lheight=.8); plot(0,0,col=0,axes=FALSE,ylab="",xlab=""); text(0,1.5,cex=4,SpName,xpd=NA);
	text(0,0,cex=2,paste0("E=",info[1],"\n Leave-one-out \n global R2 = ",round(info[2],2)),col=4,xpd=NA)
	for(p in up){
		predi=na.omit(pred[pred[,2]==p,]); ttl=paste0("pop# ",p,"   pop R2=",round(cor(predi[,c("obs","predmean")])[1,2]^2,2));
		matplot(predi[,1],predi[,c("obs","predmean")],lwd=1,pch=16,lty=1,type="o",col=c(1,4),ylab="Obs (black), Predicted (blue)",main=ttl,xlab="")
		polygon(c(predi[,1],rev(predi[,1])),c(predi[,3],rev(predi[,3]))+c(predi[,5],-rev(predi[,5])),col=rgb(0,0,0.5,alpha=0.15),border=0)
	}
	dev.off()
}

zrochk=function(x,mini=min(x,na.rm=TRUE)){ if(mini==0) x=x+min(x[x>0],na.rm=TRUE); x; }
zrochkSer=function(x,cats){ for(ci in unique(cats)) x[cats==ci]=zrochk(x[cats==ci]); x; }
serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
    if (is.null(AggTarg)) {
        if (is.numeric(AggCats)) AggTarg = (1:ncol(x))[-AggCats]
        if (is.character(AggCats)) AggTarg = colnames(x)[!colnames(x) %in% AggCats]
    }
    Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
    ncat = length(AggCats)
    if (ncat == 1) Cats = as.character(x[, AggCats])
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

#codify: turn rows of @param x into single characters, with columns separated by @param sep
#' @cols which cols of x to use; defauls to all
codify=function(x,cols=1:ncol(x),sep="_")  as.matrix(cbind(Index=apply(x[,cols], 1, paste0, collapse=sep),  x[,-cols]))

#nrm2: Normalize x. Default is global; if levels Lvl supplied will be local.
#' @param x vector of values to normalize
#' @param Lvl optional vector denoting category of each x value for local lormalization
nrm2=function (x, Lvl = rep(1, length(x)), xuse = rep(TRUE, length(x)),zeroNullSD = FALSE, center = FALSE){
    normcent = function(x, xuse) {
        SD = sd(x[xuse], na.rm = TRUE)
        if ((zeroNullSD & SD == 0) | center) SD = 1
        (x - mean(x[xuse], na.rm = TRUE))/SD
    }
    xout = x
    for (i in unique(Lvl)) xout[Lvl == i] = normcent(x[Lvl == i], xuse[Lvl == i])
    xout
}

# popsFilterSimp requires the functions serialAgg, nrm2, and codify to be entered into console
# datfull must only have numeric (or NA) entries, and must have column names, which each contain:
# "spp" - Species number
# "pop" - Population number (can be rounded latitude)
# "yr" - Year
# "n" - Mean population biomass
# We'll also need to normalize environmental data. 
# envtDrivers are the column numbers (or names) that tell the function which columns contain environmental data.
popsFilterSimp=function(datfull,envtDrivers=NULL){
	sa=serialAgg(datfull,c("spp","pop"),"n",FUN=function(x) c(mean(x!=0 & !is.na(x)), sum(x!=0 & !is.na(x)), sum(!is.na(x))))
	colnames(sa)[3:5]=c("muPres","nPres","nObs")
	use=sa[,"nPres"]>9 & sa[,"nObs"]>12
	adi=datfull[codify(datfull[,c("spp","pop")])%in%codify(sa[use,c("spp","pop")]),]
	
	#log population biomass, call it new column logn
	adi=cbind(adi,logn=log(zrochkSer(adi[,"n"],adi[,"spp"])))
	#normalize biomass by species and location
	ID=IDn=1e4*adi[,"spp"]+adi[,"pop"]
	adi[,"logn"]=nrm2(adi[,"logn"],IDn,zeroNullSD=TRUE)
	#if present, normalize environmental drivers by species
	if(!is.null(envtDrivers)) adi[,envtDrivers]=apply(adi[,envtDrivers],2,nrm2,Lvl=adi[,"spp"]); #finds the minimum
	#Filter out species with only 1 pop left (never really used)
	nps=serialAgg(adi,"spp","pop",FUN=function(x) length(unique(x)))
	adi=adi[adi[,1]%in%nps[nps[,2]>1,1],]
	adi[!is.na(adi[,"logn"]),]
}


# Rename each of the columns and also add the rounded columns
# datfull must only have numeric (or NA) entries, and must have column names, which each contain:
# "spp" - Species number
# "pop" - Population number (can be rounded latitude)
# "yr" - Year
# "n" - Mean population biomass
# We'll also need to normalize environm

head(alldat)

EnvtDataColumns=c("wtemp","stemp","zp","nao")
MyData=cbind(spp=alldat[,"SVSPP"], pop=round(alldat[,"DECDEG_BEGLAT"]),  yr=alldat[,"GMT_YEAR"], n=alldat[,"EXPCATCHWT"], alldat[,c(EnvtDataColumns, "fall")])
head(MyData)
# Select fall only for now
MyDataUse = MyData[MyData[,"fall"]==1,]
MyDataAgg = serialAgg(MyDataUse, AggCats=c("spp","pop","yr"))
head(MyDataAgg) #This takes the average among sites per each year

MyDataFiltered=popsFilterSimp(MyDataAgg, envtDrivers=EnvtDataColumns)
head(MyDataFiltered)
#This is the cleaned up data

data("thetalog2pop")
head(thetalog2pop)
plot(thetalog2pop[thetalog2pop[,2] =="PopA",3], type="o")
points(thetalog2pop[thetalog2pop[,2] =="PopB",3], type="o", col=2)

library(GPEDM)
data("thetalog2pop")
#This code is copied from the github and uses the sample data
pA=subset(thetalog2pop,Population=="PopA")
pB=subset(thetalog2pop,Population=="PopB")
N=nrow(pA)
plot(pA$Abundance[1:(N-1)],pA$Abundance[2:N],
     xlab="Abundance t",ylab="Abundance t+1",main="PopA")
plot(pB$Abundance[1:(N-1)],pB$Abundance[2:N],
     xlab="Abundance t",ylab="Abundance t+1",main="PopB")
#This is the EDM
tlogtest=fitGP(data = thetalog2pop, y = "Abundance", pop = "Population", E=3, tau=1, 
               scaling = "local", predictmethod = "loo")
#If environment was present you would have another parameter with a concatenation of environmental factors.
#summary(tlogtest) This produces the valuable info we need
#Process variance (ve) is the error
#Phi the higher number the more correlated
#0 we don't need
#Out of sample R2 is the value that we want
con=getconditionals(tlogtest)
#This shows the graphs of the logs with lags of 1 2 and 3 where 2 and 3 got thrown out

#Old way of writing most basic version
#Can write in one line that wil take the E value and do the summary
#E=3; summary(fitGP(data = species103, y = "logn",x="logn", pop = "pop", E=3, tau=1, scaling = "none", predictmethod = "loo"))
#test103=fitGP(data = species103, y = "logn",x="logn", pop = "pop", E=3, tau=1, scaling = "none", predictmethod = "loo")


#Repeating this process for our own data
species103 = MyDataFiltered[MyDataFiltered[,"spp"]==103 & MyDataFiltered[,"yr"]%in%(1977:2008),] #This takes the rows with specific years
E=2; 
test103=fitGP(data = species103, y = "logn",x=c("logn",EnvtDataColumns), pop = "pop", E=E, tau=1, scaling = "none", predictmethod = "loo")
#summary(test103) #This prints out the summary of the data
test103$outsampfitstats[1] #This gets just the R2 value
plotGPpredSimp(test103, "myfishplot")#This saves to the working directory

