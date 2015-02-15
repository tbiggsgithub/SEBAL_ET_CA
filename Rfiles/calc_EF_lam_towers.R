#  Calculate LE/(Rn-G) for fluxtower with data

library(XLConnect)
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/"

fnames.in = list.files(indir.flux,pattern="xlsx")
fnames = fnames.in[grep("~",fnames.in,invert=TRUE)]

lamcalc <- function(tower,year){
	fname = fnames[grep(tower,fnames)]
	wb = loadWorkbook(paste(indir.flux,fname,sep=""))
	x = readWorksheet(wb,"R")
	x.dates = strptime(x$Date,format="%Y-%m-%d")
	start.season = as.POSIXlt(paste(year,"-05-01",sep=""))
	end.season = as.POSIXlt(paste(year,"-9-30",sep=""))
	cnames = names(x)
	Rn.col = grep("Rn.G",cnames)[1]
	LE.col = grep("LEec",cnames)
	ET.col = grep("ETec",cnames)
	ETo.col = grep("PM.ETo",cnames)
	x[,c(Rn.col,LE.col,ET.col,ETo.col)] = sapply(x[,c(Rn.col,LE.col,ET.col,ETo.col)],as.numeric)
	lam = as.numeric(x[,LE.col])/as.numeric(x[,Rn.col])
	EF = as.numeric(x[,ET.col])/as.numeric(x[,ETo.col])
	lam[lam<0]=NA
	meanlam = mean(lam[(x.dates<=end.season) & (x.dates>=start.season)],na.rm=TRUE)
	meanEF = mean(EF[(x.dates<=end.season) & (x.dates>=start.season)],na.rm=TRUE)
	xfin.lam = x[(!is.na(x[,LE.col])) & (!is.na(x[,Rn.col])),]
	xfin.lam.dates = strptime(xfin.lam$Date,format="%Y-%m-%d")
	xfin.EF = x[(!is.na(x[,ET.col])) & (!is.na(x[,ETo.col])),]
	xfin.EF.dates = strptime(xfin.EF$Date,format="%Y-%m-%d")
	meanlam.sum = sum(xfin.lam[(xfin.lam.dates<=end.season) & (xfin.lam.dates>=start.season),LE.col])/sum(xfin.lam[(xfin.lam.dates<=end.season) & (xfin.lam.dates>=start.season),Rn.col])
	meanEF.sum = sum(xfin.EF[(xfin.EF.dates<=end.season) & (xfin.EF.dates>=start.season),ET.col])/sum(xfin.EF[(xfin.EF.dates<=end.season) & (xfin.EF.dates>=start.season),ETo.col])
	return(c(meanlam,meanEF,meanlam.sum,meanEF.sum))
}

#  1_mean lambda (ET/Rn.G)   2_mean evaporative fraction (ET/ETo)  3_mean lambda of total seasonal ET/Rn.G
lam.StaD = lamcalc(tower="StaD",year=2012)
lam.StaW = lamcalc(tower="StaW",year=2012)
lam.Wil = lamcalc(tower="CA-Wil",year=2012)
lam.UStwt = lamcalc(tower="US_twt",year=2012)
lam.Big = lamcalc(tower="Big",year=2011)
lam.Fiv = lamcalc(tower="Fiv",year=2011)

outmat.lam.EF = data.frame(round(rbind(lam.StaD,lam.StaW,lam.Wil,lam.UStwt,lam.Big,lam.Fiv),2))
towers = c("StaD","StaW","Wil","US.twt","Big","Fiv")
rownames(outmat.lam.EF)=towers
names(outmat.lam.EF) = c("lam.mean.daily","ETETo.mean.daily","lam.season","ETETo.season")
