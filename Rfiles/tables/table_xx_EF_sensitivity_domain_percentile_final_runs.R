library(XLConnect)

#indir.SEBAL=rep(NA,times=4)
indir.SEBAL.base = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/final/"
indir.SEBAL.list = list.dirs(indir.SEBAL.base,recursive=FALSE)

indir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/from_MMarshal_2013_11_04/"
indir.flux.Rn = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/"
files.fluxtower = list.files(indir.flux.Rn,pattern="xlsx")
towers = c("CA.StaD","CA.StaW","CA.Wil","US.Twt","CA.Fiv","CA.Big")
#towers = c("StaD","StaW","Wil","US.Twt","Fiv","Big")
towers2 = c("StaD","StaW","CA-Wil","US_twt","Fiv","Big") # Used to match tower name to xlsx file
# Load all Rn data

alldays = as.Date(strptime(c(seq(2011001,2011365),seq(2012001,2012366)),format="%Y%j"))

outmat.EF = data.frame(Dates=alldays)
outmat.RnG = data.frame(Dates=alldays)
for (j in 1:length(towers2)){
	print(j)
	flush.console()
	tname = towers2[j]
	file.fluxtower = files.fluxtower[grep(tname,files.fluxtower)]
	flux.wb <- loadWorkbook(paste(indir.flux.Rn,file.fluxtower,sep=""))
	flux.data = readWorksheet(flux.wb, sheet = "R")
	flux.dates = as.Date(strptime(flux.data$Date,format="%Y-%m-%d"))
	flux.RnG.MJ = as.numeric(flux.data$Rn.G)
	flux.RnG.mm = 11.574*flux.RnG.MJ/28.4  # 28.4 converts W/m2 to mm/day
	flux.ET.mm = flux.data$ETec
	flux.ET.mm[flux.ET.mm<0]=NA
	flux.RnG.mm[flux.RnG.mm<0] = 0
	EF = flux.ET.mm/flux.RnG.mm
	EF[EF>5]=NA
	flux.match = match(alldays,flux.dates)
	outmat.EF[,j+1] = round(EF[flux.match],2)
	outmat.RnG[,j+1] = round(flux.RnG.mm[flux.match],1)
	#outmat.RnG$datesmatch = flux.dates[flux.match]
  #  Next:  put into single data frame to calculate ET/RnG...
}

names(outmat.RnG) = c("Dates",towers)
names(outmat.EF) = c("Dates",towers)
error.calc <- function(indir.SEBAL,year,i){
	files.flux.in = list.files(indir.flux,pattern=as.character(year))
	files.flux = files.flux.in[grep("~",files.flux.in,invert=TRUE)]
	fluxfile = files.flux[grep(towers[i],files.flux)]
	nET.daily = 2  # number of separate daily SEBAL ET files for the year.  SEBAL had to break up
			#  series into several bits
	file.SEBALEF24.daily=""
	file.SEBALEF24.daily[1] = paste("EF_", year, "061_", year, "061_towers_Rn1030.txt",sep="")
	file.SEBALEF24.daily[2] = paste("EF_", year, "181_", year, "181_towers_Rn1030.txt",sep="")
	setwd(indir.SEBAL)
	for (j in 1:nET.daily) {
		SEBALtmp = read.table(file.SEBALEF24.daily[j])
		if (j==1){SEBALEF.daily=SEBALtmp}
		if (j>1) {SEBALEF.daily = rbind(SEBALEF.daily,SEBALtmp)}
	}
	SEBAL.daily.dates = strptime(SEBALEF.daily$Date,format="%Y%j")
	dflux = read.table(paste(indir.flux,fluxfile,sep=""),header=TRUE)
  	dflux.dates = strptime(dflux$Date,"%Y-%m-%d")
  	# Recreate the station code from the file name
 	file.flux.split = strsplit(fluxfile,".",fixed=TRUE)
  	flux.code = paste(file.flux.split[[1]][1],file.flux.split[[1]][2],sep=".")
  	flux.label = paste(flux.code,file.flux.split[[1]][3],sep=" ")
	matchcol.SEBAL = match(flux.code,names(SEBALEF.daily))
	ma <- function(x,n){filter(x,rep(1/n,n), sides=2)} # Moving average
	SEBALEF = data.frame(Date=SEBAL.daily.dates,EF=SEBALEF.daily[,matchcol.SEBAL])
  	SEBALEF.noNA = SEBALEF[!is.na(SEBALEF$EF),]
  	SEBAL.MA = ma(x=SEBALEF.noNA$EF,n=9)

	flux.matchcol = match(flux.code,names(outmat.EF))

    	fluxET = data.frame(Date=dflux.dates,ET=dflux$Eta.mm.day)
  	fluxET.noNA = fluxET[!is.na(fluxET$ET),]
  	fluxET.MA = ma(x=fluxET.noNA$ET,n=15)
	# Calculate seasonal total ET
  	start.season = as.Date(start.dates[i])
  	end.season = as.POSIXlt(paste(year,"-9-30",sep=""))
	season.dates = as.Date(strptime(seq(as.numeric(format(start.season,"%Y%j")),as.numeric(format(end.season,"%Y%j"))),"%Y%j"))
	SEBALEF.filled = approx(as.Date(SEBALEF$Date),SEBALEF$EF,season.dates)
	fluxEF.filled = approx(outmat.EF$Dates,outmat.EF[,flux.matchcol],season.dates)
	seasonal.EF.SEBAL = round(mean(SEBALEF.filled$y),2)
  	seasonal.EF.flux = round(mean(fluxEF.filled$y,na.rm=TRUE),2)
	err.SEBAL = round((seasonal.EF.SEBAL-seasonal.EF.flux)/seasonal.EF.flux,2)*100
	return(c(seasonal.EF.flux,seasonal.EF.SEBAL,err.SEBAL))
}

stats.sum <- function(indir){
	stats = data.frame(matrix(NA,6,5))
	colnames(stats) = c("Tower","Year","EF.tower","EF.SEBAL","err.SEBAL")
	#rownames(stats) = towers
	indir=indir
	stats[,1]=towers
	if (!is.na(stations.2012[1])){
	for (i in stations.2012){
	stats[i,3:5] = error.calc(indir=indir,year=2012,i=i)
	stats[i,2]=2012
	}}
	for (i in stations.2011){
	stats[i,3:5] = error.calc(indir,year=2011,i=i)
	stats[i,2]=2011
	}
	stats.out=stats[!is.na(stats[,3]),]
	return(stats.out)
}


# For stations in the north:
stations.2012 = c(1,2,3,4)  # stations that have data in 2012
stations.2011 = c(6)        # stations that have data in 2011

start.dates = c("2012-05-01","2012-05-01","2012-06-17","2012-05-01", "2011-06-01", "2011-07-01")
folder.name.list = list.files(indir.SEBAL.base)
folder.indices.north = grep("north",folder.name.list)
stats=list()
params=data.frame(dom=NA,q=NA,q.s=NA,afit=NA,afit.s=NA,bfit=NA,bfit.s=NA,mbias=NA,minb=NA,maxb=NA,mcorn=NA,mrice=NA,mcot=NA)

for (f in folder.indices.north){
	folder.name.north = folder.name.list[f]
	folder.name.south = folder.name.list[f+15]
	q=as.numeric(strsplit(folder.name.north,"q")[[1]][2])
	q.s = as.numeric(strsplit(folder.name.south,"q")[[1]][2])
	afit.sub = strsplit(folder.name.north,"afit")[[1]][2]
	afit = as.numeric(strsplit(afit.sub,"_")[[1]][1])
	afit.sub = strsplit(folder.name.south,"afit")[[1]][2]
	afit.s = as.numeric(strsplit(afit.sub,"_")[[1]][1])
	bfit.sub = strsplit(folder.name.north,"bfitm")[[1]][2]
	bfit = -as.numeric(strsplit(bfit.sub,"q")[[1]][1])
	bfit.sub = strsplit(folder.name.south,"bfitm")[[1]][2]
	bfit.s = -as.numeric(strsplit(bfit.sub,"q")[[1]][1])
	stations.2012 = c(1,2,3,4)  # stations that have data in 2012
	stations.2011 = c(6)
	stats[[f]] = stats.sum(indir=indir.SEBAL.list[f])
	stations.2011 = c(5)
	stations.2012 = NA
	stats[[f]][6,] = stats.sum(indir=indir.SEBAL.list[f+15])
	err = stats[[f]]$err.SEBAL
	params[f,] = c(2,q,q.s,afit,afit.s,bfit,bfit.s,mbias=round(mean(abs(err)),1),minb=min(err),maxb=max(err),mcorn=round(mean(err[c(1,2)]),1),mrice=round(mean(err[c(3,4,5)]),1),mcot=err[6])
}

folder.indices.one = grep("one",folder.name.list)
stats.1D = list()

paramlen = length(params[,1])
for (f in folder.indices.one){
	paramlen=paramlen+1
	folder.name.one = folder.name.list[f]
	q=as.numeric(strsplit(folder.name.one,"q")[[1]][2])
	afit.sub = strsplit(folder.name.one,"afit")[[1]][2]
	afit = as.numeric(strsplit(afit.sub,"_")[[1]][1])
	bfit.sub = strsplit(folder.name.one,"bfitm")[[1]][2]
	bfit = -as.numeric(strsplit(bfit.sub,"q")[[1]][1])
	stations.2012 = c(1,2,3,4)  # stations that have data in 2012
	stations.2011 = c(5,6)
	stats.1D[[f]] = stats.sum(indir=indir.SEBAL.list[f])
	err = stats.1D[[f]]$err.SEBAL
	params[paramlen,] = c(1,q,q.s=NA,afit,afit.s,bfit,bfit.s,mbias=round(mean(abs(err)),1),minb=min(err),maxb=max(err),mcorn=round(mean(err[c(1,2)]),1),mrice=round(mean(err[c(3,4,5)]),1),mcot=err[6])
}

params$mbycrop = round(rowMeans(cbind(abs(params$mcorn),abs(params$mrice),abs(params$mcot))),1)

outparams = params[,c(1,2,4,6,8,9,10,11,12,13,14)]
outorder = outparams[order(outparams$mbycrop),]
outorder$rank = seq(1:length(outorder[,1]))
outtable = outorder[with(outorder, order(dom,q,afit)),]
names(outtable)[c(3,4)] = c("c","d")

# __________________________



