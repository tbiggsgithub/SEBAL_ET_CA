
indir.SEBAL=rep(NA,times=4)
indir.SEBAL.base = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/final/"
indir.SEBAL.list = list.dirs(indir.SEBAL.base,recursive=FALSE)

indir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/from_MMarshal_2013_11_04/"

# towers = c("StaD","StaW","Wil","US.Twt","Big","Fiv")
towers = c("StaD","StaW","Wil","US.Twt","Fiv","Big")

error.calc <- function(indir.SEBAL,start.season.txt,i){
  	start.season = as.POSIXlt(start.season.txt)
	year = format(start.season,"%Y")
  	end.season = as.POSIXlt(paste(year,"-9-30",sep=""))
	
	files.flux.in = list.files(indir.flux,pattern=as.character(year))
	files.flux = files.flux.in[grep("~",files.flux.in,invert=TRUE)]
	fluxfile = files.flux[grep(towers[i],files.flux)]
	nET.daily = 2  # number of separate daily SEBAL ET files for the year.  SEBAL had to break up
			#  series into several bits
	file.SEBALET24.daily=""
	file.SEBALET24.daily[1] = paste("ET24_daily_", year, "061_", year, "180_towers_Rn1030.txt",sep="")
	file.SEBALET24.daily[2] = paste("ET24_daily_", year, "181_", year, "305_towers_Rn1030.txt",sep="")
	setwd(indir.SEBAL)
	for (j in 1:nET.daily) {
		SEBALtmp = read.table(file.SEBALET24.daily[j])
		if (j==1){SEBALET.daily=SEBALtmp}
		if (j>1) {SEBALET.daily = rbind(SEBALET.daily,SEBALtmp)}
	}
	SEBAL.daily.dates = strptime(SEBALET.daily$Date,format="%Y%j")
	dflux = read.table(paste(indir.flux,fluxfile,sep=""),header=TRUE)
  	dflux.dates = strptime(dflux$Date,"%Y-%m-%d")
  	# Recreate the station code from the file name
 	 file.flux.split = strsplit(fluxfile,".",fixed=TRUE)
  	flux.code = paste(file.flux.split[[1]][1],file.flux.split[[1]][2],sep=".")
  	flux.label = paste(flux.code,file.flux.split[[1]][3],sep=" ")
	matchcol.SEBAL = match(flux.code,names(SEBALET.daily))
	SEBALET.daily[SEBALET.daily$fract.notNA<0.75,matchcol.SEBAL]=NA

	ma <- function(x,n){filter(x,rep(1/n,n), sides=2)} # Moving average
	SEBALET = data.frame(Date=SEBAL.daily.dates,ET=SEBALET.daily[,matchcol.SEBAL])
	alldays = as.Date(seq(as.Date(start.season),as.Date(end.season),by=1))
	SEBALET.daily.interp = approx(as.Date(SEBALET$Date),SEBALET$ET,xout=alldays,rule=2)
	#plot(as.Date(SEBALET$Date),SEBALET$ET)
	#lines(as.Date(SEBALET.daily.interp$x),SEBALET.daily.interp$y)
	#points(as.Date(SEBALET.daily.interp$x),SEBALET.daily.interp$y,pch=20,cex=1)
  	SEBALET.noNA = SEBALET[!is.na(SEBALET$ET),]
  	SEBAL.MA = ma(x=SEBALET.noNA$ET,n=9)
    	fluxET = data.frame(Date=dflux.dates,ET=dflux$Eta.mm.day)
  	fluxET.noNA = fluxET[!is.na(fluxET$ET),]
  	fluxET.MA = ma(x=fluxET.noNA$ET,n=15)
	# Calculate seasonal total ET

	seasonal.ET.SEBAL = round(mean(SEBALET.daily.interp$y),1)
	#seasonal.ET.SEBAL = round(mean(SEBALET[(SEBALET$Date<=end.season)&(SEBALET$Date>=start.season),]$ET,na.rm=TRUE),1)
  	seasonal.ET.flux = round(mean(fluxET[(fluxET$Date<=end.season)&(fluxET$Date>=start.season),]$ET,na.rm=TRUE),1)
	err.SEBAL = round((seasonal.ET.SEBAL-seasonal.ET.flux)/seasonal.ET.flux,2)*100
	return(c(seasonal.ET.flux,seasonal.ET.SEBAL,err.SEBAL))
}

stats.sum <- function(indir){
	stats = data.frame(matrix(NA,6,5))
	colnames(stats) = c("Tower","Year","ET.tower","ET.SEBAL","err.SEBAL")
	#rownames(stats) = towers
	indir=indir
	stats[,1]=towers
	if (!is.na(stations.2012[1])){  # southern plot only has 2011 data, so stations.2012 is NA
	for (i in stations.2012){
	
	stats[i,3:5] = error.calc(indir.SEBAL=indir,start.season.txt=start.dates[i],i=i)
	stats[i,2]=2012
	}}
	for (i in stations.2011){
	stats[i,3:5] = error.calc(indir,start.season.txt=start.dates[i],i=i)
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

