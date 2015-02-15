#  Extract ET (tower, MOD16, SEBAL) for specific dates
#  Plot error against crop height

library(XLConnect)
indir.SEBAL=rep(NA,times=4)
indir.SEBAL.base = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/final/"
indir.SEBAL.list = list.dirs(indir.SEBAL.base,recursive=FALSE)

indir.extract = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/from_MMarshal_2013_11_04/"
towers = c("StaD","StaW","Wil","US.Twt","Fiv","Big")

# Load MOD16ET
file.list = list.files(indir.extract)
#file.MOD16ET = "MOD16ET-2011-2012-9towers.txt"
file.MOD16ET = "MOD16ET-2011-2012-9sites-buffer.txt"
setwd(indir.extract)
MOD16ET = read.table(file.MOD16ET)
MOD16ET.yyyyjjj = as.numeric(rownames(MOD16ET))
MOD16ET$date = strptime(MOD16ET.yyyyjjj,format="%Y%j")
MOD16ET.yr = as.numeric(format(MOD16ET$date,"%Y"))

indir.height.data = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/other_data_fluxtowers/"
fname.height = "biomass_height_fvc_from_michael.xlsx"
wbht = loadWorkbook(paste0(indir.height.data,fname.height))
dates.ht = readWorksheet(wbht,"R.dates")
dates.ht[,3] = as.Date(dates.ht[,3])
dates.ht[,4] = as.Date(dates.ht[,4])
dates.ht[,5] = as.Date(dates.ht[,5])

ht = readWorksheet(wbht,"R")

error.calc <- function(indir.SEBAL,start.season.txt,ht.dates.tower,i){   # calculates error for one tower
  	start.season = as.POSIXlt(start.season.txt)
	year = format(start.season,"%Y")
  	end.season = as.POSIXlt(paste(year,"-10-15",sep=""))
	MOD16ETsub = MOD16ET[MOD16ET.yr==year,]

	files.flux.in = list.files(indir.flux,pattern=as.character(year))
	files.flux = files.flux.in[grep("~",files.flux.in,invert=TRUE)]
	fluxfile = files.flux[grep(towers[i],files.flux)]
	nET.daily = 2  # number of separate daily SEBAL ET files for the year.  SEBAL had to break up
			#  series into several bits
	file.SEBALET24.daily=""
	file.SEBALET24.daily[1] = paste("ET24_daily_", year, "061_", year, "180_towers_buffer_Rn1030.txt",sep="")
	file.SEBALET24.daily[2] = paste("ET24_daily_", year, "181_", year, "305_towers_buffer_Rn1030.txt",sep="")
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
	matchcol.MOD16 = match(flux.code,names(MOD16ETsub))
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
	SEBAL.MA.interp = ma(x=SEBALET.daily.interp$y,n=9)
    	fluxET = data.frame(Date=dflux.dates,ET=dflux$Eta.mm.day)
	fluxET.interp = approx(as.Date(fluxET$Date),fluxET$ET,alldays,rule=2)
  	fluxET.noNA = fluxET[!is.na(fluxET$ET),]
  	fluxET.MA = ma(x=fluxET.noNA$ET,n=15)
	fluxET.MA.interp = ma(x=fluxET.interp$y,n=9)
	MOD16.interp = approx(as.Date(MOD16ETsub$date),MOD16ETsub[,matchcol.MOD16],alldays,rule=2)
	# Calculate seasonal total ET

	# Extract ET for all sample dates
	SPRdate.index = match(ht.dates.tower[1],alldays)  
	FLWdate.index = match(ht.dates.tower[2],alldays)
	GBdate.index = match(ht.dates.tower[3],alldays)

	date.index = c(match(ht.dates.tower[1],alldays),match(ht.dates.tower[2],alldays),match(ht.dates.tower[3],alldays))

	errors = round(SEBAL.MA.interp[date.index]-fluxET.MA.interp[date.index],2)
	errors.pct = 100*round(errors/fluxET.MA.interp[date.index],2)
	error.MOD16 = round(MOD16.interp$y[date.index]-fluxET.MA.interp[date.index],2)
	error.MOD16.pct = round(error.MOD16/fluxET.MA.interp[date.index],2)*100
	return(c(errors,errors.pct,error.MOD16,error.MOD16.pct))
	# return error spr, flw, gb
}

stats.sum <- function(indir){
	stats = data.frame(matrix(NA,6,14))
	colnames(stats) = c("Tower","Year","ErrorSPR","ErrorFLW","ErrorGB","ESPR%","EFLW%","EGB%","MODSPR","MODFLW","MODGB","MODSPR%","MODFLW%","MODGB%")
	#rownames(stats) = towers
	indir=indir
	stats[,1]=towers
	if (!is.na(stations.2012[1])){  # southern plot only has 2011 data, so stations.2012 is NA
	for (i in stations.2012){
		tower = towers[i]
		ht.dates.tower = dates.ht[(dates.ht$Tower==tower)&(dates.ht$Year==2012),3:5]
		stats[i,3:14] = error.calc(indir.SEBAL=indir,start.season.txt=start.dates[i],ht.dates.tower=ht.dates.tower,i=i)
		stats[i,2]=2012
	}}
	for (i in stations.2011){
		tower = towers[i]
		ht.dates.tower = dates.ht[(dates.ht$Tower==tower)&(dates.ht$Year==2011),3:5]
		#stats[i,3:5] = error.calc(indir,start.season.txt=start.dates[i],i=i)
		stats[i,3:14] = error.calc(indir.SEBAL=indir,start.season.txt=start.dates[i],ht.dates.tower=ht.dates.tower,i=i)
		stats[i,2]=2011
	}
	#stats.out=stats[!is.na(stats[,3]),]
	return(stats)
}

indir.SEBAL = indir.SEBAL.list[9]
indir=indir.SEBAL

# For stations in the north:
stations.2012 = c(1,2,3,4)  # stations in north that have data in 2012 (StaD, StaW, Wil, Twt)
stations.2011 = c(6)        # stations in north that have data in 2011 (Big)

start.dates = c("2012-05-01","2012-05-01","2012-06-17","2012-05-01", "2011-06-01", "2011-07-01")
folder.name.list = list.files(indir.SEBAL.base)
folder.indices.north = grep("north",folder.name.list)
stats=list()
f=9
stats = stats.sum(indir=indir.SEBAL.list[f])

# southern domain
stations.2011 = c(5)
stations.2012 = NA
stats.south = stats.sum(indir=indir.SEBAL.list[f+15])
stats[5,]=stats.south[5,]

htindex = c(3,4,6,2,8,5) # reorganizes ht into same rows as stats
statsvec = matrix(as.matrix(stats[,3:5]),6*3,1)
statsvec.pct = matrix(as.matrix(stats[,6:8]),6*3,1)
hvec = as.numeric(matrix(as.matrix(ht[htindex,3:5]),6*3,1))
FVC.vec = as.numeric(matrix(as.matrix(ht[htindex,6:8]),6*3,1))
AWB.vec = as.numeric(matrix(as.matrix(ht[htindex,9:11]),6*3,1))

statsvec.MOD.pct = matrix(as.matrix(stats[,12:14]),6*3,1)
crops = c("Corn","Corn","Rice","Rice","Cotn","Rice")
cropleg = c("Corn","Rice","Cotn")
colvec = c("black","black","black")
colvecMOD = c("grey","grey","grey")
pchvec = c(16,17,18)
pchvecMOD = c(16,17,18)
cexvec = c(1.2,1.2,1.5)

cropsvec = rep(crops,3)
pchplot = pchvec[match(cropsvec,cropleg)]
colplot = colvec[match(cropsvec,cropleg)]
cexplot = cexvec[match(cropsvec,cropleg)]
colplotMOD = colvecMOD[match(cropsvec,cropleg)]

# plot(hvec,statsvec)

outdir.figs = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/figures/"
plottofile = 1

if (plottofile==1){
setwd(outdir.figs)
postscript("figure_xx_error_vs_height.eps",height=5,width=5,horizontal=FALSE)
} else {
 dev.new(height=5,width=5)
}

plot(hvec,statsvec.pct,xlim=c(0,350),ylim=c(-100,120),xlab="Crop height m",ylab="Error %",pch=pchplot,col=colvec,xaxs="i",yaxs="i",cex=cexplot)
abline(0,0,lty=3)
points(hvec,statsvec.MOD.pct,pch=pchplot,col=colplotMOD,cex=cexplot)

#legend("topleft",cropleg,pch=pchvec,col="black",bty="n")

legx = c(20,40,60)
legy = c(95,80)

points(legx,rep(legy[1],3),pch=pchvec,col="black",cex=cexvec)
points(legx,rep(legy[2],3),pch=pchvec,col="grey",cex=cexvec)
text(legx,legy[1]+15,c("M","R","C"))
text(rep(100,2),legy,c("SEBAL","MOD16"))
text(310,93,"GB")
points(310,93,cex=8.5)

if (plottofile==1){
	dev.off()
}

# Plot FVC vs error
plot(FVC.vec,statsvec.pct,xlim=c(0,1),ylim=c(-100,120),xlab="FVC",ylab="Error %",pch=pchplot,col=colvec,xaxs="i",yaxs="i",cex=cexplot)
abline(0,0,lty=3)
points(FVC.vec,statsvec.MOD.pct,pch=pchplot,col=colplotMOD,cex=cexplot)

#legend("topleft",cropleg,pch=pchvec,col="black",bty="n")

#  Plot AWB vs error
plot(AWB.vec,statsvec.pct,xlim=c(0,12000),ylim=c(-100,120),xlab="AWB",ylab="Error %",pch=pchplot,col=colvec,xaxs="i",yaxs="i",cex=cexplot)
abline(0,0,lty=3)
abline(-50,0,lty=3)
points(AWB.vec,statsvec.MOD.pct,pch=pchplot,col=colplotMOD,cex=cexplot)


lm.ht.fvc = lm(y~(H+FVC),data.frame(y=statsvec.MOD.pct,H=hvec,FVC=FVC.vec))
summary(lm.ht.fvc)
lm.fvc = lm(y~(FVC),data.frame(y=statsvec.MOD.pct,H=hvec,FVC=FVC.vec))
summary(lm.fvc)
lm.h = lm(y~(H),data.frame(y=statsvec.MOD.pct,H=hvec,FVC=FVC.vec))
summary(lm.h)
lm.awb = lm(y~(x1),data.frame(y=statsvec.MOD.pct,x=hvec,x1=AWB.vec))
summary(lm.awb)
lm.ht.awb = lm(y~(H+AWB),data.frame(y=statsvec.MOD.pct,H=hvec,AWB=AWB.vec))
summary(lm.ht.awb)

lm.ht.awb.fvc = lm(y~(H+AWB+FVC),data.frame(y=statsvec.MOD.pct,H=hvec,AWB=AWB.vec,FVC=FVC.vec))
summary(lm.ht.awb.fvc)

library(Hmisc)
#r = cor(data.frame(y=as.numeric(statsvec.MOD.pct),H=as.numeric(hvec),AWB=as.numeric(AWB.vec),FVC=as.numeric(FVC.vec)))
r = rcorr(as.matrix(data.frame(y=as.numeric(statsvec.MOD.pct),H=as.numeric(hvec),AWB=as.numeric(AWB.vec),FVC=as.numeric(FVC.vec))))

outtable.r = round(r$r[2:4,1],2)
#library(htmlTable)

#outhtml = htmlTable(



