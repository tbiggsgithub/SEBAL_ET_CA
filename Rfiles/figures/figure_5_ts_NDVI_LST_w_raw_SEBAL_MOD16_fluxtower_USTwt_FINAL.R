#  Plot time series if Twitchell Rice data
#  Same as other plots, just with water level as a separate plot.

library(XLConnect)

indir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/from_MMarshal_2013_11_04/"

indir.SEBAL.base = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/final/"
indir.SEBAL.list = list.dirs(indir.SEBAL.base,recursive=FALSE)

outdir.figs = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/figures/"

# Load MOD16ET
file.list = list.files(indir)
#file.MOD16ET = "MOD16ET-2011-2012-9towers.txt"
file.MOD16ET = "MOD16ET-2011-2012-9sites-buffer.txt"
setwd(indir)
MOD16ET = read.table(file.MOD16ET)
MOD16ET.yyyyjjj = as.numeric(rownames(MOD16ET))
MOD16ET$date = strptime(MOD16ET.yyyyjjj,format="%Y%j")
MOD16ET.yr = as.numeric(format(MOD16ET$date,"%Y"))

#  Load LST
file.LST = "MOD11A1LST.2011.2012.towers.buffer.txt"
LST = read.table(file.LST)

# Load EVI
file.EVI = "MOD13A2EVI.2010.2012.towers.buffer.txt"
EVI = read.table(file.EVI)

# Load LSWI
#file.LSWI = "MOD09LSWI-2000-2013-9towers.txt"  # No buffer, based on ts from resampled grid subsets
file.LSWI = "LSWI.2010.2012.towers.buffer.txt"
LSWI = read.table(file.LSWI)
rownames(LSWI)=LSWI$yyyyjjj # only if using "LSWI.2010.2012.towers.buffer.txt"

# Load biomass dates
indir.biomass = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/other_data_fluxtowers/"
fname.biomass = "biomass_height_fvc_from_michael_with_dates.xlsx"
wb.bio = loadWorkbook(paste(indir.biomass,fname.biomass,sep=""))
x.bio = readWorksheet(wb.bio,"R")


#  Load water level data
loadwaterlevel <- function(fname) {
	wb = loadWorkbook(paste(indir.wl,fname,sep=""))
	x = readWorksheet(wb,"R")
	yyyymmdd = substr(x$TIMESTAMP,1,8)
	dates = as.Date(yyyymmdd,format="%Y%m%d")
	x[x[,2]==-9999,2]=NA

	daymean = aggregate(x[,2],by=list(dates),FUN="mean")
	#plot(daymean[,1],daymean[,2],type="l",xlab="",ylab="Water level, m")
	#abline(h=0,lty=4)
	names(daymean)=c("Date","WL.m")
	return(daymean)
}

indir.wl = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/"
wl.2011 = loadwaterlevel("AMF_USTWT_2011_L1_GF_V001_water_level_only.xlsx")
wl.2012 = loadwaterlevel("AMF_USTWT_2012_L1_GF_V001_water_level_only.xlsx")
wl = data.frame(rbind(wl.2011,wl.2012))

colors = c("gray","black","grey80")

plot.EVILST.ETerror <- function(year,i,leg) {
	files.flux.in = list.files(indir.flux,pattern=as.character(year))
	files.flux = files.flux.in[grep("~",files.flux.in,invert=TRUE)]
	fluxfile = files.flux[grep(towers[i],files.flux)]
	MOD16ETsub = MOD16ET[MOD16ET.yr==year,]

	# READ IN SEBAL
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

	# READ IN FLUX TOWER DATA
	dflux = read.table(paste(indir.flux,fluxfile,sep=""),header=TRUE)
  	dflux.dates = strptime(dflux$Date,"%Y-%m-%d")

	file.flux.split = strsplit(fluxfile,".",fixed=TRUE)
  	flux.code = paste(file.flux.split[[1]][1],file.flux.split[[1]][2],sep=".")
  	flux.label = paste(flux.code,file.flux.split[[1]][3],sep=" ")
  	matchcol.MOD16 = match(flux.code,names(MOD16ETsub))
  	matchcol.SEBAL = match(flux.code,names(SEBALET.daily))
	matchcol.EVI = match(flux.code,names(EVI))
	matchcol.LST = match(flux.code,names(LST))
	matchcol.LSWI = match(flux.code,names(LSWI))
	SEBALET.daily[SEBALET.daily$fract.notNA<0.75,matchcol.SEBAL]=NA  #  *** Screen for low data availability
	
	#year = as.numeric(format(as.Date(start.season),"%Y"))
	labeltext = c("03-01","05-01","07-01","09-01","11-01")
	labeldates = as.Date(paste(year,labeltext,sep="-"),format="%Y-%m-%d")
      legendcoords = as.Date(paste(year,"09-01",sep="-"),format="%Y-%m-%d")
	xlims = as.Date(c(paste(year,"-03-01",sep=""),paste(year,"-11-01",sep="")),format="%Y-%m-%d")
	#ylabels = seq(ylims[1],ylims[2],by=2)

	# CALCULATE 8-DAY MOVING AVERAGES FOR ET
	ma <- function(x,n){filter(x,rep(1/n,n), sides=2)}
  	SEBALET = data.frame(Date=SEBAL.daily.dates,ET=SEBALET.daily[,matchcol.SEBAL])
  	SEBALET.noNA = SEBALET[!is.na(SEBALET$ET),]
  	SEBAL.MA = ma(x=SEBALET.noNA$ET,n=8)
    	fluxET = data.frame(Date=dflux.dates,ET=dflux$Eta.mm.day)
  	fluxET.noNA = fluxET[!is.na(fluxET$ET),]
  	fluxET.MA = ma(x=fluxET.noNA$ET,n=8)

	# CALCULATE ERROR
	match.dates.MOD16.flux = match(as.Date(MOD16ETsub$date),as.Date(fluxET.noNA$Date))
	error.mod16 = MOD16ETsub[,matchcol.MOD16]-fluxET.MA[match.dates.MOD16.flux]
	match.dates.SEBAL.flux = match(as.Date(SEBALET.noNA$Date),as.Date(fluxET.noNA$Date))
	error.SEBAL = SEBAL.MA-fluxET.MA[match.dates.SEBAL.flux]

	# PLOT EVI, LST IN TOP PLOT
	par(mar=c(0,4.5,1.5,4.5))
	plot(as.Date(strptime(EVI$yyyyjjj,format="%Y%j")),EVI[,matchcol.EVI],type="l",xlim=as.Date(xlims),xlab="",xaxt="n",ylim=c(-0.6,1),lwd=2,las=1,ylab="",xaxs="i")
	axis(1,at=as.Date(labeldates),labels=NA)
	mtext("EVI, LSWI",side=2,line=3,cex=cexval)
	lines(as.Date(strptime(rownames(LSWI),format="%Y%j")),LSWI[,matchcol.LSWI],col="grey80",lwd=2)
	abline(0,0,lty=2)
	mtext(paste("  ",towers[i],year,sep=" "),side=3,line=-1.7,cex=cexval)
	mtext(paste("   ",crops[i],sep=""),side=3,line=-2.9,cex=cexval)
	par(new=TRUE)
	plot(as.Date(strptime(LST$yyyyjjj,format="%Y%j")),LST[,matchcol.LST],type="l",col="black",lwd=1,xlim=as.Date(xlims),xaxt="n",xlab="",yaxt="n",ylab="",ylim=c(10,50),las=1,xaxs="i")
	axis(4,las=1)
	mtext(expression(paste("T"["R"]," "^"o","C",sep="")),side=4,line=2.5,cex=cexval)
	#mtext("LST C",side=4,line=2.0,cex=cexval)
	if(leg==1){
	legend(as.numeric(legendcoords),23,c("EVI","LSWI","LST"),col=c("black","grey","black"),lwd=c(2,2,1),bty="n")
	#legend("bottom",c("EVI","LSWI","LST"),col=c("black","grey","black"),lwd=c(2,2,1),bty="n")
	}

	#  Plot water level data
	wl.years = format(wl$Date,"%Y")
	wl.sub = wl[wl.years==year,]
	par(new=FALSE)
	par(mar=c(0,4.5,0,4.5))
	plot(as.Date(wl.sub$Date),wl.sub$WL.m,type="l",xlim=as.Date(xlims),ylim=c(-0.85,0.2),xlab="",xaxt="n",las=1,ylab="",xaxs="i")
	axis(1,at=as.Date(labeldates),labels=NA)
	mtext("Water level, m",side=2,line=3,cex=cexval)
	abline(h=0,lty=2)
	
	# Add arrows
	date.cols = grep(year,names(arrow.dates))
	arrows(as.Date(arrow.dates[,date.cols[1]]),rep(arrow.lims[2],times=dim(arrow.dates[date.cols[1]])[1]),as.Date(arrow.dates[,date.cols[1]]),y1=rep(arrow.lims[1],times=dim(arrow.dates[date.cols[1]])[1]),length=0.05)
	text(as.Date(arrow.dates[,date.cols[1]]),rep(arrow.lims[2]+0.05,times=dim(arrow.dates[date.cols[1]])[1]),arrow.dates[,date.cols[2]])

	# PLOT ET TIME SERIES
	par(mar=c(1.5,4.5,0,4.5))
	plot(as.Date(MOD16ETsub$date),MOD16ETsub[,matchcol.MOD16],type="l",col=colors[2],lwd=4,xlim=xlims,xlab="",ylim=c(0,12),ylab="",las=1,xaxs="i",yaxs="i")
	lines(as.Date(SEBAL.daily.dates),SEBALET.daily[,matchcol.SEBAL],type="l",col=colors[1],lwd=1)
	lines(as.Date(SEBALET.noNA$Date),SEBAL.MA,col=colors[1],lwd=4)
	lines(as.Date(fluxET.noNA$Date),fluxET.MA,col=colors[2],lwd=2)
  	points(as.Date(fluxET$Date),fluxET$ET)
	ylims.error = c(floor(min(min(error.SEBAL,na.rm=TRUE),min(error.mod16,na.rm=TRUE))),ceiling(max(max(error.SEBAL,na.rm=TRUE),max(error.mod16,na.rm=TRUE))))
	ylims.error = c(-6,6)
	mtext("ET, mm/day",side=2,line=3,cex=cexval)
	#mtext(paste("   ",towers[i],sep=""),side=3,adj=0,line=-1.5,cex=cexval)
	#mtext(paste("   ",crops[i],sep=""),side=3,adj=0,line=-2.5,cex=cexval)
	if (leg==1){
	legend("topleft",c("SEBAL","MOD16","Tower"),col=c("grey","black","black"),lwd=c(4,4,2),bty="n",pch=c(NA,NA,1))
	}
	# ADD ARROWS FOR GROWTH STAGE
	x.bio.sub = x.bio[x.bio$Tower==towers[i] & x.bio$Year==year,]
	arrows(as.Date(x.bio.sub$SPR),arrow2.lims[i,1],as.Date(x.bio.sub$SPR),arrow2.lims[i,2],length=0.05)
	arrows(as.Date(x.bio.sub$FLW),arrow2.lims[i,1],as.Date(x.bio.sub$FLW),arrow2.lims[i,2],length=0.05)
	arrows(as.Date(x.bio.sub$GB),arrow2.lims[i,1],as.Date(x.bio.sub$GB),arrow2.lims[i,2],length=0.05)
	text(c(as.Date(x.bio.sub$SPR),as.Date(x.bio.sub$FLW),as.Date(x.bio.sub$GB)),rep(arrow2.lims[i,1]+0.5,times=3),c("SPR","FLW","GB"))
}

#  Load water level data

indir.SEBAL = indir.SEBAL.list[9]  # 9 = q0.005, c7.97 d-7 (z0m parameter set 4)

cexval = 0.9
towers = c("StaD","StaW","Wil","US.Twt","Fiv","Big")
crops = c("Corn","Corn","Rice","Rice","Cotton","Rice")

arrow.lims = c(0.07,0.12)
arrow2011.text = c("FL","DR","PL","FL","DR","FL","DR")
arrow2011.date = as.Date(c("2011-03-01","2011-04-05","2011-04-22","2011-05-11","2011-05-21","2011-06-15","2011-09-15"),format="%Y-%m-%d")
arrow2012.date = as.Date(c("2012-03-26","2012-04-22","2012-05-17","2012-06-18","2012-09-30",NA,NA),format="%Y-%m-%d")
arrow2012.text = c("FL","DR","PL","FL","DR",NA,NA)

arrow2.lims = rbind(c(9,7),c(NA,NA),c(9,8),c(10.5,9),c(9,7),c(9,7))

arrow.dates = data.frame(y2011=arrow2011.date,y2012=arrow2012.date,txt2011=arrow2011.text,txt2012=arrow2012.text)

#dev.new(height=8,width=12)
#par(mfcol=c(3,2),oma=c(2,0,0,0),mai=c(0,1,0,1))
#plot.EVILST.ETerror(year=2011,i=4,leg=1)
#plot.EVILST.ETerror(year=2012,i=4,leg=1)

setwd(outdir.figs)
postscript("figure_5_ts_UStwt_w_water_levelg.eps",height=7,width=10,horizontal=TRUE)
par(mfcol=c(3,2),oma=c(5.5,4.5,3.5,3.5),mai=c(0,1,0,1))
plot.EVILST.ETerror(year=2011,i=4,leg=1)
plot.EVILST.ETerror(year=2012,i=4,leg=1)
dev.off()


