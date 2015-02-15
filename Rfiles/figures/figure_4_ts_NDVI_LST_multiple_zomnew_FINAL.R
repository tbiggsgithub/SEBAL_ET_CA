# Plot time series of:
  #  Top:  EVI, LST at corn and rice site
#    Bottom:  Error in ET (mm/day) for MOD16 and SEBAL

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

#### END LOADING DATA  ####

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
      legendcoords = as.Date(paste(year,"09-18",sep="-"),format="%Y-%m-%d")
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
	axis(1,at=as.Date(labeldates),labels=NA,tck=0.03)
	mtext("EVI, LSWI",side=2,line=2.5,cex=cexval)
	lines(as.Date(strptime(rownames(LSWI),format="%Y%j")),LSWI[,matchcol.LSWI],col="grey80",lwd=2)
	abline(0,0,lty=2)
	mtext(paste("   ",towers[i],sep=""),side=3,adj=0,line=-2.5,cex=cexval)
	mtext(paste("   ",crops[i],sep=""),side=3,adj=0,line=-3.5,cex=cexval)
	par(new=TRUE)
	plot(as.Date(strptime(LST$yyyyjjj,format="%Y%j")),LST[,matchcol.LST],type="l",col="black",lwd=1,xlim=as.Date(xlims),xaxt="n",xlab="",yaxt="n",ylab="",ylim=c(10,50),las=1,xaxs="i")
	axis(4,las=1)
	mtext(expression(paste("T"["R"]," "^"o","C",sep="")),side=4,line=2.5,cex=cexval)
	#mtext("LST C",side=4,line=2.0,cex=cexval)
	if(leg==1){
	legend(as.numeric(legendcoords),23,c("EVI","LSWI","LST"),col=c("black","grey","black"),lwd=c(2,2,1),bty="n")
	#legend("bottom",c("EVI","LSWI","LST"),col=c("black","grey","black"),lwd=c(2,2,1),bty="n")
	}

	#  ADD ARROWS
	arrows(as.Date(arrow1.date[i]),arrow1.lims[i,1],as.Date(arrow1.date[i]),arrow1.lims[i,2],length=0.05)
	text(as.Date(arrow1.date[i]),arrow1.lims[i,1]+2,arrow.text[i])
	if (i==5){  # Irrigation dates for cotton
		for (j in 1:length(cotton.irrig.dates)){
			arrows(cotton.irrig.dates[j],arrow1.lims[i,1],cotton.irrig.dates[j],arrow1.lims[i,2],length=0.05)
		}
	}
	if (i==1){  # Irrigation dates for corn
		for (j in 1:length(corn.irrig.dates)){
			arrows(corn.irrig.dates[j],arrow1.lims[i,1],corn.irrig.dates[j],arrow1.lims[i,2],length=0.05)
		}
	}

	# PLOT ET TIME SERIES
	par(mar=c(1.5,4.5,0,4.5))
	plot(as.Date(MOD16ETsub$date),MOD16ETsub[,matchcol.MOD16],type="l",col=colors[2],lwd=4,xlim=xlims,xlab="",ylim=c(0,10),ylab="",las=1,xaxs="i",yaxs="i")
	lines(as.Date(SEBAL.daily.dates),SEBALET.daily[,matchcol.SEBAL],type="l",col=colors[1],lwd=1)
	lines(as.Date(SEBALET.noNA$Date),SEBAL.MA,col=colors[1],lwd=4)
	lines(as.Date(fluxET.noNA$Date),fluxET.MA,col=colors[2],lwd=2)
  	points(as.Date(fluxET$Date),fluxET$ET)
	ylims.error = c(floor(min(min(error.SEBAL,na.rm=TRUE),min(error.mod16,na.rm=TRUE))),ceiling(max(max(error.SEBAL,na.rm=TRUE),max(error.mod16,na.rm=TRUE))))
	ylims.error = c(-6,6)
	mtext("ET, mm/day",side=2,line=2.5,cex=cexval)
	mtext(paste("   ",towers[i],sep=""),side=3,adj=0,line=-2.5,cex=cexval)
	mtext(paste("   ",crops[i],sep=""),side=3,adj=0,line=-3.5,cex=cexval)
	if (leg==1){
	legend(as.numeric(legendcoords),9.5,c("SEBAL","MOD16","Tower"),col=c("grey","black","black"),lwd=c(4,4,2),bty="n",pch=c(NA,NA,1))
	}

	#  ADD ARROWS, GROWTH STAGE
	
	x.bio.sub = x.bio[x.bio$Tower==towers[i] & x.bio$Year==year,]
	arrows(as.Date(x.bio.sub$SPR),arrow2.lims[i,1],as.Date(x.bio.sub$SPR),arrow2.lims[i,2],length=0.05)
	arrows(as.Date(x.bio.sub$FLW),arrow2.lims[i,1],as.Date(x.bio.sub$FLW),arrow2.lims[i,2],length=0.05)
	arrows(as.Date(x.bio.sub$GB),arrow2.lims[i,1],as.Date(x.bio.sub$GB),arrow2.lims[i,2],length=0.05)
	text(c(as.Date(x.bio.sub$SPR),as.Date(x.bio.sub$FLW),as.Date(x.bio.sub$GB)),rep(arrow2.lims[i,1]+0.5,times=3),c("SPR","FLW","GB"))
	if (i==1){
		arrows(as.Date("2012-04-22",format="%Y-%m-%d"),arrow2.lims[i,1],y1=arrow2.lims[i,2],length=0.05)
		text(as.Date("2012-04-22",format="%Y-%m-%d"),arrow2.lims[i,1]+0.5,"Planting")
	}
	#arrows(as.Date(arrow1.date[i]),arrow2.lims[i,1],as.Date(arrow1.date[i]),arrow2.lims[i,2],length=0.05)
	#text(as.Date(arrow1.date[i]),arrow2.lims[i,1]+0.5,arrow.text[i])
	#if (i==5){  # Irrigation dates for cotton
	#	for (j in 1:length(cotton.irrig.dates)){
	#		arrows(cotton.irrig.dates[j],arrow2.lims[i,1],cotton.irrig.dates[j],arrow2.lims[i,2],length=0.05)
	#	}
	#}
	#if (i==1){  # Irrigation dates for corn
	#	for (j in 1:length(corn.irrig.dates)){
	#		arrows(corn.irrig.dates[j],arrow2.lims[i,1],corn.irrig.dates[j],arrow2.lims[i,2],length=0.05)
	#	}
	#	arrows(as.Date("2012-04-22",format="%Y-%m-%d"),arrow2.lims[i,1],y1=arrow2.lims[i,2],length=0.05)
	#	text(as.Date("2012-04-22",format="%Y-%m-%d"),arrow2.lims[i,1]+0.5,"Planting")
	#}

}
f=9
indir.SEBAL = indir.SEBAL.list[f]
print(indir.SEBAL)
#indir.SEBAL = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/2014_11_large_area_mcd43a_nowater_urban_north_005/new_z0m_formula_afit4.3.bfit.minus.4.6/"

cexval = 0.7
towers = c("StaD","StaW","Wil","US.Twt","Fiv","Big")
crops = c("Corn","Corn","Rice","Rice","Cotton","Rice")

arrow1.lims = rbind(c(48,44),c(NA,NA),c(48,40),c(NA,NA),c(48,43),c(48,38))
arrow2.lims = rbind(c(9,7),c(NA,NA),c(9,8),c(NA,NA),c(9,7),c(9,7))
arrow.text = c("Irrigated",NA,"Flooded",NA,"Irrigated","Flooded")

cotton.irrig.dates = as.Date(c("2011-05-21","2011-05-27","2011-06-03","2011-06-08","2011-06-14","2011-06-21","2011-07-07","2011-07-19","2011-07-23","2011-07-25","2011-07-29","2011-08-08","2011-08-19","2011-08-24","2011-08-31","2011-09-06"),format="%Y-%m-%d")
corn.irrig.dates = as.Date(c("2012-07-09","2012-07-16","2012-07-23","2012-07-30","2012-08-04"),format="%Y-%m-%d")
arrow1.date = as.Date(c(NA,NA,"2012-05-20",NA,NA,"2011-05-15"),format="%Y-%m-%d")
arrow1.date[1] = corn.irrig.dates[1]
arrow1.date[5] = cotton.irrig.dates[1]

abcdline.top = 9.8

setwd(outdir.figs)
postscript("figure_4_ts_EVILSWI_ET.eps",height=7,width=11,horizontal=TRUE)



# dev.new(height=7,width=11)  # turn on for test plots
par(mfcol=c(4,2),oma=c(2,0,0,0),mai=c(0,1,0,1))
# Corn
plot.EVILST.ETerror(year=2012,i=1,leg=1)
mtext(" a)",side=3,adj=0,line=abcdline.top,cex=1.1)
mtext(" b)",side=3,adj=0,line=-1.5,cex=1.1)
# Rice USTwt
# plot.EVILST.ETerror(year=2012,i=4,leg=0)

# Rice Big
plot.EVILST.ETerror(year=2011,i=6,leg=0)
mtext(" c)",side=3,adj=0,line=abcdline.top,cex=1.1)
mtext(" d)",side=3,adj=0,line=-1.5,cex=1.1)
#arrows(as.Date("2011-05-15",format="%Y-%m-%d"),0.75,as.Date("2011-05-15",format="%Y-%m-%d"),0.4)

#dev.new(height=8,width=5)
#par(mfrow=c(4,1),oma=c(3,5,0,4),mai=c(0,0,0,0))
#  Cotton
indir.SEBAL = indir.SEBAL.list[f+15]
print(indir.SEBAL)
plot.EVILST.ETerror(year=2011,i=5,leg=1)
mtext(" e)",side=3,adj=0,line=abcdline.top,cex=1.1)
mtext(" f)",side=3,adj=0,line=-1.5,cex=1.1)
#  Rice
indir.SEBAL = indir.SEBAL.list[1]
plot.EVILST.ETerror(year=2012,i=3,leg=1)
mtext(" g)",side=3,adj=0,line=abcdline.top,cex=1.1)
mtext(" h)",side=3,adj=0,line=-1.5,cex=1.1)
dev.off()




