#  Load NDVI time series at towers, xbiomass dates.
#  Determine relationship between height and NDVI
#  Compare with the default relationship in Morse et al 2000 eq 9.1

library(XLConnect)

# Load NDVI
indir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
file.LSWI = list.files(indir,patt="LSWI")
file.NDVI = "MOD13A2NDVI.2010.2012.towers.buffer.txt"
#file.NDVI = "MOD13A2NDVI.2010.2012.towers.txt"
file.EVI = "MOD13A2EVI.2010.2012.towers.buffer.txt"
NDVI = read.table(paste(indir,file.NDVI,sep=""))
EVI = read.table(paste(indir,file.EVI,sep=""))

#NDVI = EVI  # Try using EVI

#NDVI.z0m padfsafdssdfafadsdfsfdsyyyyjjj = NDVI$yyy
NDVI$Date = strptime(NDVI$yyyyjjj,format="%Y%j")
NDVI.yr = as.numeric(format(NDVI$Date,"%Y"))
tower.names = names(NDVI)

#year = 2012
#text.coord = as.Date(paste(year,"-01-01",sep=""),format="%Y-%m-%d")
#LSWIsub = LSWI[LSWI.yr==year,]
#NDVIsub = NDVI[NDVI.yr==year,]

indir.biomass = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/other_data_fluxtowers/"
fname.biomass = "biomass_height_fvc_from_michael_with_dates.xlsx"
fname.biomass.ht = "biomass_height_fvc_from_michael.xlsx"
wb.bio = loadWorkbook(paste(indir.biomass,fname.biomass,sep=""))
x.bio = readWorksheet(wb.bio,"R")
wb.ht = loadWorkbook(paste(indir.biomass,fname.biomass.ht,sep=""))
x.ht = readWorksheet(wb.ht,"R")
class(x.ht$h.SPR)="numeric"  # read as character due to NA

out.df = data.frame(Tower=x.bio$Tower,Year=x.bio$Year,NDVI.SPR=NA,NDVI.FLW=NA,NDVI.GB=NA)
#  For each station, find NDVI on the sampled dates
for (row in 1:length(x.bio[,1])){
	for (col in 3:5){
		#print(c(row,col))
		#flush.console()
		tower = x.bio$Tower.ID.full[row]
		datediff = abs(as.numeric(x.bio[row,col])-as.numeric(NDVI$Date))
		NDVI.matchcol = grep(tower,tower.names)
		NDVI.date = NDVI[which(min(datediff)==datediff),NDVI.matchcol][1]
		out.df[row,col]=NDVI.date
	}
}

maxh = 3.5 # max veg ht
maxz0m = 3.5*0.123
NDVI.new=seq(0,1,by=0.01)
afit.vec = c(1,4.3,5.5,7.97)
bfit.vec = c(-3.1,-4.6,-4.0,-7.0)
z0m.eqs = matrix(NA,length(NDVI.new),length(afit.vec))
for (p in 1:length(afit.vec)){
	z0m.eqs[,p] = exp(afit.vec[p]*NDVI.new+bfit.vec[p])
	z0m.eqs[z0m.eqs[,p]>maxz0m,p] = maxz0m
}

cornsub = c(3,4)
ricesub = c(1,2,5,6)
cotsub = c(7,8)




#  Plot
outdir.figs = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/figures/"

plottofile = 1
if (plottofile==1){
	setwd(outdir.figs)
	postscript("figure_3_NDVI_vs_z0m_final_eqs.eps",height=5,width=5,horizontal=FALSE)
} else {
	dev.new(height=5,width=5)
}

txtcexval = 0.8
par(oma=c(0.5,0.5,0,0))
#par(oma=c(0,0,0,0))
plot(out.df$NDVI.GB[cornsub],(0.123*x.ht$h.GB/100)[cornsub],xlab="",ylab="",xlim=c(0,1),ylim=c(0,0.5),col="black",las=1,pch=1,xaxs="i",yaxs="i",cex.axis=txtcexval,cex.lab=txtcexval+0.1)

mtext("NDVI",side=1,cex=txtcexval*1.2,line=2)
mtext("z0m, m",side=2,cex=txtcexval*1.2,line=2.25)

points(out.df$NDVI.GB[ricesub],(0.123*x.ht$h.GB/100)[ricesub],col="grey",pch=1)
points(out.df$NDVI.GB[cotsub],(0.123*x.ht$h.GB/100)[cotsub],col="grey", pch=16)

# FLW STAGE
points(out.df$NDVI.FLW[cornsub],(0.123*x.ht$h.FLW/100)[cornsub],pch=0,col="black")
points(out.df$NDVI.FLW[ricesub],(0.123*x.ht$h.FLW/100)[ricesub],col="grey",pch=0)
points(out.df$NDVI.FLW[cotsub],(0.123*x.ht$h.FLW/100)[cotsub],col="grey", pch=15)

# SPR STAGE
points(out.df$NDVI.SPR[cornsub],(0.123*x.ht$h.SPR/100)[cornsub],pch=2,col="black")
points(out.df$NDVI.SPR[ricesub],(0.123*x.ht$h.SPR/100)[ricesub],col="grey",pch=2)
points(out.df$NDVI.SPR[cotsub],(0.123*x.ht$h.SPR/100)[cotsub],col="grey", pch=17)

lty.vec = c(1,1,2,2)
col.vec = c("black","grey","black","grey")

for (p in 1:length(afit.vec)){
	lines(NDVI.new,z0m.eqs[,p],lty=lty.vec[p],col=col.vec[p])
}

xdif = 0.05
xoff = 0.045
xpoints = c(0,1,2)*xdif+xoff
ydif = 0.035
yoff = 0.28
ypoints = c(0,-1,-2)*ydif+yoff
legend.x.points = matrix(xpoints,ncol=3,nrow=3)
legend.y.points = t(matrix(ypoints,ncol=3,nrow=3))

pchvec = matrix(c(2,0,1,2,0,1,17,15,16),nrow=3,ncol=3,byrow=TRUE)
colvec = c("black","grey","grey")

for (i in 1:length(legend.x.points[,1])){
	for (j in 1:length(legend.y.points[1,])){
		print(c(i,j,legend.x.points[i,j],legend.y.points[i,j]))
		flush.console()
		points(legend.x.points[i,j],legend.y.points[i,j],pch=pchvec[i,j],col=colvec[i])
	}
}

text(xpoints,ypoints[1]+ydif*1.1,labels=c("M","R","C"),cex=txtcexval)
text(xpoints[3]+xdif*2,ypoints,labels=c("SPR","FLW","GB"),cex=txtcexval)

#legend("topleft",legend=as.character(seq(1,4)),lty=lty.vec,col=col.vec,bty="n",title="z0m Parameter set",cex=txtcexval)
legtext = c(paste0(seq(1,4),". c=",as.character(format(afit.vec,digits=3))," d=",as.character(format(bfit.vec,digits=3))))
legend("topleft",legend=legtext,lty=lty.vec,col=col.vec,bty="n",title="z0m Parameter set",cex=txtcexval)

if (plottofile==1){
	dev.off()
}


# _____________________________
# END MAIN PLOT; below is extra text















par(new=TRUE)
plot(NDVI.new,z0m.morse.2/0.123,type="l",lwd=2,col="black",xaxt="n",yaxt="n",ylab="",xlab="",xlim=c(0,1),ylim=c(0,1/0.123),las=1)
axis(4)
mtext("Height, m",side=4,line=2)

#  OLD CODE
afit = 5.9
bfit = -3.3 #  Morse et al curve 21 (p 95), for forest
z0m.morse.forest = exp(afit*NDVI.new+bfit)
z0m.morse.forest[z0m.morse.forest>maxz0m]=maxz0m

# Morse et al curve 21 (p 95), for cropland
ba2 = c(-3.3356,0.9648)
z0m.morse.crops = exp(ba2[1]+ba2[2]*NDVI.new)
z0m.morse.crops[z0m.morse.crops>maxz0m]=maxz0m


# lm fits
lm.towers.GB = lm(log(0.123*x.ht$h.GB/100)~out.df$NDVI.GB)
ba = coefficients(lm.towers)
z0m.pred.GB = exp(ba[1]+ba[2]*NDVI.new)
z0m.pred.GB[z0m.pred.GB>maxz0m]=maxz0m

lm.towers.FLW = lm(log(0.123*x.ht$h.FLW/100)~out.df$NDVI.FLW)
ba = coefficients(lm.towers.FLW)
z0m.pred.FLW = exp(ba[1]+ba[2]*NDVI.new)
z0m.pred.FLW[z0m.pred.FLW>maxz0m]=maxz0m

z0m.orig.SEBAL.runs = exp(-8.12+7.97*NDVI.new)  # Original in Messina SEBAL code
z0m.orig.SEBAL.runs[z0m.orig.SEBAL.runs>maxz0m]=maxz0m
lines(NDVI.new,z0m.orig.SEBAL.runs,lwd=2,lty=5,col="blue")
