#  Compare Rn from GMAO with Rn from fluxtowers
#  Extracted Rn from "extract_Rn_fluxtowers.R"
#  Rn is 24-hour averaged from GMAO MERRA (vs layer 1)

library(XLConnect)
outdir.figs = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/figures/"

plotem <- function(file.fluxtower,start.date.txt,i,plotnum) {
start.date = as.Date(start.date.txt,"%Y-%m-%d")
year = format(start.date,"%Y")
end.date = as.Date(paste(year,"-10-15",sep=""),format="%Y-%m-%d")
fname.Rn = paste("Rn_GMAO_", as.character(year), "_UStowers_24hmean.txt",sep="")
Rn.GMAO = read.table(paste(indir.Rn,fname.Rn,sep=""))
Rn.GMAO.dates = strptime(rownames(Rn.GMAO),format="%Y%j")
tower = towers[i]

flux.wb <- loadWorkbook(paste(indir.flux,file.fluxtower,sep=""))
flux.data.in = readWorksheet(flux.wb, sheet = "R")
flux.data = flux.data.in[grep(year,flux.data.in[,1]),]
flux.dates = strptime(flux.data$Date,format="%Y-%m-%d")
flux.Rn.MJ = as.numeric(flux.data$Rn)
flux.Rn.Wm2 = 11.574*flux.Rn.MJ
flux.Rn.Wm2[flux.Rn.Wm2<0] = NA

#  Match individual values of GMAO and fluxtowers
match.index = match(flux.dates,Rn.GMAO.dates)
match.index2 = match(Rn.GMAO.dates,flux.dates)
match.col = match(tower,names(Rn.GMAO))
diff.flux.GMAO = Rn.GMAO[,match.col]-flux.Rn.Wm2[match.index2]
diff.mmday = diff.flux.GMAO/28.4  # 

#match.tower.GMAO = match(flux.dates,Rn.GMAO.dates)
match.tower.GMAO = match(Rn.GMAO.dates,flux.dates)
Rn.flux.match = flux.Rn.Wm2[match.tower.GMAO]

#  Scatter plot of GMAO vs tower
#startdate = strptime(paste(year,"-05-01",sep=""),format="%Y-%m-%d")
ylabels = seq(ylims[1],ylims[2],by=50)
#labeltext = c("03-01","05-01","07-01","09-01","11-01")
#labeltext.letters = c("Mar","May","Jul","Sep","Nov")
#labeldates = as.Date(paste(year,labeltext,sep="-"),format="%Y-%m-%d")
Rn.GMAO[(as.Date(Rn.GMAO.dates)<start.date)|(as.Date(Rn.GMAO.dates)>end.date),match.col]=NA  # For seasonal average
 
# SCATTERPLOT
if (plotnum==1){
	plot(Rn.GMAO[,match.col],Rn.flux.match,pch=plot.pch[i],xlab=xlabel,ylab=ylabel,ylim=ylims,xlim=ylims,cex=plotcex)
	abline(0,1,lty=3)
} else {
	points(Rn.GMAO[,match.col],Rn.flux.match,col=plot.colors[i],pch=plot.pch[i],cex=plotcex)
}

#if ((plotnum==5) & (plot.to.file == 1)){
#	dev.off()
#}

meanbias.May.Oct = round(mean(Rn.GMAO[,match.col]-Rn.flux.match,na.rm=TRUE),1)
relbias.May.Oct = round(meanbias.May.Oct/mean(Rn.flux.match,na.rm=TRUE),2)
RMSE.May.Oct = round(sqrt(mean((Rn.GMAO[,match.col]-Rn.flux.match)^2,na.rm=TRUE)),1)
rRMSE.May.Oct = round(sqrt(mean(((Rn.GMAO[,match.col]-Rn.flux.match)/Rn.flux.match)^2,na.rm=TRUE)),2)

Rn.GMAO[Rn.GMAO.dates<strptime(paste(year,"-07-01",sep=""),format="%Y-%m-%d"),match.col]=NA
meanbias.Jul.Oct = round(mean(Rn.GMAO[,match.col]-Rn.flux.match,na.rm=TRUE),1)
relbias.Jul.Oct = round(meanbias.May.Oct/mean(Rn.flux.match,na.rm=TRUE),2)
RMSE.Jul.Oct = round(sqrt(mean((Rn.GMAO[,match.col]-Rn.flux.match)^2,na.rm=TRUE)),1)
rRMSE.Jul.Oct = round(sqrt(mean(((Rn.GMAO[,match.col]-Rn.flux.match)/Rn.flux.match)^2,na.rm=TRUE)),2)

return(c(mean(Rn.GMAO[,match.col],na.rm=TRUE),mean(Rn.flux.match,na.rm=TRUE),meanbias.May.Oct,relbias.May.Oct,RMSE.May.Oct,rRMSE.May.Oct,meanbias.Jul.Oct,relbias.Jul.Oct,RMSE.Jul.Oct,rRMSE.Jul.Oct))
#return(c(meanbias.May.Oct,relbias.May.Oct,RMSE.May.Oct,rRMSE.May.Oct,meanbias.Jul.Oct,relbias.Jul.Oct,RMSE.Jul.Oct,rRMSE.Jul.Oct))
}
# END PLOTEM FUNCTION

indir.Rn = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/"

plot.colors = c("black","grey","black","grey","black","grey")
plot.pch = c(16,16,1,1,16,16)  # Why six plotting variables but only 4 towers plotted???? 2 years?

plot.to.file = 0
xlabel = expression("Rn GMAO W m"^"-2")
ylabel = expression("Rn Tower W m"^"-2")
plotcex = 0.7

# WILSON, BIG, TWT, AND FIV

towers = c("CA.Wil","CA.Big","US.Twt","CA.Fiv","CA.StaD","CA.StaW")
towers.legend = c("Wil","Big","Twt","Fiv","StaD","StaW")
ylims=c(0,250)
nplots = 7  # NUMBER OF SERIES THAT WILL BE PLOT, TOTAL FOR BOTH PLOTS
stats = matrix(NA,nplots,10)
rownames(stats)=c("CA.Wil","Ca.Big","US.Twt 2012","US.Twt 2011", "CA.Fiv", "StaD","StaW")
colnames(stats) = c("RnGMAO","RnTower","Bias.May.Oct","Relbias.May.Oct","RMSE.May.Oct","rRMSE.May.Oct","Bias.Jul.Oct","Relbias.Jul.Oct","RMSE.Jul.Oct","rRMSE.Jul.Oct")

if (plot.to.file == 1){
	setwd(outdir.figs)
	postscript("figure_2_Rn_tower_GMAO.eps",height=5,width=5,horizontal=FALSE)
} else {
    	dev.new(height=5,width=5)
}

stats[1,] = plotem(file.fluxtower="CA-Wil_Rice_DailyET_Williams(Bransford2012)_RIWILET.xlsx",start.date.txt="2012-06-17",i=1,plotnum=1)
stats[2,] = plotem(file.fluxtower="CA-Big_DailyETEastRice(Boeger)2011.xlsx", start.date.txt="2011-07-01", i=2,plotnum=2)
stats[3,] = plotem(file.fluxtower="US_twt_rice_Twitchell_Rice_ET_2009_2013.xlsx", start.date.txt="2012-05-01", i=3,plotnum=3)
stats[4,] = plotem(file.fluxtower="US_twt_rice_Twitchell_Rice_ET_2009_2013.xlsx", start.date.txt="2011-05-01", i=3,plotnum=4)
stats[5,] = plotem(file.fluxtower="CA-Fiv_Cotton SR Farming D 2011 VWC corrected_5points.xlsx", start.date.txt="2011-06-01",i=4,plotnum=5)
legend("topleft",legend=towers.legend[1:4],col=plot.colors[1:4],pch=plot.pch[1:4],bty="n")
dev.off()
# ___________ END FIGURE WITH FOUR SERIES ________________________



#  run these two lines to get all 6 time series on one plot, or don't run to keep scatterplot with just four series
stats[6,] = plotem(file.fluxtower="CA-StaD_ET_staten_island.xlsx",start.date.txt="2012-05-01",i=5,plotnum=6)
stats[7,] = plotem(file.fluxtower="CA-StaW_ET_staten_island.xlsx", start.date.txt="2012-05-01", i=6,plotnum=7)


stats.df=as.data.frame(stats)
meanbias = stats.df$RnGMAO - stats.df$RnTower
relmeanbias = 100*meanbias/stats.df$RnTower

#  Separate plot for STAD STAW
stats[6,] = plotem(file.fluxtower="CA-StaD_ET_staten_island.xlsx",start.date.txt="2012-05-01",i=5,plotnum=1)
stats[7,] = plotem(file.fluxtower="CA-StaW_ET_staten_island.xlsx", start.date.txt="2012-05-01", i=6,plotnum=2)


