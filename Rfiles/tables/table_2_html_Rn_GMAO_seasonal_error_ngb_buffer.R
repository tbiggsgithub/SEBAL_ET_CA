#  Compare Rn from GMAO with Rn from fluxtowers
#  Extracted Rn from "extract_Rn_fluxtowers.R"
#  Rn is 24-hour averaged from GMAO MERRA (vs layer 1)

library(XLConnect)
library(htmlTable)
outdir.table = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"

plotem <- function(file.fluxtower,start.date.txt,i) {
start.date = as.Date(start.date.txt,"%Y-%m-%d")
year = format(start.date,"%Y")
end.date = as.Date(paste(year,"-09-30",sep=""),format="%Y-%m-%d")
fname.Rn = paste("Rn_GMAO_", as.character(year), "_UStowers_24hmean_ngb.txt",sep="")
#fname.Rn = paste("Rn_GMAO_", as.character(year), "_UStowers_24hmean_buffer_ngb.txt",sep="")
Rn.GMAO = read.table(paste(indir.Rn,fname.Rn,sep=""))
Rn.GMAO.dates = strptime(Rn.GMAO$Date,format="%Y%j")
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
labeltext = c("03-01","05-01","07-01","09-01","11-01")
labeltext.letters = c("Mar","May","Jul","Sep","Nov")
labeldates = as.Date(paste(year,labeltext,sep="-"),format="%Y-%m-%d")

# start end dates of season
Rn.GMAO[(as.Date(Rn.GMAO.dates)<start.date)|(as.Date(Rn.GMAO.dates)>end.date),match.col]=NA  # For seasonal average
 
meanbias.May.Oct = round(mean(Rn.GMAO[,match.col]-Rn.flux.match,na.rm=TRUE),1)
relbias.May.Oct = round(meanbias.May.Oct/mean(Rn.flux.match,na.rm=TRUE),2)
RMSE.May.Oct = round(sqrt(mean((Rn.GMAO[,match.col]-Rn.flux.match)^2,na.rm=TRUE)),1)
rRMSE.May.Oct = round(sqrt(mean(((Rn.GMAO[,match.col]-Rn.flux.match)/Rn.flux.match)^2,na.rm=TRUE)),2)

Rn.GMAO[Rn.GMAO.dates<strptime(paste(year,"-07-01",sep=""),format="%Y-%m-%d"),match.col]=NA
meanbias.Jul.Oct = round(mean(Rn.GMAO[,match.col]-Rn.flux.match,na.rm=TRUE),1)
relbias.Jul.Oct = round(meanbias.May.Oct/mean(Rn.flux.match,na.rm=TRUE),2)
RMSE.Jul.Oct = round(sqrt(mean((Rn.GMAO[,match.col]-Rn.flux.match)^2,na.rm=TRUE)),1)
rRMSE.Jul.Oct = round(sqrt(mean(((Rn.GMAO[,match.col]-Rn.flux.match)/Rn.flux.match)^2,na.rm=TRUE)),2)

return(c(round(mean(Rn.flux.match,na.rm=TRUE),1),round(mean(Rn.GMAO[,match.col],na.rm=TRUE),1),meanbias.May.Oct,relbias.May.Oct,RMSE.May.Oct,rRMSE.May.Oct,meanbias.Jul.Oct,relbias.Jul.Oct,RMSE.Jul.Oct,rRMSE.Jul.Oct))
}
# END PLOTEM FUNCTION

indir.Rn = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
indir.flux = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/fluxtower_data/"

# WILSON, BIG, TWT, AND FIV

towers = c("CA.Wil","CA.Big","US.Twt","CA.Fiv","CA.StaD","CA.StaW")
towers.legend = c("Wil","Big","Twt","Fiv","StaD","StaW")
stats = matrix(NA,nplots,10)
rownames(stats)=c("CA.Wil","Ca.Big","US.Twt 2012","US.Twt 2011", "CA.Fiv", "StaD","StaW")
colnames(stats) = c("RnTower","RnGMAO","Bias.May.Oct","Relbias.May.Oct","RMSE.May.Oct","rRMSE.May.Oct","Bias.Jul.Oct","Relbias.Jul.Oct","RMSE.Jul.Oct","rRMSE.Jul.Oct")
stats[1,] = plotem(file.fluxtower="CA-Wil_Rice_DailyET_Williams(Bransford2012)_RIWILET.xlsx",start.date.txt="2012-06-17",i=1)
stats[2,] = plotem(file.fluxtower="CA-Big_DailyETEastRice(Boeger)2011.xlsx", start.date.txt="2011-07-01", i=2)
stats[3,] = plotem(file.fluxtower="US_twt_rice_Twitchell_Rice_ET_2009_2013.xlsx", start.date.txt="2012-05-01", i=3)
stats[4,] = plotem(file.fluxtower="US_twt_rice_Twitchell_Rice_ET_2009_2013.xlsx", start.date.txt="2011-05-01", i=3)
stats[5,] = plotem(file.fluxtower="CA-Fiv_Cotton SR Farming D 2011 VWC corrected_5points.xlsx", start.date.txt="2011-06-01",i=4)

#  run these two lines to get all 6 time series on one plot, or don't run to keep scatterplot with just four series
stats[6,] = plotem(file.fluxtower="CA-StaD_ET_staten_island.xlsx",start.date.txt="2012-05-01",i=5)
stats[7,] = plotem(file.fluxtower="CA-StaW_ET_staten_island.xlsx", start.date.txt="2012-05-01", i=6)

stats.df=as.data.frame(stats)
meanbias = stats.df$RnGMAO - stats.df$RnTower
meanmeanbias = mean(meanbias)
absmeanbias = mean(abs(meanbias))
relmeanbias = 100*meanbias/stats.df$RnTower
as.numeric(stats.df$rRMSE.May.Oct,digits=2)
towers.legend = c("Wil","Big","Twt 2012","Twt 2011","Fiv","StaD","StaW")
errpct = round((stats[,2]-stats[,1])/stats[,1],2)*100
outtable = data.frame(stats[,1:2],Error=round(stats[,2]-stats[,1],1),Errorpct=errpct,RMSE=stats.df$RMSE.May.Oct,rRMSE=print(100*as.numeric(stats.df$rRMSE.May.Oct,digits=2)))
rownames(outtable) = towers.legend
outtable$Error=format(outtable$Error,digits=2)

outtable2 = as.data.frame(sapply(outtable,as.numeric))

means = round(colMeans(outtable2),1)
absmeans = round(colMeans(abs(outtable2)),1)
absmeans[c(1,2,5,6)]=NA
outtable[length(outtable[,1])+1,] = means
outtable[length(outtable[,1])+1,] = absmeans

#rownames(outtable)[length(outtable[,1])+1] = "Mean"
rownames(outtable) = c(towers.legend,"Mean","Abs mean")
foo = htmlTable(outtable,
	cgroup=c("Rn W m <sup>-2</sup>","Error","RMSE","rRMSE"),
	n.cgroup = c(2,2,1,1),
	#header = c("Tower","GMAO","W/m2","%","","%")
	header = c("Tower","GMAO","W m <sup>-2</sup>","%","W m <sup>-2</sup>","%"),
	caption="Table xx.  Comparison of seasonal net radiation (Rn) from towers and GMAO during the validation period (Table 1) using nearest neighbor interpolation of GMAO to the MODIS grid.  Means are over a 3x3 cell buffer around each tower.",
	#rgroup = c("",""),
	#n.rgroup = c(7,2),
	#ctable=TRUE
)

#foo = htmlTable(outtable,
#	cgroup=c("Rn W m <sup>-2</sup>","Error","RMSE","rRMSE"),
#	n.cgroup = c(2,2,1,1),
#	#header = c("Tower","GMAO","W/m2","%","","%")
#	header = c("Tower","GMAO","W m <sup>-2</sup>","%","W m <sup>-2</sup>","%"),
#	caption="Table xx.  Comparison of seasonal net radiation (Rn) from towers and GMAO during the validation period (Table 1) using nearest neighbor resampling.  Means are over a 3x3 cell buffer around each tower.",
#)
print(foo)

outdir.tables = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"

setwd(outdir.tables)
sink("Table_2_Rn_tower_GMAO_compare_buffer_ngb.html")
print(foo,type="html",useViewer=FALSE)
sink()

# push2doc

