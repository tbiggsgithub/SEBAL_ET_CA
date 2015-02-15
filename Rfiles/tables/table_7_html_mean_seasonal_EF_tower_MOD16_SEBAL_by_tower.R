#  Table with MOD16 and SEBAL ET and errors
library(htmlTable)

outdir.tables = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"
basedir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/plots/"
genfile = "table_xx_EF_sensitivity_domain_percentile_final_runs.R"
source(paste(basedir,genfile,sep=""))

#  Get full stats for just one station
folder.name.list
f = 9 # 	=0.005, afit 7.97, bfit 7
EF.tower = stats[[f]][,3]

err = stats[[f]]$err.SEBAL

indir.extracted = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
mod16EF = read.table(paste0(indir.extracted,"MOD16EF-2011-2012-9sites-buffer.txt"))
mod16EFdate = as.Date(strptime(rownames(mod16EF),format="%Y%j"))
start.season = as.Date(strptime(c("2012-05-01","2012-05-01","2012-06-17","2012-05-01", "2011-06-01", "2011-07-01"),"%Y-%m-%d"))
meanMODEF=NA
towers.stats = stats[[f]]$Tower
for (t in 1:length(towers.stats)){  # extracts MOD16EF in same order as towers in stats[[f]]
	year = format(start.season[t],"%Y")
	end.season = as.Date(strptime(paste0(year,"-9-30"),"%Y-%m-%d"))
	alldays = as.Date(seq(as.Date(start.season[t]),as.Date(end.season),by=1))
	matchcol = match(towers.stats[t],names(mod16EF))
	MODEF.filled = approx(mod16EFdate,mod16EF[,matchcol],xout=alldays,rule=2)
	meanMODEF[t] = round(mean(MODEF.filled$y),2)
}

#tower.EF = outmat.lam.EF$lam.mean.daily
#EF.towers = rownames(outmat.lam.EF)

MOD16EFerror = 100*round((meanMODEF-stats[[f]]$EF.tower)/stats[[f]]$EF.tower,2)

crops = c("Corn","Corn","Rice","Rice","Rice","Cotn")
year = stats[[f]][,2]

#finaltable = data.frame(Tower=towers,Crop=as.character(crops),Year=as.character(year),TowerEF=as.character(stats[[f]]$EF.tower),MOD16EF=as.character(meanMODEF),MOD16rMB=as.character(MOD16EFerror),SEBALEF=as.character(stats[[f]][,4]),SEBALrMB=as.character(stats[[f]][,5]))
finaltable = data.frame(Tower=stats[[f]]$Tower,Crop=crops,Year=as.character(year),TowerEF=stats[[f]]$EF.tower,MOD16EF=meanMODEF,MOD16rMB=MOD16EFerror,SEBALET=stats[[f]][,4],SEBALrMB=stats[[f]][,5])

names(finaltable)=c("Tower","Crop","Year","EF Tower","EF","rMB","EF","rMB")
rownames(finaltable)=finaltable$Tower
rownames(finaltable)=c("StaD","StaW","Wil","Twt","Big","Fiv")
finalout = finaltable[,2:length(finaltable[1,])]
finalout$Crop = as.character(finalout$Crop)
finalout[,3]=as.character(format(finalout[,3],digits=2))
finalout[,6]=as.character(format(finalout[,6],digits=2))

#htmlTable(finalout)
colnames.repeatET = colnames(finalout)
colnames.repeatET[6]="EF"
colnames.repeatET[7]="rMB"
finalout$Year = as.character(finalout$Year)
outhtml = htmlTable(finalout,
	cgroup=c("","MOD16","SEBAL"),
	header=colnames.repeatET,
	n.cgroup = c(3,2,2),
	caption = paste("Table xx.  Seasonal evaporative fraction (EF) estimated by towers, MOD16, and SEBAL.",
	"rMB is the relative mean bias (%).  SEBAL was implemented using q=0.005, z0m parameter set 4, using two domains."),
	rowlabel="Tower",
	#results="asis"
)
print(outhtml)

setwd(outdir.tables)
sink("Table_xx_seasonal_EF_by_tower.html")
print(outhtml,type="html",useViewer=FALSE)
sink()