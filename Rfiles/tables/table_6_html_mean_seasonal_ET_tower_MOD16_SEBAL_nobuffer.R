#  Table with MOD16 and SEBAL ET and errors
library(htmlTable)

outdir.tables = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"
basedir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/plots/"
genfile = "table_xx_errors_final_runs_nobuffer.R"
source(paste(basedir,genfile,sep=""))

#  Get full stats for just one station
folder.name.list
f = 9 # 	=0.005, afit 7.97, bfit 7
stats[[f]]
err = stats[[f]]$err.SEBAL

MOD16ET = c(2.6,2.9,2.8,1.9,3.3,1.1)
towerET = c(2.9,2.9,5.7,5.7,5.0,5.0)

MOD16ETerror = 100*round((MOD16ET-towerET)/towerET,2)
mean(MOD16ETerror)
MOD16mean.bias = mean(MOD16ET-towerET)

MOD16mean.abs.error = mean(abs(MOD16ET-towerET))
mean(abs(MOD16ETerror))

crops = c("Corn","Corn","Rice","Rice","Rice","Cotn")
year = stats[[f]][,2]

#finaltable = data.frame(Tower=towers,Crop=as.character(crops),Year=as.character(year),TowerET=as.character(towerET),MOD16ET=as.character(MOD16ET),MOD16rMB=as.character(MOD16ETerror),SEBALET=as.character(stats[[f]][,4]),SEBALrMB=as.character(stats[[f]][,5]))
finaltable = data.frame(Tower=stats[[f]]$Tower,Crop=crops,Year=year,TowerET=towerET,MOD16ET=MOD16ET,MOD16rMB=MOD16ETerror,SEBALET=stats[[f]][,4],SEBALrMB=stats[[f]][,5])

names(finaltable)=c("Tower","Crop","Year","ET Tower","ET","rMB","ET","rMB")
rownames(finaltable)=finaltable$Tower
rownames(finaltable)[4]="Twt"
finalout = finaltable[,2:length(finaltable[1,])]
finalout$Crop = as.character(finalout$Crop)
finalout[,3]=as.character(format(finalout[,3],digits=2))
finalout[,6]=as.character(format(finalout[,6],digits=2))
htmlTable(finalout)
colnames.repeatET = colnames(finalout)
colnames.repeatET[6]="ET"
colnames.repeatET[7]="rMB"

outhtml = htmlTable(finalout,
	cgroup=c("","MOD16","SEBAL"),
	header=colnames.repeatET,
	n.cgroup = c(3,2,2),
	caption = paste("Table xx.  Seasonal ET estimated by towers, MOD16, and SEBAL in mm/day.",
	"rMB is the relative mean bias (%).  SEBAL was implemented using q=0.005, z0m parameter set 4, using two domains.  Values extracted for the MODIS cell at the tower."),
	rowlabel="Tower",
	#results="asis"
)
print(outhtml)

setwd(outdir.tables)
sink("Table_6a_ET_no_buffer.html")
print(outhtml,type="html",useViewer=FALSE)
sink()


# Just trying to see what a barplot looks like...ok but sort of confusing
outbarplottable = t(as.matrix(finaltable[,c(4,5,7)]))
barplot(outbarplottable,beside=TRUE)