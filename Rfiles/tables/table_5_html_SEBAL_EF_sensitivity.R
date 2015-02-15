#  Prepare table xx for publication

#library(xtable)
library(tools)
library(utils)
library(htmlTable)
# system("pdflatex Table_xx_error_SEBAL_runs.tex")

outdir.tables = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"
basedir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/plots/"
genfile = "table_xx_EF_sensitivity_domain_percentile_final_runs.R"
source(paste(basedir,genfile,sep=""))

outtable2 = outtable
# Instead of listing c and d, make new column with the equation number for z0m-NDVI parameters
paramlink = data.frame(N=c(1,2,3,4),c=c(1,4.3,5.5,7.97))
outtable2$z0mEq = match(outtable2$c,paramlink$c)
names(outtable2) = c("NDOM","q","c","d","Tower Mean","Min","Max","Corn","Rice","Cotton","Crop mean","Rank","z0m par")
outtable2$Corn = round(outtable2$Corn,0)
outtable2$Rice = round(outtable2$Rice,0)
rownames(outtable2) = outtable2$Rank
#outtable.final = outtable[,c(1,2,3,4,6,7,8,9,10,5,11)] # Re-order the columns
outtable.final = outtable2[,c(1,2,13,6,7,8,9,10,5,11)] # Re-order the columns

#  USE htmlTable
# http://gforge.se/packages/

outhtml.html = htmlTable(outtable.final,
	cgroup=c("","Relative Mean Bias (%)"),
	n.cgroup = c(3,7),
	caption = paste("Table xx.  Relative mean bias (rMB) in seasonal SEBAL evaporative fraction (EF) for different domains and values of","\n",
	"q, c, and d.  NDOM is the number of domains.  z0m par is the parameter used for calculating z0m, corresponding to Table xx. Tower mean is the mean of rMB for all six towers.  Crop mean is the mean of the rMB for each crop type.",
	" The rank is of the Crop mean."),
	# "Rank is by the mean rMB by crop."), 
	rowlabel="Rank",
	results="asis"
)

setwd(outdir.tables)
sink("Table_xx_SEBAL_EF_error_sensitivity.html")
print(outhtml.html,type="html",useViewer=FALSE)
sink()









# Can't get into pdf (but ok importing to Word
system("pandoc -s -o -process-html outputsink2.pdf outputsink.html")
system("pandoc -s -o outputsink2.pdf outputsink.html")
system("pandoc -f html -t docx -s -o outputsink.docx outputsink.html")