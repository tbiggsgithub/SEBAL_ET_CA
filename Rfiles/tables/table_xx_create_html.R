#  Prepare table xx for publication

library(xtable)
library(tools)
library(utils)
library(htmlTable)
# system("pdflatex Table_xx_error_SEBAL_runs.tex")

basedir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/plots/"
genfile = "table_xx_errors_final_runs.R"
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
	caption = paste("Table xx.  Relative mean bias (rMB) in seasonal SEBAL ET for different domains and values of","\n",
	"q, c, and d.  NDOM is the number of domains.  z0m par is the parameter used for calculating z0m, corresponding to Table xx. Tower mean is the mean of rMB for all six towers.  Crop mean is the mean of the rMB for each crop type.",
	" The rank is of the Crop mean."),
	# "Rank is by the mean rMB by crop."), 
	rowlabel="Rank",
	results="asis"
)

#  Works!  But still can't convert to pdf
setwd(basedir)
sink("outputsink.html")
print(outhtml.html,type="html",useViewer=FALSE)
sink()









# Can't get into pdf (but ok importing to Word
system("pandoc -s -o -process-html outputsink2.pdf outputsink.html")
system("pandoc -s -o outputsink2.pdf outputsink.html")
system("pandoc -f html -t docx -s -o outputsink.docx outputsink.html")

# Doesnt work; results in no file
#setwd(basedir)
#fileConn<-file("outputfoo.html")
#print(outhtml.html,type="html",useViewer=FALSE)
#close(fileConn)

#  Doesnt' work: out html file has nothing in it:
#cat(print(outhtml.html,type="html",useViewer=FALSE),file="outhtmlfoo3.html")

#  Doesnt' work (empty html file):
#outhtml.print <- print(outhtml.html,type="html",useViewer=FALSE)
#write(print(outhtml.html,type="html",useViewer=FALSE),file="outhtml2.html")



#  Old Sweave commands:
#title = "Table xx. Mean bias of mean seasonal ET from SEBAL for different values of q, c, and d."
#outtable.x = xtable(outtable.final)
#digits(outtable.x) =  c(0,0,3,2,1,1,0,0,1,1,0,1,0)
#outtable.print = print.xtable(outtable.x,
#	include.rownames=FALSE,
#	add.to.row = list(pos=list(-1),
#		command = c(paste("\\toprule \n",
#			"Mean Bias \n"))
#	)
#)
#outtable.print = print(outtable.x,include.rownames=FALSE)

#header = "\\documentclass{article}\n \\usepackage{booktabs}\n \\begin{document}\n"
#footer = "\\end{document} \n"
#fname = "Table_xx_error_SEBAL_runso.Snw"
#write(paste(header,title,outtable.print,footer,sep=""),file=paste(basedir,fname,sep=""))
#write(paste(header,outtable.print,footer,sep=""),file=paste(basedir,fname,sep=""))
#setwd(basedir)
#foo = Sweave(paste(basedir,fname,sep=""))
#texi2pdf(paste(basedir,foo,sep=""))
