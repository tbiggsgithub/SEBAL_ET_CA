library(htmlTable)

outdir.tables = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/writeups/tables/"

sourcedir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/plots/"
sourcefile = "plot_ts_PET_MOD16_tower_multiple_in_one.R"

names(statsout2) = c("Tower","MOD16","Error %")
outhtml = htmlTable(statsout2,
	rowlabel = "Tower name",
	cgroup = c("ETo",""),
	n.cgroup = c(2,1),
	caption = "Table 3.  Comparison of mean seasonal reference evapotranspiration (ETo) from MOD16 and towers.",
)

setwd(outdir.tables)
sink("Table_xx_ETo_tower_MOD16_error.html")
print(outhtml,type="html",useViewer=FALSE)
sink()

