library(stringi)

start.dates = as.Date(c("2012-05-01","2012-05-01","2012-06-17","2012-05-01", "2011-06-01", "2011-07-01"),format="%Y-%m-%d")
towers = c("StaD","StaW","Wil","Twt","Fiv","Big")

# Load EVI
indir = "G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/timeseries_extracted/9sites/"
setwd(indir)
file.EVI = "MOD13A2EVI.2010.2012.towers.buffer.txt"
EVI = read.table(file.EVI)
EVI.dates = as.Date(strptime(EVI$yyyyjjj,format="%Y%j"))

# Load LSWI
#file.LSWI = "MOD09LSWI-2000-2013-9towers.txt"  # No buffer, based on ts from resampled grid subsets
file.LSWI = "LSWI.2010.2012.towers.buffer.txt"
LSWI = read.table(file.LSWI)
LSWI.dates = as.Date(strptime(LSWI$yyyyjjj,format="%Y%j"))
#rownames(LSWI)=LSWI$yyyyjjj # only if using "LSWI.2010.2012.towers.buffer.txt"

#  Load LAI
file.LAI = "LAI.2011.2012.towers.buffer.txt"
LAI = read.table(file.LAI)
LAI.dates = as.Date(strptime(LAI$yyyyjjj,format="%Y%j"))


mean.NDVI.LSWI <- function(i){
    matchcol = match(towers[i],tower.ids.evi)+1
    year = format(start.dates[i],"%Y")
    end.date = as.Date(paste(year,"0930",sep=""),format="%Y%m%d")
    evi.mean = mean(EVI[(EVI.dates>=start.dates[i]) & (EVI.dates<=end.date),matchcol])
    lswi.mean = mean(LSWI[(LSWI.dates>=start.dates[i]) & (LSWI.dates<=end.date),matchcol])
    lai.mean = mean(LAI[(LAI.dates>=start.dates[i]) & (LAI.dates<=end.date),matchcol])
    return(c(evi.mean,lswi.mean,lai.mean))
}


namesEVI = data.frame(n=names(EVI)[2:length(names(EVI))])
names.split = stri_split_fixed(namesEVI$n,".",simplify=TRUE)
tower.ids.evi = names.split[,2]

outdf = data.frame(towers=towers,evi=NA,lswi=NA,lai=NA)
for (j in 1:length(towers)){
	x = mean.NDVI.LSWI(i=j)
	outdf[j,2:4]=x
}
	
i=4


