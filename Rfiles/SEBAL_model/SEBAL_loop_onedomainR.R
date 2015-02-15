
# Run SEBAL for all years and jdays
library(raster)

sub.extent.df = c(-122.5,-120,36,39.55)
regname = c("one")
basedir = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/final/"
afit.vec = c(1,4.3,5.5,7.97)
bfit.vec = c(-3.1,-4.6,-4.0,-7.0)
quant.break = 0.005  # The quantile break for selecting dry and wet pixels

for (ab in 3:length(afit.vec)){
afit = afit.vec[ab]
bfit = bfit.vec[ab]
print(paste("param ",ab," of ",length(afit.vec),". afit = ",afit,"  bfit = ",bfit,sep=""))
flush.console()
for (region in 1:1){
	sub.extent = extent(sub.extent.df)
	outdir = paste(basedir,"/2015_02_",regname[region],"_afit",afit,"_bfitm",abs(bfit),"q",quant.break,sep="")
	#dir.create(outdir)
	if (region==1){
		start.year.vec = c(2011,2012)
	} else {
		start.year.vec = c(2011)  # south only has tower data for 2011
	}	
	start.jday.vec = c("061","181")
	end.jday.vec = c("180","305")

for (yind in 2:length(start.year.vec)){
	start.year = start.year.vec[yind]
	end.year = start.year.vec[yind]
	for (dayind in 1:2) {  # ****  change to 2:2 to do 181-305, or 1:1 to do 061-180
		start.jday = start.jday.vec[dayind]
		end.jday = end.jday.vec[dayind]
		setwd("G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/SEBAL/")
		source("SEBAL_Rn_MCD43A_nowater_urban_NS_mask.R")
	}
}

#  Extract ET data for fluxtowers (single directory)

indir = outdir

for (yind in 1:length(start.year.vec)){
	year = start.year.vec[yind]
	for (dayind in 1:2) {
		start.jjj = start.jday.vec[dayind]
		end.jjj = end.jday.vec[dayind]
		setwd("G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/extract_for_fluxtowers/")
		source("extract_SEBAL_ET24_fluxtowers_daily.R")
	}
}

}  # END REGION LOOP
}  # END AB loop

#  END OVERALL LOOP THROUGH AB PARAMETERS, NORTH AND SOUTH DOMAINS





#  Extract data for fluxtowers, loop over all directories

indir.SEBAL.list = list.dirs("G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET",recursive=FALSE)

beginfolder = 5
endfolder = 10 #  end index of folders to perform the EF calculation
	#   "south" only have data for 2011, so set start.year.vec to 2011 for those folders

for (findex in beginfolder:endfolder){
	indir = paste(indir.SEBAL.list[findex],"/",sep="")
	for (yind in 1:length(start.year.vec)){
		year = start.year.vec[yind]
		for (dayind in 1:2) {
			start.jjj = start.jday.vec[dayind]
			end.jjj = end.jday.vec[dayind]
			setwd("G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/Rfiles/extract_for_fluxtowers/")
			source("extract_SEBAL_EF24_fluxtowers_daily.R")
		}
	}
}

		
	