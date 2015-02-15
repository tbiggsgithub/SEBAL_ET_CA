#  Uses Rn from 1030am

#  1. Copy Rn into the appropriate directory.
#  2. Find an LST image that has little missing data (will be used for the mask).

library(raster)
library(maptools)

goodlayer = 101
outdir.list = list.dirs("G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/",recursive=FALSE)

loop=1  # Using SEBAL_loop.R to run?
if (loop==0){
#outdir = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/ET/2014_11_large_area_mcd43a_nowater_urban_north_005/new_z0m_formula_afit4.3.bfit.minus.4.6/"
outdir = outdir.list[11]
start.year=2011
start.jday ="181"  # "181"
end.year = 2011
end.jday = "305"  # "305"
sub.extent = extent(c(-121,-120,36,37))  # south
#sub.extent = extent(c(-122.5,-120,38,39.55))  # north
quant.break = 0.005  # The quantile break for selecting dry and wet pixels
}

shpdir="G:/mydocuments/SDSU/research/CA/ET_MOD16_SEBAL_towers/ETlatlong"
indir.shp = "G:/mydocuments/SDSU/research/globalET/fluxtowers_USA/fluxtowerALL"
fname.shp = "cropfluxUSlatlon"

elevrange = c(-20,100)

#  extent: rows=xmin, xmax; cols=ymin, ymax
startindex = 1  #  Used for January, when Rn<0
ELR = 6.5 #  degrees per km.  Used to correct LST to sea-level
NAthresh = 0.2  #  Fraction of cells in LST grid that need data to do the procedure

#  Input directories
indir.LST = "G:/large_datasets/USA/california/modis_fluxtower_sites/MOD11A1/projected/"
indir.NDVI = "G:/large_datasets/USA/california/modis_fluxtower_sites/MOD13A2/projected/"
indir.alb = "G:/large_datasets/USA/california/modis_fluxtower_sites/MCD43A3/projected/"
indir.Rn.1030 = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/Rn/1030am/2014_11_8_MCD43A_large_area/"
indir.Rn.24 = "G:/large_datasets/USA/california/modis_fluxtower_sites/SEBAL/Rn/24hmean/2014_11_8_MCD43A_large_area/"
indir.dem = "G:/large_datasets/USA/california/modis_fluxtower_sites/dem/gtopo30_1km/"
indir.lu = "G:/large_datasets/USA/california/modis_fluxtower_sites/MCD12Q1_land_cover/projected/"
indir.met = "G:/large_datasets/USA/california/modis_fluxtower_sites/GMAO/SLV_wind_temp_rH/"
fname.dem = "e10g.bil"  # 1km GTOPO30 dataset

#  Load land use 
flist.lu.in = list.files(indir.lu,pattern="Type_5.tif")
fname.lu = flist.lu.in[grep(as.character(start.year),flist.lu.in)]
setwd(indir.lu)
lc.in = raster(fname.lu)

shpin = readShapePoints(paste(indir.shp,fname.shp,sep="/"))
shp = shpin[shpin$Location=="California",]

codes = read.table("lu_codes_PFT_type_5.txt",header=TRUE)
lc.in[lc.in %in% c(0,9)]=NA
lc = crop(lc.in,sub.extent)
plot(lc)
plot(shp,add=TRUE)

# LST QC flag ranges
#  Based on MOD11A1_QC_descriptions.R
QCexclude = c(seq(65,255, by = 1))  #  QC 65-128 -> LST error 2-3K		
						#  QC 129-192 ->  LST error 1-2K
						#  QC 193-265 ->  LST error >4K

#  _______________ END INPUT _________________________ #

date.start = strptime(paste(start.year,start.jday,sep="-"),format="%Y-%j")
yyyyjjj.start = as.numeric(paste(start.year,start.jday,sep=""))
yyyyjjj.end = as.numeric(paste(end.year,end.jday,sep=""))

# Load LST filelist
flist.LST = list.files(indir.LST, pattern="LST_Day")
flist.LST.QC = list.files(indir.LST, pattern="QC_Day")
yyyyjjj.LST = as.numeric(substr(flist.LST,10,16))
LST.start.index = min(which(yyyyjjj.LST>=yyyyjjj.start))
LST.end.index = max(which(yyyyjjj.LST<=yyyyjjj.end))
flist.LST.sub = flist.LST[LST.start.index:LST.end.index]
flist.LST.QC.sub = flist.LST.QC[LST.start.index:LST.end.index]
yyyyjjj.LST.sub = as.numeric(substr(flist.LST.sub,10,16))

# Load NDVI filelist
flist.NDVI = list.files(indir.NDVI,pattern="NDVI")
yyyyjjj.NDVI = as.numeric(substr(flist.NDVI,10,16))
NDVI.start.index = min(which(yyyyjjj.NDVI>=yyyyjjj.start))
NDVI.end.index = max(which(yyyyjjj.NDVI<=yyyyjjj.end))
flist.NDVI.sub = flist.NDVI[NDVI.start.index:NDVI.end.index]
yyyyjjj.NDVI.sub = as.numeric(substr(flist.NDVI.sub,10,16))

length(flist.LST.sub)
length(flist.NDVI.sub)

#  Load albedo list
setwd(indir.alb)
flist.alb.in = list.files(pattern="shortwave.tif")
flist.alb = flist.alb.in[grep("WSA",flist.alb.in)]
yyyyjjj.alb = as.numeric(substr(flist.alb,10,16))
alb.start.index = min(which(yyyyjjj.alb>=yyyyjjj.start))
alb.end.index = max(which(yyyyjjj.alb<=yyyyjjj.end))
flist.alb.sub = flist.alb[alb.start.index:alb.end.index]
yyyyjjj.alb.sub = as.numeric(substr(flist.alb.sub,10,16))

#  Load netR raster and create date list
fname.Rn.1030 = paste("RnWm2_1030_", start.year, start.jday, "_", start.year, end.jday, ".gri",sep="")
fname.Rn.24 = paste("RnWm2_24_", start.year, start.jday, "_", start.year, end.jday, ".gri",sep="")
file.list.Rn.1030 = list.files(indir.Rn.1030,pattern="gri")
file.list.Rn.24 = list.files(indir.Rn.24,pattern="gri")
yyyyjjj.netR = seq(yyyyjjj.start,yyyyjjj.end)
Rn.stack.1030 = stack(paste(indir.Rn.1030,fname.Rn.1030,sep=""))
Rn.stack.24 = stack(paste(indir.Rn.24,fname.Rn.24,sep=""))

#  Load DEM for lapse rate correction.  Only load this layer once.
	# Projection command requires loading at least one LST image, then reproject DEM

	#  Note: Mask was created using mask_create_northern_CA_fluxsites.R
	
#x=flist.alb.sub[1]
#balc = raster(x)
#balc[balc==32767]=NA

# RUN THIS SECTION TO MAKE SURE THE MASK IS OK

LSTin1 = raster(paste(indir.LST,flist.LST.sub[goodlayer],sep=""))

# CAmask = raster(paste(indir.mask,fname.mask,sep=""))
#CAmask = LSTin1/LSTin1
CAmask = resample(lc,LSTin1)


setwd(indir.dem)
demin = raster(fname.dem)
dema = resample(demin,CAmask)
dem = mask(dema,CAmask)
dem[dem>elevrange[2]]=NA
dem[dem<elevrange[1]]=NA 
LST.correction = dem*ELR/1000

dev.new()
par(mfrow=c(1,2))
plot(CAmask)
plot(dem)

#  END TEST IF MASK IS OK

# Load wind speed file list
flist.met = list.files(indir.met,pattern="nc")
#met.fname = "CAmet_foo.txt"
#d.met = read.table(paste(indir.met,met.fname,sep=""),header=TRUE)
dates.met = strptime(substr(flist.met,37,44),format="%Y%m%d")
dates.met.jjj = as.numeric(format(dates.met,"%Y%j"))

#  Load shapefile
#shp = readShapePoints(fname.shp)
ncells = length(dem[!is.na(dem)])

# ***** START LOOP THROUGH LST LIST *****
yyyyjjj.LST.sub.out = matrix(NA,365,2)
n.out=0 #  Number of layers in the final output; layers will be missing
	   # where number of cells with LST data < NAthresh
if (exists(as.character(substitute(ET24.stack)))) {remove(ET24.stack)}
a=1
# for (i in startindex:length(flist.LST.sub)) {
for (i in startindex:length(flist.LST.sub)) {
	print(paste(start.year,start.jday,i,sep=" "))
	flush.console()
	#  Load LST
	LSTin1 = raster(paste(indir.LST,flist.LST.sub[i],sep=""))
	NA.LST = 0
	LSTin1[LSTin1==NA.LST]=NA
	LSTin2 = crop(LSTin1,dem)
	LSTin=mask(LSTin2,dem)
	scale.factor.LST = 0.02
	LSTraw = LSTin*scale.factor.LST
	LSTall = LSTraw+LST.correction
	LSTQCin1 = raster(paste(indir.LST,flist.LST.QC.sub[i],sep=""))
	LSTQCin2 = crop(LSTQCin1,dem)
	LSTQC=mask(LSTQCin2,dem)
	LSTQC[LSTQC>64] = NA
	LST = mask(LSTall,LSTQC)
	#foohist = hist(LSTQC,breaks=seq(0,255,by=1))
	#foobind = data.frame(foohist$breaks[1:255],foohist$counts)
	#QClist = foobind[foobind[,2]>0,]
	#QCbitlist = QC_Data[match(QClist[,1],QC_Data$Integer_Value),]
	#  Need minimum fraction of LST cells to have data
	if(length(LST[!is.na(LST)])/ncells > NAthresh) {
	#  Load NDVI
  		diff.jday = abs(yyyyjjj.NDVI.sub-yyyyjjj.LST.sub[i])
  		index.NDVI = which(diff.jday==min(diff.jday))[1]
		NDVIin = raster(paste(indir.NDVI,flist.NDVI.sub[index.NDVI],sep=""))
		#  projectRaster automatically crops to raster extent
		NDVIag = aggregate(NDVIin/10000,4)  #aggregates to LST cellsize
		NDVIres=resample(NDVIag,LST)
		NDVI=mask(NDVIres,dem)
		NDVI[NDVI==0] = NA

		#  Load, project albedo
  		# Determine which albedo file to load
  		diff.jday = abs(yyyyjjj.alb.sub-yyyyjjj.LST.sub[i])
  		index.alb = which(diff.jday==min(diff.jday))[1]
  		alb.file = flist.alb.sub[index.alb]
		albin = raster(paste(indir.alb,alb.file,sep=""))
		albin[albin==32767]=NA
		albres=resample(albin/1000,LST)
		alb=mask(albres,dem)

		#  Load, project NetR
		index.Rn = which(yyyyjjj.netR==yyyyjjj.LST.sub[i])
		Rnin = Rn.stack.1030[[index.Rn]]
		Rn = resample(Rnin,LST)  #  CAn experiment with different values of Rn
		Rn = mask(Rn,dem)
		Rn24in = Rn.stack.24[[index.Rn]]
		Rn24 = resample(Rn24in,LST)
		Rn24 = mask(Rn24,dem)

		#  Extract wind speed
		setwd(indir.met)
		index.met = which(dates.met.jjj==yyyyjjj.LST.sub[i])
		UxEast = raster(flist.met[index.met],band=19,varname="U2M")  # Band 19 = 1030am
		UxNorth = raster(flist.met[index.met],band=19,varname="V2M")	       
		Uxraw = mean(abs(UxEast),abs(UxNorth))
		Ux = resample(Uxraw,LST,method="bilinear")
		Ux = mask(Ux,dem)
		DHraw = raster(flist.met[index.met],varname="DISPH") # FROM MERRA
		DH = resample(DHraw,LST,method="bilinear")
		DH = mask(DH,dem)

#  Calculate G
# Bastiaanssen 1998 Equation 12
  # G is in fraction of Rn (G in W/m2 = Rn*G)
Gfract = ((LST-273.15)/alb)*(0.0032*alb + 0.0062*alb^2)*(1-0.978*(NDVI^4))
G0 = Rn*Gfract
#Gfract.water.jul.to.dec = Rn-90  #  Eq 8.5, Morse et al 2000, Bear 
#Gfract.water.jan.to.jun = 0.9*Rn-40  #  Eq 8.7, Morse et al 2000, Bear

#  Select dry/wet pixels
LST.vals = getValues(LST)
NDVI.vals = getValues(NDVI)
# NDVI.vals[NDVI.vals<0]=NA
lowLST = quantile(LST.vals,probs=c(quant.break),na.rm=TRUE)
index.lowLST = which(LST.vals<=lowLST)
quant.break.ndvi = 1E-9
NDVI.lowLST = quantile(NDVI.vals[index.lowLST],probs=c(1-quant.break.ndvi),na.rm=TRUE)
index.wet = which((LST.vals<=lowLST) & (NDVI.vals>=NDVI.lowLST))[1]

hiLST=quantile(LST.vals,probs=c(1-quant.break),na.rm=TRUE)
index.hiLST = which(LST.vals>=hiLST)
NDVI.hiLST = quantile(NDVI.vals[index.hiLST],probs=c(quant.break),na.rm=TRUE)
NDVI.lowLST = sort(NDVI.vals[index.hiLST])
NDVI.hiLST.gt0 = NDVI.lowLST[NDVI.lowLST>=0]

index.dry = which((LST.vals>=hiLST) & (NDVI.vals==NDVI.hiLST.gt0[1]))



#  index.dry sometimes returned a NA value for Rn, so set up a loop to go thorugh sorted NDVI list
	#  to find pixel with Rn data
j=1
while (is.na(Rn[index.dry])) {   
	j=j+1
	print(index.dry)
	flush.console()
	index.dry = which((LST.vals>=hiLST) & (NDVI.vals==NDVI.hiLST.gt0[j]))
}

#  Initialize ustar, rah
   # Original python:  z0m at weather station = .03 (Morse 2000), ln(2/.03)=4.199
   #  z0m based on Morse et al 2000 eq 9.1, also p. 94
  #afit = 7.97
  #bfit = -8.12 #  From 
  #afit = 4.3  # Hypothetical, fits corn data better.  See script "z0m_vs_NDVI.R"
  #bfit = -4.6
  #afit = 5.5  # Best for corn in senescence
  #bfit = -4   # Best for corn in senescence
  #afit = 1  # Best for rice
  #bfit = -3.1  # Best for rice
  z0m = exp(afit*NDVI+bfit)  # Morse et al 2000 eq 9.1, also p. 94
  maxh = 3.5 # meters.  Max height allowed for veg
  z0m[z0m>(maxh*0.123)]=maxh*0.123 # Max height 3m allowed
  h = z0m/0.123  # crop height...to check realism
  print(round(extract(h,shp),1))
  flush.console()
  # ustar
  k=0.41 # Von Karman constant
  z.ws=2 # Height of anenometer at the weather station
  h.veg.ws = 0.25 # Height of vegetation in m at weather station
  z0m.ws = 0.123*h.veg.ws #  Morse eq 10.4  # SCALAR
  z0m.ws = 0.123*DH  # Displacement height is gridded from MERRA dataset...should be from NDVI?
	# 0.123 is best for fields, 0.2 best for sparse veg (Morse 2000 p. 94)
  UstarX = (Ux*k)/(log(z.ws/z0m.ws)) ## This calculates Ustar at the weather station
  #  Python code:  U200 = ((8.805)/k)*UstarX, close to below
  U200 = (log(200/z0m.ws)/k)*UstarX
#  For rah
  z1 = 0.01 # m, Morse et al 2000 p 62
  z2 = 2  # m, independent of anenometer height
  
#  Calculate dT coefficients
Tdry=LST[index.dry]  # Tdry and Twet never change in the iterations
Twet = LST[index.wet]
Rn.v = getValues(Rn)
Rn.dry = Rn.v[index.dry]

Hdry=Rn[index.dry]-G0[index.dry]  #Hdry never changes

# Tair is initially a single value for dry pixel, then becomes a field once a and b are known
Tair = 273.15+26
Ustar = (U200*k) /log(2/z0m)  # Gridded if z0m is gridded
rah = log(z2/z1)/(Ustar*k)
percentDiffrah = 100
percentDiffH = 100
count=0
a=1  # foo value of a to get loop going
while((percentDiffH>1) & (count<100) & (a<1000000)){  # sometimes "a" goes infinite
	count=count+1
	AirPressure = 101.3*(((Tair-0.0065*dem)/Tair)^5.26)
	airDens = (1000.0*AirPressure)/(1.01*Tair*287.0)  #  Morse et al 2000 p 99
	dTdry = (Hdry*rah[index.dry])/(airDens[index.dry]*1004.0)
	a = dTdry/(Tdry-Twet)
	b = a*Twet
	dT = a*LST-b # Raster of dT1	
	H = airDens*1004*dT/rah

	# Monin-Obukhov stability
	H.ne.0 = H
	H.ne.0[H.ne.0==0] <- NA
	L = -1*((airDens*1004*(Ustar^3)*Tair)/(k*9.81*H.ne.0))
	L[H==0] = -1000

	L.lt.0 = L
	L.lt.0[L.lt.0>=0] = NA
	L.gt.0 = L
	L.gt.0[L.gt.0<=0] = NA

	#  L<0
	xz2 = (1-16*z2/L.lt.0)^0.25 # Morse eq 10.11
	x200m = (1-16*200/L.lt.0)^0.25  # Morse eq 10.11
	psi_hz2.Llt0 = 2*log((1+xz2^2)/2) # Morse eq 10.9
	psi_m200.Llt0 = 2*log((1+x200m)/2)+log((1+x200m^2)/2)-2*atan(x200m)+0.5*pi # Morese eq 10.9a

	# L>0
	psi_hz2.Lgt0 = -5*(z2/L.gt.0)
	psi_m200.Lgt0 = -5*(200/L.gt.0)

	#  combine psi_hz2 and psi_m200 for L<0 and L>0:
	psi_hz2=cover(psi_hz2.Llt0,psi_hz2.Lgt0)
	psi_hz2[L==0]=2*log((1+1)/2)  # xz2,xz200=1 when L=0
	psi_m200=cover(psi_m200.Llt0,psi_m200.Lgt0)
	psi_m200[L==0]=2*log((1+1)/2)+log((1+1^2)/2)-2*atan(1)+0.5*pi

	UstarUp = U200*k/(log(200/z0m)-psi_m200)  # Morse 2000 eq 10.8
	# Different from python code Line 867 == log(200/z0m.dry) is log(2/z0m.dry) in python
	rahUp = (log(z2/z1)-psi_hz2)/(UstarUp*k)    # Morse 2000 eq 10.7', p65
	Hup = airDens*1004*dT/rahUp
	percentDiffrah = abs(((rahUp[index.dry]-rah[index.dry])/rah[index.dry])*100)
	percentDiffH = abs(((Hup[index.dry]-H[index.dry])/H[index.dry])*100)
	rah=rahUp
	Ustar=UstarUp
	Tair = LSTraw-dT  # Raster of Tair
}

EvapFrac = (Rn - G0 - H) / (Rn - G0)
lambda = (2.501-0.00236*(LST-273))*1E6
ET24 = 86400*(Rn24)*EvapFrac/lambda
#if (i==startindex) {ET24.stack=ET24}
#if (i>startindex) {ET24.stack=addLayer(ET24.stack,ET24)}
if ((count<100) & (a<1000000)){
	n.out=n.out+1
	if (!exists(as.character(substitute(ET24.stack)))) {
		ET24.stack=ET24
		rah.stack = rah
	} else {
	ET24.stack=addLayer(ET24.stack,ET24)
	rah.stack=addLayer(rah.stack,rah)
	}
	yyyyjjj.LST.sub.out[n.out,1] = yyyyjjj.LST.sub[i]
	yyyyjjj.LST.sub.out[n.out,2]= length(LST[!is.na(LST)])/ncells
	print(paste("N.ET.layer=",dim(ET24.stack)[3],"  n.dates = ", n.out, sep=""))  # n layers
	}
flush.console()
print(paste("Number of good layers = ", n.out,sep=""))

par(mfrow=c(2,2))
plot(LST.vals,NDVI.vals,pch=20,cex=0.6,main = yyyyjjj.LST.sub.out[i])
points(LST.vals[index.wet],NDVI.vals[index.wet],col="blue",pch=20,cex=2)
points(LST.vals[index.dry],NDVI.vals[index.dry],col="red",pch=20,cex=2)
plot(LST)
plot(NDVI)
plot(ET24)
plot(shp,add=TRUE)
}
}

fname.out = paste("ET24_daily_", yyyyjjj.start, "_", yyyyjjj.end,sep="")
fname.out.rah = paste("rah_daily_", yyyyjjj.start, "_", yyyyjjj.end,sep="")
fname.out.dates = paste(fname.out,"_dates.txt",sep="")

yyyyjjj.LST.sub.out.b = yyyyjjj.LST.sub.out[!is.na(yyyyjjj.LST.sub.out[,1]),]
yyyyjjj.final = data.frame(yyyyjjj.LST.sub.out.b)
names(yyyyjjj.final) = c("Date","fract.notNA")
length(yyyyjjj.final[,1])
dim(ET24.stack)[3]

setwd(outdir)

writeRaster(ET24.stack,fname.out,overwrite=TRUE)
write.table(yyyyjjj.final,fname.out.dates)
writeRaster(rah.stack,fname.out.rah,overwrite=TRUE)

rm(ET24.stack)
rm(rah.stack)


