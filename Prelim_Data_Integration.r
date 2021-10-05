# coupling preliminary chemistry data together across runs and with FTICR, for RC2
# steps:
# combine NPOC and TN across runs
# read in FTICR
# merge each dataset with the metadata with dates and times
# further merge each dataset with with the catchment area data from Kyongho
rm(list=ls());graphics.off();
campaign = 'RC2' # define which campaign is the focus, currently only written for RC2
PNNL.dir = "C:/Users/gara009/"
home.dir = "OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/"
git.hub.dir = "C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/RC2_Temp/" 
source(paste0(git.hub.dir,"Stats_fun.R"))
  
ion.in.dir = paste0(PNNL.dir,home.dir,campaign,"/Ions/02_FormattedData/")
meta.in.dir = "//pnl/projects/SBR_SFA/RC2/03_Temporal_Study/06_Metadata/"
fticr.in.dir = "//pnl/projects/SBR_SFA/RC2/03_Temporal_Study/08_FTICR/Diversity Analyses/"
npoc.tn.in.dir = paste0(PNNL.dir,home.dir,"/Matched_files/",campaign,"/")
out.dir = paste0(PNNL.dir,home.dir,campaign,"/Prelim_Data_Integration/")

### Combine NPOC and TN data across runs

npoc.tn.files = list.files(npoc.tn.in.dir)

npoc.tn.combined = numeric()

for (curr.file in npoc.tn.files) {
  
  npoc.tn.combined = rbind(npoc.tn.combined,read.csv(paste0(npoc.tn.in.dir,curr.file),stringsAsFactors = F))

}

npoc.tn.combined$Event_ID = substring(npoc.tn.combined$Sample_ID,first = 1,last = 8) # pull out the unique event 'parent'
#str(npoc.tn.combined)

npoc.tun.stats = Stats_fun(npoc.tn.combined)


### combine ion data across runs. want each ion as column and each row is a sample ID

ion.compiled = matrix()

ion.folders = list.files(path = ion.in.dir)

for (curr.folder in ion.folders) {
  
  ion.files = list.files(path = paste0(ion.in.dir,curr.folder))
  
  ion.compiled.within.date = matrix()
  
  for (curr.ion.file in ion.files) {
    
    ion.name = strsplit(x = curr.ion.file,split = "_")[[1]][5]
    ion.temp = read.csv(paste0(ion.in.dir,curr.folder,"/",curr.ion.file),stringsAsFactors = F)
    ion.temp = ion.temp[-which(is.na(ion.temp$Sample_ID) == T),c('Amount','Sample_ID')]
    ion.temp = ion.temp[grep(pattern = campaign,x = ion.temp$Sample_ID),]
    colnames(ion.temp)[which(colnames(ion.temp) == 'Amount')] = ion.name
    ion.temp[,ion.name] = suppressWarnings(as.numeric(ion.temp[,ion.name]))
    
    ion.means = as.data.frame(tapply(X = ion.temp[,ion.name],INDEX = ion.temp$Sample_ID,FUN = mean))
    colnames(ion.means)[1] = ion.name
    ion.means$Sample_ID = row.names(ion.means)
    row.names(ion.means) = NULL
    ion.temp = ion.means
    
    if (ncol(ion.compiled.within.date) == 1) { 
      
      ion.compiled.within.date = ion.temp
        
    } else {
      
      ion.compiled.within.date = unique(merge(ion.compiled.within.date,ion.temp,by = 'Sample_ID',all = T))
      
    }
    
  }
  
  if (ncol(ion.compiled) == 1) { 
    
    ion.compiled = ion.compiled.within.date
    
  } else {
    
    if (identical(colnames(ion.compiled),colnames(ion.compiled.within.date)) == T) {
      
      ion.compiled = rbind(ion.compiled,ion.compiled.within.date)
      print('Good news, column names match')
      
    } else {
      
      print('Error: Column names do not match, must fix')
      break()
      
    }
  }

}

for (i in which(colnames(ion.compiled) != 'Sample_ID')) {
  
  ion.compiled[,i] = as.numeric(ion.compiled[,i])
  
}

ion.compiled$Event_ID = substring(ion.compiled$Sample_ID,first = 1,last = 8) # pull out the unique event 'parent'

str(ion.compiled)
head(ion.compiled)

ion.compiled.stats = Stats_fun(ion.compiled)
### read in FTICR data

# this is the full set
fticr.div = read.csv(paste0(fticr.in.dir,"RC2_Diversity_Metrics.csv"))
colnames(fticr.div)[which(colnames(fticr.div) == 'X')] = "FTICR_Sample_ID"
fticr.div$Event_ID = substring(fticr.div$FTICR_Sample_ID,first = 1,last = 8) # pull out the unique event 'parent'
head(fticr.div)

# this is the PCA subset
fticr.pca.div = read.csv(paste0(fticr.in.dir,"RC2_PCA_Diversity_Metrics.csv"))
colnames(fticr.pca.div)[which(colnames(fticr.pca.div) == 'X')] = "FTICR_Sample_ID"
head(fticr.pca.div)

# merge the whole fticr with the PCA subset
fticr.div = merge(fticr.div,fticr.pca.div,by="FTICR_Sample_ID")

### read in metadata with date and time

metadata = read.csv(paste0(meta.in.dir,"RC2 Temporal Study Responses (Responses) - Form Responses 1.csv"),stringsAsFactors = F)
metadata$Event_ID = NA
for (i in 1:nrow(metadata)) {
  
  metadata$Event_ID[i] = paste0("RC2_",paste0(rep(x = 0,(4-nchar(x = metadata$Site_Vial_ID_.4_digit_numeric_code.[i]))),collapse = ""),metadata$Site_Vial_ID_.4_digit_numeric_code.[i])

}

str(metadata)

### merge metadata with NPOC and TN

npoc.tn.meta = merge(metadata,npoc.tn.combined,by="Event_ID")
dim(npoc.tn.meta)

### merge metadata with FTICR diversity

fticr.meta = merge(metadata,fticr.div,by="Event_ID")
dim(fticr.meta)

### merge metadata with ions

ion.meta = merge(metadata,ion.compiled,by="Event_ID")
dim(ion.meta)

### read in the catchment area data from Kyongho

cat.dat = read.csv(paste0(meta.in.dir,"Rc2_allsites_0717.csv"),stringsAsFactors = F)
cat.dat = cat.dat[which(cat.dat$Study == "Temporal"),c('Study',"Name","TotDASqKM","D50_m")]
cat.dat$Name[grep(cat.dat$Name,pattern = "American")] = "American River"
cat.dat$Name[grep(cat.dat$Name,pattern = "Union Gap")] = "Union Gap"
cat.dat$Name[grep(cat.dat$Name,pattern = "Little Naches")] = "Little Naches"
cat.dat$Name[grep(cat.dat$Name,pattern = "Mabton")] = "Mabton"
cat.dat$Name[grep(cat.dat$Name,pattern = "Kiona")] = "Kiona"
cat.dat$Name[grep(cat.dat$Name,pattern = "Craig Road 1")] = "Naches- Craig Road 1"
cat.dat$Name[grep(cat.dat$Name,pattern = "Craig Road 2")] = "Naches- Craig Road 2"
str(cat.dat)

### merge catchment area data with NPOC and TN

npoc.tn.cat = merge(npoc.tn.meta,cat.dat,by.x = "Location",by.y = "Name")
npoc.tn.cat$Location[grep(npoc.tn.cat$Location,pattern = "Craig")] = "Naches-Craig"

dim(npoc.tn.cat)

### merge catchment area data with FTICR

fticr.cat = merge(fticr.meta,cat.dat,by.x = "Location",by.y = "Name")
fticr.cat$Location[grep(fticr.cat$Location,pattern = "Craig")] = "Naches-Craig"
dim(fticr.cat)

### merge catchment area data with ions

ion.cat = merge(ion.meta,cat.dat,by.x = "Location",by.y = "Name")
ion.cat$Location[grep(ion.cat$Location,pattern = "Craig")] = "Naches-Craig"
dim(ion.cat)

######
# moving into analyses

### make time series of NPOC
# lines commented out are for making plot with all the sites, which is not dialed in

npoc.tn.cat$Date_.mm.dd.yyyy. = as.Date(npoc.tn.cat$Date_.mm.dd.yyyy., "%m/%d/%Y")

################
# CAUTION !!! this is pulling out what looks like bad data, but hasn't been formally examined
npoc.tn.cat = npoc.tn.cat[which(npoc.tn.cat$NPOC_after_correcting_for_dilution < 3),]
################

#plot(npoc.tn.cat$NPOC_after_correcting_for_dilution ~ npoc.tn.cat$Date_.mm.dd.yyyy.,xaxt = "n",ylab="Dissolved OC (mg C/L)",xlab="Date (2021)",typ="n",ylim=c(0,max(npoc.tn.cat$NPOC_after_correcting_for_dilution)))
#cols.use = c(1,2,3,4,5,6)

pdf(paste0(out.dir,"Prelim_NPOC_Time.pdf"))
par(pty="s")
plot(npoc.tn.cat$NPOC_after_correcting_for_dilution ~ npoc.tn.cat$Date_.mm.dd.yyyy.,xaxt = "n",ylab="Dissolved OC (mg C/L)",xlab="Date (2021)",typ="n",ylim=c(0.5,3),cex.lab=2,cex.axis=1.5)
cols.use = c('black','red','blue')

axis(1, npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)], format(npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)], "%b %d"), cex.axis = 1.5)

i = 1
#for (curr.site in unique(npoc.tn.cat$Location)) {
for (curr.site in c('American','Craig','Kiona')) {
  points(npoc.tn.cat$NPOC_after_correcting_for_dilution[grep(curr.site,npoc.tn.cat$Location)] ~ npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)],xaxt = "n",ylab="Dissolved OC (mg C/L)",xlab="Date (2021)",col=cols.use[i])
  points(lowess(npoc.tn.cat$NPOC_after_correcting_for_dilution[grep(curr.site,npoc.tn.cat$Location)] ~ npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)],f = 0.1),typ="l",lwd=2,col=cols.use[i])
  print(c(curr.site,cols.use[i]))
  i = i + 1
}
legend(x = as.Date("2021-05-22"),y = 3,legend = c('Headwater','Primary Trib','Mainstem'),lty = 1,lwd=2,col = cols.use,cex=1.5)

dev.off()


### make time series of TN
# lines commented out are for making plot with all the sites, not dialed in

#plot(npoc.tn.cat$TN_after_correcting_for_dilution ~ npoc.tn.cat$Date_.mm.dd.yyyy.,xaxt = "n",ylab="Total N (mg N/L)",xlab="Date (2021)",typ="n",ylim=c(0,max(npoc.tn.cat$TN_after_correcting_for_dilution)))
#cols.use = c(1,2,3,4,5,6)

pdf(paste0(out.dir,"Prelim_TN_Time.pdf"))
par(pty="s")
plot(npoc.tn.cat$TN_after_correcting_for_dilution[grep(curr.site,npoc.tn.cat$Location)] ~ npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)],xaxt = "n",ylab="Total N (mg N/L)",xlab="Date (2021)",typ="n",ylim=c(0,1),cex.lab=2,cex.axis=1.5)
cols.use = c('black','red','blue')

axis(1, npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)], format(npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)], "%b %d"), cex.axis = 1.5)

i = 1
#for (curr.site in unique(npoc.tn.cat$Location)) {
for (curr.site in c('American','Craig','Kiona')) {
  points(npoc.tn.cat$TN_after_correcting_for_dilution[grep(curr.site,npoc.tn.cat$Location)] ~ npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)],col=cols.use[i])
  points(lowess(npoc.tn.cat$TN_after_correcting_for_dilution[grep(curr.site,npoc.tn.cat$Location)] ~ npoc.tn.cat$Date_.mm.dd.yyyy.[grep(curr.site,npoc.tn.cat$Location)],f = 0.1),typ="l",lwd=2,col=cols.use[i])
  print(c(curr.site,cols.use[i]))
  i = i + 1
}
legend(x = as.Date("2021-05-22"),y = 1,legend = c('Headwater','Primary Trib','Mainstem'),lty = 1,lwd=2,col = cols.use,cex=1.5)

dev.off()

########
### make time series of specific ions

#########
# !! CAUTION ONE SAMPLE REMOVED BECAUSE IT LOOKS CONTAMINATED
ion.cat = ion.cat[-which(ion.cat$Sample_ID == 'RC2_0050_ION-3'),]

ion.cat$Date_.mm.dd.yyyy. = as.Date(ion.cat$Date_.mm.dd.yyyy., "%m/%d/%Y")

for (ion.use in  c('Nitrate','Chloride','Sulfate','Calcium','Magnesium')) {

locations.to.plot = c("American River","Naches-Craig",'Kiona')

ion.cat.for.plotting = ion.cat[which(ion.cat$Location %in% c(locations.to.plot)),]

pdf(paste0(out.dir,"Prelim_",ion.use,"_Time.pdf"))
par(pty="s")

plot(ion.cat.for.plotting[,ion.use] ~ ion.cat.for.plotting$Date_.mm.dd.yyyy.,xaxt = "n",ylab=paste0(ion.use," mg / L"),xlab="Date (2021)",typ="n",ylim=c(floor(min(ion.cat.for.plotting[,ion.use])),ceiling(max(ion.cat.for.plotting[,ion.use]))),cex.lab=2,cex.axis=1.5)
cols.use = c('black','red','blue')

axis(1, ion.cat.for.plotting$Date_.mm.dd.yyyy., format(ion.cat.for.plotting$Date_.mm.dd.yyyy., "%b %d"), cex.axis = 1.5)

i = 1

for (curr.site in locations.to.plot) {
  mod.to.plot = ion.cat.for.plotting[grep(curr.site,ion.cat.for.plotting$Location),ion.use] ~ ion.cat.for.plotting$Date_.mm.dd.yyyy.[grep(curr.site,ion.cat.for.plotting$Location)]
  points(mod.to.plot,col=cols.use[i])
  points(lowess(mod.to.plot,f = 0.1),typ="l",lwd=2,col=cols.use[i])
  print(c(curr.site,cols.use[i]))
  i = i + 1
}
legend(x = as.Date("2021-04-20"),y = ceiling(max(ion.cat.for.plotting[,ion.use])),legend = c('Headwater','Primary Trib','Mainstem'),lty = 1,lwd=2,col = cols.use,cex=1.5)

dev.off()

}

######
# FTICR analyses

# time series

fticr.cat$Date_.mm.dd.yyyy. = as.Date(fticr.cat$Date_.mm.dd.yyyy., "%m/%d/%Y")

var.to.plot = "TD_SR"
ylab.use = "TD Molecular Richness"

pdf(paste0(out.dir,"Prelim_FTICR_Time",var.to.plot,".pdf"))
par(pty="s")
plot(fticr.cat[,var.to.plot] ~ fticr.cat$Date_.mm.dd.yyyy.,xaxt = "n",ylab=ylab.use,xlab="Date (2021)",typ="n",ylim=c(min(fticr.cat[,var.to.plot]),max(fticr.cat[,var.to.plot])),cex.lab=2,cex.axis=1.5)
cols.use = c('black','red','blue','green','grey','cyan')

axis(1, fticr.cat$Date_.mm.dd.yyyy., format(fticr.cat$Date_.mm.dd.yyyy., "%b %d"), cex.axis = 1.5)

i = 1
for (curr.site in unique(fticr.cat$Location)) {
#for (curr.site in c('American','Naches','Kiona')) {
  mod.to.plot = fticr.cat[grep(curr.site,fticr.cat$Location),var.to.plot] ~ fticr.cat$Date_.mm.dd.yyyy.[grep(curr.site,fticr.cat$Location)]
  points(mod.to.plot,col=cols.use[i])
  points(lowess(mod.to.plot,f = 0.1),typ="l",lwd=2,col=cols.use[i])
  print(c(curr.site,cols.use[i]))
  i = i + 1
}
#legend(x = as.Date("2021-04-20"),y = 2750,legend = c('Headwater','Primary Trib','Mainstem'),lty = 1,lwd=2,col = cols.use,cex=0.75)

dev.off()

# relating diversity to upstream basin area

fticr.means = numeric()

for (curr.site in unique(fticr.cat$Location)) {
  
  fticr.means = rbind(fticr.means,c(curr.site,unique(fticr.cat[grep(curr.site,fticr.cat$Location),c("TotDASqKM","D50_m")]),
                                           apply(fticr.cat[grep(curr.site,fticr.cat$Location),c("SR","Normalized_TR","MCD_PD","MCD_SR","TD_PD","TD_SR","SR_PCA","Norm_TR_PCA","Norm_TR_Tot_PCA","MCD_PD_PCA","MCD_SR_PCA","TD_PD_PCA","TD_SR_PCA")],MARGIN = 2,FUN = median)
                                    )
    
                      )
  
}

fticr.means = as.data.frame(fticr.means)
colnames(fticr.means)[1] = "Location"
fticr.means

var.to.plot = "SR_PCA"
ylab.use = "Number of Unique PCA Molecues"

x.var.to.plot = "TotDASqKM"
xlab.use = "Total Basin Area (km.sq)"

pdf(paste0(out.dir,var.to.plot,"_vs_",x.var.to.plot,".pdf"))
par(pty="s")
mod.to.plot = (unlist(fticr.means[,var.to.plot])) ~ (unlist(fticr.means[,x.var.to.plot]))
plot(mod.to.plot,xlab = xlab.use,ylab = ylab.use,cex.lab=2,cex.axis=1.5,pch=19,cex=1.3)
mod = summary(lm(mod.to.plot))
abline(mod,lwd=2,col=2)
mtext(text = paste(" R.sq = ",round(mod$r.squared,digits=2),sep=""),side = 3,line = -1.5,cex=1.5,adj = 0)
mtext(text = paste(" p = ",round(mod$coefficients[2,4],digits=2),sep=""),side = 3,line = -3,cex=1.5,adj = 0)
dev.off()
