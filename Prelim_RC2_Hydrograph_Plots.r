# code for preliminary plots of hydrographs at RC2 temporal sites

in.dir = "//pnl/projects/SBR_SFA/RC2/03_Temporal_Study/09_Gauge_Data/"
out.dir = "C:/Users/steg815/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/RC2/Prelim_Data_Integration/"

# define site names tied to codes
site.codes = data.frame(Site_code = c(12488500,12500450,12508990,12510500),Location = c('American River','Union Gap','Mabton','Kiona'),plot.col = c('black','green','gray','blue'))

# read in data for all usgs sites
usgs.height.dat = read.csv(paste0(in.dir,"height_ft_RC2_15min.csv"),stringsAsFactors = F)
colnames(usgs.height.dat)[which(colnames(usgs.height.dat) == 'X_00065_00000')] = "Height_Ft"
head(usgs.height.dat)

# summarize into daily data and plot each site across the temporal study sampling period

usgs.height.dat$Date = substr(usgs.height.dat$dateTime,start = 1,stop = 10)
usgs.height.dat$site_day = paste(usgs.height.dat$site_no,usgs.height.dat$Date,sep="_")
usgs.height.daily = as.data.frame(tapply(X = usgs.height.dat$Height_Ft,INDEX = usgs.height.dat$site_day,FUN = median))
colnames(usgs.height.daily) = "Height_Ft"
usgs.height.daily$Date = sub(pattern = ".*_",replacement = "",x = rownames(usgs.height.daily))
usgs.height.daily$site_no = sub(pattern = "_.*",replacement = "",x = rownames(usgs.height.daily))
rownames(usgs.height.daily) = NULL
usgs.height.daily$Date = as.Date(usgs.height.daily$Date, "%Y-%m-%d")
head(usgs.height.daily)

# plot time series for selected sites
start.date = as.Date("2021-04-20", "%Y-%m-%d")
end.date = as.Date("2021-06-24", "%Y-%m-%d")

for (curr.site in c("American River","Kiona")) {
  
  pdf(paste(out.dir,curr.site,"_Hydrograph.pdf",sep=""))
  par(pty="s")
  curr.site.no = site.codes$Site_code[which(site.codes$Location == curr.site)]
  curr.col = as.character(site.codes$plot.col[which(site.codes$Location == curr.site)])
  plot(usgs.height.daily$Height_Ft[which(usgs.height.daily$site_no == curr.site.no & usgs.height.daily$Date >= start.date & usgs.height.daily$Date <= end.date)]/max(usgs.height.daily$Height_Ft[which(usgs.height.daily$site_no == curr.site.no)]) ~ usgs.height.daily$Date[which(usgs.height.daily$site_no == curr.site.no & usgs.height.daily$Date >= start.date & usgs.height.daily$Date <= end.date)],xlim=c(start.date,end.date),typ="l",xlab="Date",ylab="Relative River Stage",cex.lab=2,cex.axis=1.5,lwd=2,col=curr.col,main=curr.site,xaxt='n')
  axis(side = 1,at = as.Date(c("2021-04-20","2021-05-06","2021-05-20","2021-06-03","2021-06-17"),"%Y-%m-%d"),labels = c('Apr 20','May 06',"May 20","Jun 03","Jun 17"),cex.axis=1.5)
  dev.off()
  
}

######
# read in BoR data

# plotting for Naches at Craig road
naches.craig = read.csv(paste0(in.dir,"NACW daily gauge height and discharge (2020-04-01 to 2021-07-19).csv"),stringsAsFactors = F)
naches.craig$DATE = as.Date(x = naches.craig$DATE,"%m/%d/%Y")
head(naches.craig)
str(naches.craig)

curr.site = c("Naches-Craig")
pdf(paste(out.dir,curr.site,"_Hydrograph.pdf",sep=""))
par(pty="s")
curr.col = "red"
plot(naches.craig$Height_ft[which(naches.craig$DATE >= start.date & naches.craig$DATE <= end.date)]/max(naches.craig$Height_ft) ~ naches.craig$DATE[which(naches.craig$DATE >= start.date & naches.craig$DATE <= end.date)],typ="l",xlab="Date",ylab="Relative River Stage",cex.lab=2,cex.axis=1.5,lwd=2,col=curr.col,main=curr.site,xaxt='n')
axis(side = 1,at = as.Date(c("2021-04-20","2021-05-06","2021-05-20","2021-06-03","2021-06-17"),"%Y-%m-%d"),labels = c('Apr 20','May 06',"May 20","Jun 03","Jun 17"),cex.axis=1.5)
dev.off()
