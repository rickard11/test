## Isolating Phelps samples for TSS to determine kg/ha

TSS_data<-read.csv("2020_2021_TSS_Fulldataset.csv")

#Phelps watershed size= about 500 ha

#calculate mean of TSS
TSS_data<-TSS_data[!is.na(TSS_data$TSS..mg.L.),]
TSS_data$Datetime<-as.POSIXct(paste0(TSS_data$Date," ",TSS_data$Time),format="%Y-%m-%d %H:%M:%S")
str(TSS_data)
mean(TSS_data$TSS..mg.L.)

# separate locations into separate spreadsheets
unique(TSS_data$Location)
Phelps_TSS<-TSS_data[TSS_data$Location=="NCOS-P"|TSS_data$Location=="Phelps",]
Devereux_TSS<-TSS_data[TSS_data$Location=="NCOS-D"|TSS_data$Location=="Devereux",]
Whittier_TSS<-TSS_data[TSS_data$Location=="NCOS-W"|TSS_data$Location=="Whittier",]
Venoco_TSS<-TSS_data[TSS_data$Location=="NCOS-V"|TSS_data$Location=="Venoco",]


## Load depth data for each site to calculate volume weighted mean
Phelpsdepth<-read.csv("PhelpsBridge_PTdata_2018-2022wy.csv")
Devereuxdepth<-read.csv("DevereuxCreek_PTdata_2018-2022wy.csv")
Venocodepth<-read.csv("Venoco_PTdata_2018-2022wy.csv")


## Structure date times to be mergable and remove unneccessary columns
Phelpsdepth$Date<-as.Date(Phelpsdepth$Date,format="%Y-%m-%d")
Phelpsdepth$Datetime<-as.POSIXct(paste0(Phelpsdepth$Date," ",Phelpsdepth$Time),format="%Y-%m-%d %H:%M:%S")
Phelpsdepth<-Phelpsdepth[,c(8,4)]
Devereuxdepth$Date<-as.Date(Devereuxdepth$Date,format="%m/%d/%Y")
Devereuxdepth$Datetime<-as.POSIXct(paste0(Devereuxdepth$Date," ",Devereuxdepth$Time),format="%Y-%m-%d %H:%M:%S")
Devereuxdepth<-Devereuxdepth[,c(7,4)]
Venocodepth$Date<-as.Date(Venocodepth$Date,format="%m/%d/%Y")
Venocodepth$Datetime<-as.POSIXct(paste0(Venocodepth$Date," ",Venocodepth$Time),format="%Y-%m-%d %H:%M:%S")
Venocodepth<-Venocodepth[,c(8,4)]


## Merge TSS data with depth data and keep all TSS data (will involve some data cleaning first)
Phelpsall<-merge(Phelps_TSS,Phelpsdepth,by="Datetime",all.x=TRUE)
Venocoall<-merge(Venoco_TSS,Venocodepth,by="Datetime",all.x=TRUE)
Devereuxall<-merge(Devereux_TSS,Devereuxdepth,by="Datetime",all.x=TRUE)


##Calculate volume weighted average
##merge back together
##Aggregate to get VWM TSS/ site / year and average of all NCOS/yr


