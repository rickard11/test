## Isolating Phelps samples for TSS to determine kg/ha

TSS_data<-read.csv("2020_2021_TSS_Fulldataset.csv")

#Phelps watershed size= about 500 ha

#calculate mean of TSS
TSS_data<-TSS_data[!is.na(TSS_data$TSS..mg.L.),]
mean(TSS_data$TSS..mg.L.)


## Load data to calculate volume weighted mean
Phelpsdepth<-read.csv("PhelpsBridge_PTdata_2018-2022wy.csv")
