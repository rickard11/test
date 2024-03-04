#Start with Phelps

#load TSS concentration data
TSS_data<-read.csv("data/2020_2021_TSS_Fulldataset.csv")
#clean TSS data a bit
TSS_data<-TSS_data[!is.na(TSS_data$TSS..mg.L.),]
TSS_data$Datetime<-as.POSIXct(paste0(TSS_data$Date," ",TSS_data$Time),format="%Y-%m-%d %H:%M:%S")
#subdivide into only phelps concentrations
Phelps_TSS<-TSS_data[TSS_data$Location=="NCOS-P"|TSS_data$Location=="Phelps",]
#Load stage height for Phelps
Phelpsdepth<-read.csv("data/PhelpsBridge_PTdata_2018-2022wy.csv")

#Get packages for flow
library(lmom)
#Load flow measurements
Flow<-read_xlsx("data/Stormwaterexport_cfs.xlsx")
#I used a combination of these 2 references to create our rating curve
#https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/
#https://rpubs.com/cassiorampinelli/528388
#Flowis in cfs, need to convert to cms
Flow$Q<-Flow$cfs*0.0283168
#Convert water depth from ft to m
Flow$W<-Flow$`Water depth`*0.3048
#Create rating curve for Phelps
PhelpsFlow<-Flow[Flow$Location=="Phelps",] #Isolate only Phelps creek
PhelpsFlow<-PhelpsFlow[,9:10] #We need only discharge and stage

#Generate a power regression for the rating curve
m2<-nls(PhelpsFlow$W~a2*PhelpsFlow$Q^b2,data=PhelpsFlow,start=list(a2=1,b2=1))
#Summary of the regression
summary(m2)
#Coefficients for equation
a <- round(summary(m2)$coefficients[1], 3)
b <- round(summary(m2)$coefficients[2], 3)
#Plotting Rating curve
plot(PhelpsFlow$W~ PhelpsFlow$Q, main = "Rating Curve ",xlab="Discharge (cms)",ylab="Height (m)")
#plotting regression model
q <- seq(0.00001, 1, 0.001)
lines(q, a*q^b, lty = 1, col = "black")
#Equation
Height<-a*q^b
df<-data.frame(q,Height) #Convert equation to a dataframe
colnames(df)<-c("discharge.cms","height.m")

#Merge rating curve equation with stage height values from the nutrient table
head(df)
head(Phelpsdepth)
#Determine area of Phelps
## Phelps = ~450 ha
## Devereux = ~250 ha
## Venoco =~100 + Devereux + Phelps
#Multiple nutrients by estimate liters of water for given time period
#divide for number of Ha (size of Phelps watershed) final answer will be kg/ha/ water year
