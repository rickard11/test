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
#install.packages("lmom")
library(lmom)
library(readxl)
#Load flow measurements
Flow<-read_xlsx("data/Stormwaterexport_cfs.xlsx")
#I used a combination of these 2 references to create our rating curve
#https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/
#https://rpubs.com/cassiorampinelli/528388
#Flowis in cfs, need to convert to cms
Flow$Q<-Flow$cfs*0.0283168
#Convert water depth from ft to m
Flow$W<-Flow$`Water depth`*0.3048
Flow<-as.data.frame(Flow)
Flow<-Flow[c(1:17,19:33),]
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
plot(PhelpsFlow$W~ PhelpsFlow$Q, main = "Rating Curve ",xlab="Discharge (cms)",ylab="Height (m)",
     xlim=c(0,7),ylim=c(0,1.75))

#plotting regression model
?seq
q <- seq(0.00000001, 7, 0.01)

lines(q, a*q^b, lty = 1, col = "black")
#Equation
Height<-a*q^b
df<-data.frame(q,Height) #Convert equation to a dataframe
colnames(df)<-c("discharge.cms","height.m")
#Used excel to calculate the linear regression for flow depth to ptdepth
#This is making the pt height lower than deepest height need to fix
df$ptdepth_ft<-(0.5009*df$height.m+0.3408)*3.28 #linear regression +conversion to feet
df$height_ft<-df$height.m*3.28 #convert depth to feet to compare to expectation
#Merge rating curve equation with stage height values from the nutrient table
tail(df)
df$height_ft<-format(df$height_ft,digits=2)
df$ptdepth_ft<-format(df$ptdepth_ft,digits=3)
df<-df[,c(1,3,4)]


Phelpsdepth<-read.csv("data/PhelpsBridge_PTdata_2018-2022wy.csv")
Phelpsdepth<-Phelpsdepth[!is.na(Phelpsdepth$LEVEL),]
Phelpsdepth$LEVEL<-round(Phelpsdepth$LEVEL,2)
Phelpsdepth<-Phelpsdepth[Phelpsdepth$LEVEL>0,]
PD<-Phelpsdepth[,c(2,3,4)]
colnames(PD)[3]<-"ptdepth_ft"
# I need to merge Phelps depth with the predicted cfs
head(PD)
head(df)
test<-merge(PD,df,by="ptdepth_ft",all.x = TRUE)
test[10000:10100,]
#Determine area of Phelps
## Phelps = ~450 ha
## Devereux = ~250 ha
## Venoco =~100 + Devereux + Phelps
#Multiple nutrients by estimate liters of water for given time period
#divide for number of Ha (size of Phelps watershed) final answer will be kg/ha/ water year
