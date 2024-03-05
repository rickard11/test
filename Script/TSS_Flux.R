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
##Need to fix q
q <- seq(0.00001, 150, 0.0001)
tail(q)
lines(q, a*q^b, lty = 1, col = "black")
#Equation for deepest point height
Height<-a*q^b
df<-data.frame(q,Height) #Convert equation to a dataframe
colnames(df)<-c("discharge.cms","height.m")
df$height_ft<-df$height.m*3.28 #convert depth to feet to compare to expectation
df$ptdepth_ft<-df$height_ft-0.77 #convert height at deepest point to expected pt reading

#Merge rating curve equation with stage height values from the nutrient table change to american units
tail(df)
plot(df$height.m~df$discharge.cms)
df$height_ft<-round(df$height_ft,2)
df$ptdepth_ft<-round(df$ptdepth_ft,2)
df$discharge.cfs<-df$discharge.cms*(3.28*3.28*3.28)
df<-df[,c(3,4,5)]
head(df)
#We need only 1 value for each 0.01
dfagg<-aggregate(discharge.cfs~ptdepth_ft,df,FUN=mean)
dfagg<-dfagg[11:606,]
#Make a dataset to fill in missing low numbers then merge
ptdepth_ft<-c(0.99,1.01,1.03,1.05,1.08,1.12)
discharge.cfs<-c(3.7e-02,4.05e-02,4.4e-02,4.75e-02,5.45e-02,6.5e-02)
add<-data.frame(ptdepth_ft,discharge.cfs) #Made new dataframe with missing numbers based on other number in list
dfagg2<-rbind(add,dfagg) #Combined 2 lists.

#Upload pt depth file
Phelpsdepth<-read.csv("data/PhelpsBridge_PTdata_2018-2022wy.csv")
Phelpsdepth<-Phelpsdepth[!is.na(Phelpsdepth$LEVEL),]
Phelpsdepth$LEVEL<-round(Phelpsdepth$LEVEL,2)
Phelpsdepth<-Phelpsdepth[Phelpsdepth$LEVEL>0.97,]#anything under 0.97 has insignificant flow
#keep only important stuff
PD<-Phelpsdepth[,c(2,3,4)]
colnames(PD)[3]<-"ptdepth_ft"
# I need to merge Phelps depth with the predicted cfs
head(PD)
head(dfagg2)
Phelpsdischarge<-merge(PD,dfagg2,by="ptdepth_ft",all.x = TRUE)
Phelpsdischarge<-Phelpsdischarge[!is.na(Phelpsdischarge$ptdepth_ft),]
Phelpsdischarge
Phelpsdischarge$Date<-format(as.Date(Phelpsdischarge$Date,format="%Y-%m-%d"))
Phelpsdischarge$Datetime<-paste0(Phelpsdischarge$Date," ",Phelpsdischarge$Time)
Phelpsdischarge$Datetime<-as.POSIXct(Phelpsdischarge$Datetime,format="%Y-%m-%d %H:%M:%S")
#split into seperate water years
str(Phelpsdischarge)
Pdis2020<-Phelpsdischarge[Phelpsdischarge$Datetime>="2019-01-01 00:00:00"&
                            Phelpsdischarge$Datetime<="2020-10-01 00:00:00",]


Pdis2021<-Phelpsdischarge[Phelpsdischarge$Datetime>="2020-01-01 00:00:00"&
                            Phelpsdischarge$Datetime<="2021-10-01 00:00:00",]





















#Determine area of Phelps
## Phelps = ~450 ha
## Devereux = ~250 ha
## Venoco =~100 + Devereux + Phelps
#Multiple nutrients by estimate liters of water for given time period
#divide for number of Ha (size of Phelps watershed) final answer will be kg/ha/ water year
