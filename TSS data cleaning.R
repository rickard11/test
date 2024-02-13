#First download needed csv's and libraries
library(readxl)
library(ggplot2)
TSS<-read_xlsx("TSS_allyears.xlsx",sheet=3)
sample_locations<-read_xlsx("TSS_allyears.xlsx",sheet=4)

#remove negative values
TSS<-TSS[TSS$`TSS (mg/L)`>0 & TSS$`TSS (mg/L)`!="see above",]

#Check the notes and delete bad data
unique(TSS$NOTES)
TSS<-TSS[TSS$`Keepfor analysis`=="Y",]

#Merge with location and date date
sample_locations
data<-merge(TSS,sample_locations,by="Sample Number",all.x=TRUE)
data
#Look for any columns with missing data


#Remove a and b so that titles match
TSS$`Sample Number`<-gsub("a","",as.character(TSS$`Sample Number`))
TSS$`Sample Number`<-gsub("b","",as.character(TSS$`Sample Number`))
data<-merge(TSS,sample_locations,by="Sample Number",all.x=TRUE)
data

#change Location names to match
data$Location<-gsub("NCOS-P","Phelps",data$Location)
data$Location<-gsub("NCOS-V","Venoco",data$Location)
data$Location<-gsub("NCOS-D","Devereux",data$Location)
data$Location<-gsub("NCOS-W","Whittier",data$Location)
unique(data$Location)
data
?grepl
getwd()

write.csv(data,"C:/Users/rickard/Documents/test/TSS_andlocations.csv")

missingdata<-data[is.na(data$Location)==TRUE,]
missingdata

##Upload new file that has some 2020 data in it to reduce missing data sheet

locations2020<-read_xlsx("NCOS_Stormwater_SSC_Data_WY2020.xlsx")
colnames(locations2020)
colnames(locations2020)<-c("Sample Number","Location","Date","Time")
locations2020
locations2020<-locations2020[,1:4]
colnames(locations2020)
colnames(missingdata)
missingdata<-merge(missingdata,locations2020,by=c("Sample Number"),all.x=TRUE)
missingdata


##Load 2021 data

data2021<-read_xlsx("NCOS_Stormwater_Sample_Log_WY2021.xlsx")
colnames(data2021)
unique(data2021$Notes)
data2021<-data2021[,c(1:3,5:6)]
colnames(data2021)
colnames(data2021)<-c("Location","Sample Type","Date","Time","Sample Number")
colnames(data)
data2021
missingdata<-merge(missingdata,data2021,by=c("Sample Number"),all.x=TRUE)
locations2020$`Sample Type`<-NA



#Merge everything
colnames(data2021)
colnames(locations2020)
colnames(data)
allmissingdata<-rbind(data2021,locations2020)
allmissingdata




