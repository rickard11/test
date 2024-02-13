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
locations2020
locations2020<-locations2020[,1:4]

missingdata<-merge(missingdata,locations2020,by="Sample Number",all.x=TRUE)
missingdata
