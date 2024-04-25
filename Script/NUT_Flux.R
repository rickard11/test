#merge with nutrient data
#use existing number to fill in gaps
Phelpsflux<-merge(Phelps_nut,Phelpsstorms,by="Datetime",all.x=TRUE,all.y = TRUE)
Phelpsflux$wtr_yr <- getYearQuarter(Phelpsflux$Datetime, firstMonth=10)#GetYear function is written in data cleaning script
#use existing number to fill in gaps
na_indices <- which(is.na(Phelpsflux$TSS..mg.L.))
# Interpolate NA values

Phelpsflux$TSS..mg.L.[na_indices] <- approx(seq_along(Phelpsflux$TSS..mg.L.)
                                            [!is.na(Phelpsflux$TSS..mg.L.)],Phelpsflux$TSS..mg.L.
                                            [!is.na(Phelpsflux$TSS..mg.L.)], xout = na_indices)$y
#add 8mg/L to 3380-3390
#add nutrients up to 171 (which is 1054mg/l)
#I also need to seperate into years because the change from one water year to the next (and one storm to the next)
#is being interpolated based on the previous value which would not make sense.

#multiple by number of litres
Phelpsflux$mgTSS_15m<-Phelpsflux$TSS..mg.L.*Phelpsflux$discharge_litre_15m
#aggregate the sum of mg TSS per year
yrlyphelpsflux<-aggregate(mgTSS_15m~wtr_yr,Phelpsflux,FUN=sum)
#convert to kg
yrlyphelpsflux$kgTSS_yr<-yrlyphelpsflux$mgTSS_15m*1e-6
#divide by ha
yrlyphelpsflux$kg_yr_ha<-yrlyphelpsflux$kgTSS_yr/450
#compare to Melacks findings 

#split into seperate water years
str(Phelpsdischarge)
Pdis2020<-Phelpsstorms[Phelpsstorms$Datetime>="2019-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2020-10-01 00:00:00",]
Pdis2020$discharge.cf15min<-Pdis2020$discharge.cfs*60*15
Pdis2020<-Pdis2020[!is.na(Pdis2020$discharge.cf15min),]


Pdis2021<-Phelpsstorms[Phelpsstorms$Datetime>="2020-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2021-10-01 00:00:00",]
Pdis2021$discharge.cf15min<-Pdis2021$discharge.cfs*60*15
Pdis2021<-Pdis2021[!is.na(Pdis2021$discharge.cf15min),]

Pdis2022<-Phelpsstorms[Phelpsstorms$Datetime>="2021-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2022-10-01 00:00:00",]
Pdis2022$discharge.cf15min<-Pdis2022$discharge.cfs*60*15
Pdis2022<-Pdis2022[!is.na(Pdis2022$discharge.cf15min),]




sum(Pdis2020$discharge.cf15min)
sum(Pdis2021$discharge.cf15min)
sum(Pdis2022$discharge.cf15min)


#Determine area of Phelps
## Phelps = ~450 ha
## Devereux = ~250 ha
## Venoco =~100 + Devereux + Phelps
#Multiple nutrients by estimate liters of water for given time period
#divide for number of Ha (size of Phelps watershed) final answer will be kg/ha/ water year
