#Import Data Train ====================================================================
library(tidyverse)
library(dplyr)
dt.train<-read.csv("D:/Gelora Sigma/BRI Hackathon/Cash Ratio/train (2).csv")
train<-dt.train%>%
  select(periode, kas_kantor, kas_echannel)

#Membuat Lag1=Day(i)-Day(i-1)
hsl2<-NULL
for(j in 2:ncol(train)){
hsl1<-NULL
for(i in 2:nrow(train)){
hslr<-train[i,j]-train[i-1,j]
hsl1<-rbind(hsl1, hslr)
}
hsl2<-cbind(hsl2, hsl1)
}
colnames(hsl2)<-paste(colnames(train[,-1]), "_Lag1")
barisNA<-rbind(rep(NA,2))
lag1<-rbind(barisNA, hsl2)
datalag<-cbind(train,lag1)

#Membuat Kolom Date dan Day
datalag$periode<-as.Date(datalag$periode)
datalag<-datalag%>%
  mutate(day=weekdays(as.Date(periode)))%>%
  mutate(date=substr(periode, 9, 10))
  
dtlag<-datalag%>%
  select(-c(kas_kantor, kas_echannel))
dtlag<-dtlag[-1,]

#Rataan Tanggal (1-31) dan Rataan Hari(Senin-Minggu)
AvgDay<-aggregate(dtlag[,2:3], list(dtlag$day), mean)
AvgDay<-AvgDay%>%
  mutate(day=Group.1)%>%
  select(-Group.1)
AvgDate<-aggregate(dtlag[2:3], list(dtlag$date), mean)
AvgDate<-AvgDate%>%
  mutate(date=Group.1)%>%
  select(-Group.1)

#Pendugaan pada Data Train (September 2020)=================================================
dttest<-datalag[395:425,c(1:3,6:7)]
library(data.table)
day1<-data.table(AvgDay, key="day")
day2<-data.table(dttest, key="day")
joinday<-day2[day1]
joinday<-joinday%>%
  mutate(avgday_kantor=`kas_kantor _Lag1`, avgday_channel=`kas_echannel _Lag1`)%>%
  select(-c(`kas_kantor _Lag1`, `kas_echannel _Lag1`))

date1<-data.table(AvgDate, key="date")
date2<-data.table(dttest, key="date")
joindate<-date2[date1]
joindate<-joindate%>%
  mutate(avgdate_kantor=`kas_kantor _Lag1`, avgdate_channel=`kas_echannel _Lag1`)%>%
  select(-c(`kas_kantor _Lag1`, `kas_echannel _Lag1`))

dt1<-data.table(joinday, key="periode")
dt2<-data.table(joindate, key="periode")
dt.test<-dt1[dt2]
dt.test<-dt.test%>%
  select(-c(i.kas_kantor, i.kas_echannel, i.day, i.date))

dt.forecast<-dt.test%>%
  mutate(weighted_avg_kantor=0.4*avgdate_kantor+0.6*avgday_kantor)%>%
  mutate(weighted_avg_channel=0.4*avgdate_channel+0.6*avgday_channel)%>%
  mutate(index=0:30)

#Dugaan Kas Kantor September 2020
fc_kantor<-data.frame(fc_kantor=matrix(nrow = 31, ncol = 1))
fc_kantor[2,1]=dt.forecast[2,10]+dt.forecast[1,2]
dt.forecast<-cbind(dt.forecast, fc_kantor)

for(i in 3:nrow(dt.forecast)){
  dt.forecast[i,13]<-dt.forecast[i,10]+dt.forecast[i-1,13]
}

#Dugaan Kas Echannel September 2020
fc_channel<-data.frame(fc_channel=matrix(nrow = 31, ncol = 1))
fc_channel[2,1]=dt.forecast[2,11]+dt.forecast[1,3]
dt.forecast<-cbind(dt.forecast, fc_channel)

for(i in 3:nrow(dt.forecast)){
  dt.forecast[i,14]<-dt.forecast[i,11]+dt.forecast[i-1,14]
}

dt.forecast<-dt.forecast%>%
  mutate(MSE_kantor=abs((fc_kantor-kas_kantor)/kas_kantor))%>%
  mutate(MSE_channel=abs((fc_channel-kas_echannel)/kas_echannel))

#Prediksi Oktober 2020 =====================================================================
dt.okto<-data.frame(periode=matrix(nrow=31, ncol=1))
dt.okto[,1]<-seq(as.Date("2020-10-01"), as.Date("2020-10-31"), by="days")

dt.okto<-dt.okto%>%
  mutate(day=weekdays(as.Date(periode)))%>%
  mutate(date=substr(periode, 9, 10))

okto.day1<-data.table(AvgDay, key="day")
okto.day2<-data.table(dt.okto, key="day")
joinday.okto<-okto.day2[okto.day1]
joinday.okto<-joinday.okto%>%
  mutate(avgday_kantor=`kas_kantor _Lag1`, avgday_channel=`kas_echannel _Lag1`)%>%
  select(-c(`kas_kantor _Lag1`, `kas_echannel _Lag1`))

okto.date1<-data.table(AvgDate, key="date")
okto.date2<-data.table(dt.okto, key="date")
joindate.okto<-okto.date2[okto.date1]
joindate.okto<-joindate.okto%>%
  mutate(avgdate_kantor=`kas_kantor _Lag1`, avgdate_channel=`kas_echannel _Lag1`)%>%
  select(-c(`kas_kantor _Lag1`, `kas_echannel _Lag1`))

okto1<-data.table(joinday.okto, key="periode")
okto2<-data.table(joindate.okto, key="periode")
okto<-okto2[okto1]
oct<-okto%>%
  select(-c(i.day,i.date))

oct<-oct%>%
  mutate(weighted_avg_kantor=0.4*avgdate_kantor+0.6*avgday_kantor)%>%
  mutate(weighted_avg_channel=0.4*avgdate_channel+0.6*avgday_channel)

#Prediksi Kas Kantor Oktober 2020
fc_kantor_oct<-data.frame(fc_kantor=matrix(nrow = 31, ncol = 1))
fc_kantor_oct[1,1]=dt.forecast[31,2]+oct[1,8]
oct<-cbind(oct, fc_kantor_oct)

for(i in 2:nrow(oct)){
  oct[i,10]<-oct[i,8]+oct[i-1,10]
}

#Prediksi Kas Echannel Oktober 2020
fc_channel_oct<-data.frame(fc_channel=matrix(nrow = 31, ncol = 1))
fc_channel_oct[1,1]=dt.forecast[31,3]+oct[1,9]
oct<-cbind(oct, fc_channel_oct)

for(i in 2:nrow(oct)){
  oct[i,11]<-oct[i,9]+oct[i-1,11]
}

#Menyimpan Data Frame=======================================================================
write.csv(dt.forecast, 
          "D:/Gelora Sigma/BRI Hackathon/Cash Ratio/Hasil Prediksi September 2020.csv")
write.csv(oct, 
          "D:/Gelora Sigma/BRI Hackathon/Cash Ratio/Hasil Prediksi Oktober 2020.csv")

submit_vol2=data.frame(matrix(nrow=62, ncol=2))
submit_vol2[,1]=0:61
submit_vol2[,2]=c(oct$fc_kantor, oct$fc_channel)
colnames(submit_vol2)=c("index", "value")

write.csv(submit_vol2, 
          "D:/Gelora Sigma/BRI Hackathon/Cash Ratio/submit_vol2.csv")
