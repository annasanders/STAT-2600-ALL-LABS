library(tidyverse)
library(lubridate)
install.packages("OpenStreetMap")
library(OpenStreetMap)
bikes <- read_csv("Nice_ride_trip_history_2017_season.csv", col_names = TRUE, col_types= cols(`Start date`= col_datetime(format="%m/%d/%Y %H:%M"), `End date`= col_datetime(format="%m/%d/%Y %H:%M")))

bikes1 <- read_csv("Nice_ride_trip_history_2017_season.csv")

bikedate <-bikes1%>%
  separate(`Start date`, into= c("Sdate", "Stime"), sep= " ")%>%
  separate(`End date`, into= c("Edate", "Etime"), sep= " ")

bikedate1 <-bikedate%>%
  separate(Sdate, into= c("Smon", "Sday", "Syear"), sep= "/")%>%
  separate(Edate, into= c("Emon", "Eday", "Syear"), sep= "/")

view(bikedate)
view(bikedate1)

stat_loc <- read_csv("Nice_Ride_2017_Station_Locations.csv")
view(stat_loc)

m<-6300
c<-35700

casuals<-bikedate1%>%
  select(1:3, 6:9, 12:13)%>%
  filter(bikedate1$`Account type`=="Casual")


casuals<-casuals%>%
  mutate(plus0=ifelse(casuals$`Total duration (Seconds)`<=1800,1,0))%>%
  mutate(plus2=ifelse(casuals$`Total duration (Seconds)`>=1800 & casuals$`Total duration (Seconds)`<=3600,1,0))%>%
  mutate(plus4=ifelse(casuals$`Total duration (Seconds)`>=3600 & casuals$`Total duration (Seconds)`<=5400,1,0))%>%
  mutate(plus6=ifelse(casuals$`Total duration (Seconds)`>=5400 & casuals$`Total duration (Seconds)`<=7200,1,0))%>%
  mutate(plus8=ifelse(casuals$`Total duration (Seconds)`>=7200 & casuals$`Total duration (Seconds)`<=9000,1,0))%>%
  mutate(plus10=ifelse(casuals$`Total duration (Seconds)`>=9000 & casuals$`Total duration (Seconds)`<=10800,1,0))%>%
  mutate(plus12=ifelse(casuals$`Total duration (Seconds)`>=10800 & casuals$`Total duration (Seconds)`<=12600,1,0))%>%
  mutate(plus14=ifelse(casuals$`Total duration (Seconds)`>=12600 & casuals$`Total duration (Seconds)`<=14400,1,0))%>%
  mutate(plus16=ifelse(casuals$`Total duration (Seconds)`>=14400 & casuals$`Total duration (Seconds)`<=16200,1,0))%>%
  mutate(plus18=ifelse(casuals$`Total duration (Seconds)`>=16200 & casuals$`Total duration (Seconds)`<=18000,1,0))%>%
  mutate(plus20=ifelse(casuals$`Total duration (Seconds)`>=18000 & casuals$`Total duration (Seconds)`<=19800,1,0))%>%
  mutate(plus22=ifelse(casuals$`Total duration (Seconds)`>=19800 & casuals$`Total duration (Seconds)`<=21600,1,0))%>%
  mutate(plus24=ifelse(casuals$`Total duration (Seconds)`>=21600 & casuals$`Total duration (Seconds)`<=23400,1,0))%>%
  mutate(plus26=ifelse(casuals$`Total duration (Seconds)`>=23400 & casuals$`Total duration (Seconds)`<=25200,1,0))
view(casuals)           

members<-bikedate1%>%
  select(1:3, 6:9, 12:13)%>%
  filter(bikedate1$`Account type`=="Member")
members<-members%>%
  mutate(plus0=ifelse(members$`Total duration (Seconds)`<=3600,1,0))%>%
  mutate(plus2=ifelse(members$`Total duration (Seconds)`>=3600 & members$`Total duration (Seconds)`<=5400,1,0))%>%
  mutate(plus4=ifelse(members$`Total duration (Seconds)`>=5400 & members$`Total duration (Seconds)`<=7200,1,0))%>%
  mutate(plus6=ifelse(members$`Total duration (Seconds)`>=7200 & members$`Total duration (Seconds)`<=9000,1,0))%>%
  mutate(plus8=ifelse(members$`Total duration (Seconds)`>=9000 & members$`Total duration (Seconds)`<=10800,1,0))%>%
  mutate(plus10=ifelse(members$`Total duration (Seconds)`>=10800 & members$`Total duration (Seconds)`<=12600,1,0))%>%
  mutate(plus12=ifelse(members$`Total duration (Seconds)`>=12600 & members$`Total duration (Seconds)`<=14400,1,0))%>%
  mutate(plus14=ifelse(members$`Total duration (Seconds)`>=14400 & members$`Total duration (Seconds)`<=16200,1,0))%>%
  mutate(plus16=ifelse(members$`Total duration (Seconds)`>=16200 & members$`Total duration (Seconds)`<=18000,1,0))%>%
  mutate(plus18=ifelse(members$`Total duration (Seconds)`>=18000 & members$`Total duration (Seconds)`<=19800,1,0))%>%
  mutate(plus20=ifelse(members$`Total duration (Seconds)`>=19800 & members$`Total duration (Seconds)`<=21600,1,0))%>%
  mutate(plus22=ifelse(members$`Total duration (Seconds)`>=21600 & members$`Total duration (Seconds)`<=23400,1,0))%>%
  mutate(plus24=ifelse(members$`Total duration (Seconds)`>=23400 & members$`Total duration (Seconds)`<=25200,1,0))
view(members)  

cas_sum<-casuals%>%
  summarize(c0=2*sum(plus0),c2=4*sum(plus2),c4=6*sum(plus4),c6=8*sum(plus6),c8=10*sum(plus8),c10=12*sum(plus10),c12=14*sum(plus12),c14=16*sum(plus14),c16=18*sum(plus16),c18=20*sum(plus18),c20=22*sum(plus20),c22=24*sum(plus22),c24=26*sum(plus24),c26=28*sum(plus26))
view(cas_sum)
mem_sum<-members%>%
  summarize(c2=2*sum(plus2),c4=4*sum(plus4),c6=6*sum(plus6),c8=8*sum(plus8),c10=10*sum(plus10),c12=12*sum(plus12),c14=14*sum(plus14),c16=16*sum(plus16),c18=18*sum(plus18),c20=20*sum(plus20),c22=22*sum(plus22),c24=24*sum(plus24))
view(mem_sum)

m_rev=75*m+rowSums(mem_sum)
c_rev=rowSums(cas_sum)

casuals<-casuals%>%
  mutate(overTime=ifelse(casuals$`Total duration (Seconds)`>1800,1,0))
members<-members%>%
  mutate(overTime=ifelse(members$`Total duration (Seconds)`>3600,1,0))
plotdata<-bikedate1%>%
  select(12:13)%>%
  mutate(c_overTime=ifelse(`Total duration (Seconds)`>1800 & bikedate1$`Account type`=="Casual",1,0))%>%
  mutate(m_overTime=ifelse(`Total duration (Seconds)`>3600 & `Account type`=="Member",1,0))%>%
  mutate(ot=ifelse(c_overTime==1 | m_overTime==1,1,0))
ggplot(data=plotdata)+geom_bar(aes(x=as.factor(`Account type`),fill=as.factor(ot)))+labs(x="Account Type",y='Count',fill="0 Not Late, 1 Late",title='Lateness versus Account Type')

