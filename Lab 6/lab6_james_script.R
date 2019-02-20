library(tidyverse)
COflights <- read_csv("https://files.osf.io/v1/resources/3z5as4pej8/providers/osfstorage/5a8ca28f57103100104584db")
view(COflights)
COflights_late<-filter(COflights,ARR_DELAY>=15,CANCELLED==0)

#DIA ARRIVAL DATA/ NOT DEPARTURE/REMOVED CANCELLED AND DIVERTED
DENflights<-filter(COflights,DEST=="DEN",CANCELLED==0,DIVERTED==0)
DENflights_late<-filter(DENflights,ARR_DELAY>=15)
DENflights_test<-mutate(DENflights,LATE = ifelse(ARR_DELAY>=15,1,0))
#PROB OF BEING LATE
p_late<-34953/221280
view(DENflights)

#PLOTS AND CHECKING WHETHER CARRIER HAS AN EFFECT
ggplot(data=DENflights_test)+geom_bar(aes(x=as.factor(MONTH),fill=as.factor(LATE)),position="Dodge")
ggplot(data=DENflights_test)+geom_bar(aes(x=CARRIER,fill=as.factor(LATE)),position="Dodge")+labs(x="Carrier",y="count",title='Late Flights Per Carrier')
WN_and_late<-filter(DENflights_late,CARRIER=="WN")
WN_flights<-filter(DENflights,CARRIER=="WN")
late_given_WN = (12577/221642)/(67901/221642)
## WN has more delays. Probability of being late increases if flying WN
view(DENflights_test)
ggplot(data=DENflights_test)+geom_smooth(aes(x=DISTANCE,y=ARR_DELAY,color=as.factor(LATE)))
# PROB OF BEING LATE GIVEN FLOWN UNITED
UA_and_late<-filter(DENflights_late,CARRIER=='UA')
UA_flights<-filter(DENflights,CARRIER=='UA')
late_give_UA<-6889/61503

#PROBABILITY VERSUS CARRIER DATA
prob_vs_carrier<- DENflights_test %>%
  group_by(CARRIER)%>%
  summarize(total_late=sum(LATE),count=n())%>%
  mutate(prob = total_late/count)
view(prob_vs_carrier)

#PLOT OF DATA FROM PREVIOUS DATA
ggplot(data=prob_vs_carrier)+geom_point(aes(x=CARRIER,y=prob))+labs(x="Carrier",y="Prob. of being Late",title="Conditional Prob. of being Late Vs. Carrier")
