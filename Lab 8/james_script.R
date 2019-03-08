library(tidyverse)
library(readxl)
suicides <- read.table(file= "master.xls", sep= "\t", header= TRUE)
view(suicides)
s2<-filter(suicides,population>1000000,year==2010|year==2011|year==2012|year==2013|
             year==2014|year==2015|year==2016)
view(s2)
s3<-s2%>%
  select(1:9)
view(s3)
#look at number of suicides versus suicides per 100k people
s_density <- select(s3, country, year, 4:7)
view(s_density)

dvt<-s_density%>%
  group_by(country,year)%>%
  summarize(suicides=sum(suicides_no),pop=sum(population),den=sum(suicides.100k.pop))
view(dvt)
dvt<-arrange(dvt,desc(pop),country)
dvt85 <- filter(dvt,pop>=85000000)
view(dvt30)
ggplot(data=dvt85)+geom_point(aes(x=year,y=den,color=as.factor(year)))+facet_wrap(~country)+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())+labs(x="",y="Density per 100k",color='Year')

#LOOKING AT HDI
hdi<-s3%>%
  select(1:7,9)%>%
  filter(!is.na(s3$HDI.for.year))%>%
  group_by(country,year)%>%
  summarize(suicides=sum(suicides_no),pop=sum(population),den=sum(suicides.100k.pop),hdi=min(`HDI.for.year`))%>%
  arrange(desc(hdi))
view(hdi)       
ggplot(data=hdi)+geom_point(aes(x=den,y=hdi,color=pop))+labs(x='Suicide Density per 100k',y='HDI',color='Population',title='HDI versus Suicide Density')+geom_smooth(aes(x=den,y=hdi))

russia<-s3 %>%
  filter(country=='Russian Federation')
view(russia)
