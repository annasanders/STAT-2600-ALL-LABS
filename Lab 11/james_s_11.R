library(tidyverse)
library(babynames)
view(babynames)

james<-babynames%>%
  filter(str_detect(name, "^James$")==1,sex=="M")

jam<-babynames%>%
  filter(str_detect(name, "^Jam.")==1,sex=="M")%>%
  group_by(year)%>%
  summarize(prop=sum(prop))%>%
  

view(jam)
ggplot()+geom_line(data=jam,aes(x=year,y=prop,color='Start with Jam'))+geom_line(data=james,aes(x=year,y=prop,color='James'))


pop_james_a<-james%>%
  filter(between(year,1975,2017))
view(pop_james_a)

ggplot()+geom_line(data=pop_james_a,mapping=aes(x=year,y=prop))+labs(x='Year',y='Proportion',title='Proportion of the name James from 1975 to 2017')


babynamest<-babynames%>%
  arrange(desc(prop))%>%
  mutate(j=ifelse(name=='James',row_number(),0))%>%
  filter(name=='James')%>%
  mutate(perct = (1924665-j)/1924665)
view(babynamest)

ggplot()+geom_smooth(data=babynamest,mapping=aes(x=year,y=perct))+labs(x='Year',y='Percentile',title='Relative Percentile of Proportion of James')
babynames<-babynames
view(babynames)
#GROUP STUFF
fnames<-babynames%>%
  filter(sex=='F')%>%
  arrange(desc(prop))
view(fnames)
ariel<-fnames%>%
  filter(str_detect(name,"Ar[iy]+.l"))
view(ariel)



r_ariel<-ariel%>%
  filter(between(year, 1988,1990))%>%
  group_by(year)%>%
  summarize(prop=sum(prop))

r_fnames<-fnames%>%
  filter(between(year,1988,1990))%>%
  arrange(desc(n))%>%
  filter(name=='Jessica'|name=='Amanda'|name=='Ashley'|name=='Sarah')%>%
  group_by(year,name)%>%
  summarize(prop=sum(prop))
view(r_fnames)
view(r_ariel)
ggplot()+geom_line(data=r_ariel,aes(x=year,y=prop,color="Ariel"))+geom_line(data=r_fnames,aes(x=year,y=prop,color=name))+labs(x='Year',y='Proportion',title="Proportion of Popular Names and Ariel")


