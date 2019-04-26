library(tidyverse)
source("http://www.openintro.org/stat/data/cdc.R")
library(modelr)

#INDIVIDUAL STUFF

cdc <- as_tibble(cdc)
view(cdc)
ggplot(data=cdc,aes(x=weight,y=exp(height)))+geom_smooth(se=FALSE)
ggplot(data=cdc,aes(x=weight,y=height,color=gender))+geom_point(size=0.5)+geom_smooth(method='lm')+facet_wrap(~gender)




whlm<-lm(height~weight*gender,data=cdc)
grid<-cdc%>%
  data_grid(weight,gender)%>%
  add_predictions(whlm)
view(grid)

m<-cdc%>%
  filter(gender=='m')
f<-cdc%>%
  filter(gender=='f')
f_model<-lm(height~weight,data=f)
m_model<-lm(height~weight,data=m)
m<-m%>%
  add_residuals(m_model)
f<-f%>%
  add_residuals(f_model)
ggplot(data=m)+geom_freqpoly(aes(resid,color='Male Residuals'))+geom_freqpoly(aes(resid,color='Female Residuals'),data=f)+labs(title="Residuals for Linear Model",x="Residual")

ggplot(data=m)+geom_point(aes(x=weight,y=resid,color='Male Residuals'),size=0.4,shape='triangle')+geom_point(aes(x=weight,y=resid,color='Female Residuals'),data=f,size=0.4)+labs(title="Residuals for Linear Model",x="Weight")


#TEAM STUFF
cdc<-cdc%>%
  mutate(loseW=ifelse(wtdesire<weight,1,0))%>%
  mutate(trying=loseW*exerany)

#What are the differences between different ages and genders
#in the amount of exercise per month?

ggplot(data=cdc)+geom_count(aes(x=as.factor(loseW),y=as.factor(exerany)))+facet_wrap(~gender)
#GENERAL
probs<-cdc%>%
  summarize(try=sum(trying),ntry=sum(loseW)-sum(trying))%>%
  mutate(prob_try=try/(try+ntry),prob_ntry=ntry/(ntry+try))
view(probs)
#GENDER BASED
prob_m<-m%>%
  summarize(try=sum(trying),ntry=sum(loseW)-sum(trying))%>%
  mutate(prob_try=try/(try+ntry),prob_ntry=ntry/(ntry+try))
prob_f<-f%>%
  summarize(try=sum(trying),ntry=sum(loseW)-sum(trying))%>%
  mutate(prob_try=try/(try+ntry),prob_ntry=ntry/(ntry+try))
view(prob_f)
#Ok so for people that want to lose weight, 25% of them do not go to the gym. Also more women
#skip the gym for some reason when they want to lose weight. 

#Want to look at the difference of percentages between men and women that try. Then perform a permutation test possible. 
mpt<-prob_m$try/count(m)
fpt<-prob_f$try/count(f)

d <- fpt - mpt
#Is the difference statistically significant? Let's find out. 

perm_mean <- function(perms, values, n1)
{
  ## Variables ##
  # perms: The number of permutations 
  # values (num): 
  # n1 (int): Size of group 1
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms" to store
  # permuted mean differnces
  p<-vector("integer",1000)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2:
    # Randomly separate vector "values" into disjoint 
    # groups of size "n1" and "length(values) - n1" respectively
    rows1<-sample(length(values),n1)
    rows2<-vector('integer',length(values))
    g1<-values[rows1]
    for(j in seq(1:length(values))) 
    { 
      if (is.element(j,rows1)==FALSE) rows2[j]<-j 
    }
    #REMOVE ALL 0s WITH TRUE FALSE VECTOR
    tf<-rows2>0
    rows2<-rows2[tf]
    g2<-values[rows2]
    
    # Step 3:
    # Compute the sample means for the two groups from 
    # step 2
    g1_m<-sum(g1)/count(g1)
    g2_m<-sum(g2)/count(g2)
    # Step 4: 
    # Compute the difference in sample means, store the
    # value in the vector from step 1
    d<- g1_m - g2_m
    p[i]<-d
  }
  
  # Step 5:
  # Return new updated vector, created in step 1
  return(p)
}


#distribution<-perm_mean(1000,cdc$trying,10000)

dist<-as.tibble(distribution)
ggplot(data=dist)+geom_histogram(aes(x=dist$value),binwidth = 0.001)+geom_vline(xintercept = d$n,color='red')+labs(title="Significance of Difference in Percentage",x="Difference (%)")
view(dist)
