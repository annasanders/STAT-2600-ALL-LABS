library(tidyverse)
library(gapminder)
data<-gapminder

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
    g1_m<-mean(g1)
    g2_m<-mean(g2)
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
f_data<-perm_mean(1000,data$lifeExp,500)
ggplot(data=as.tibble(f_data))+geom_histogram(aes(f_data))
