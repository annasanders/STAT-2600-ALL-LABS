---
  title: "LAB 5"
author: "DATA ME PLS"
date: "February 13, 2019"
output: html_document
---
  
``{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)

milk <- read.csv('/Users/Chris/Desktop/GitKraken/lab5/STAT-2600-ALL-LABS/Lab 5/state_milk_production.csv')

#Chris's part 

milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)

milk1998 <- milk %>%
  filter(year == 1998)

head(milk1998)

ggplot(data = milk1998, aes(x = milk_million)) +
  geom_density() + 
  ggtitle('Distribution estimate of milk produced in 1998 by state')

median_year <- milk1998 %>%
  summarise(Med = median(milk_million)) %>%
  as_tibble()
median_year

avg_milk <- milk1998 %>%
  summarise(Avg = mean(milk_million)) %>%
  as_tibble()
avg_milk

largest_state <- milk1998 %>%
  group_by(state, milk_million) %>%
  summarize(count = n()) %>%
  arrange(desc(milk_million))
largest_state

#The tibble shows that California was the state that produced the most, and Alaska was the state that produced the least.

#group part
largest_state2 <- milk %>%
  group_by(year) %>%
  summarize(sum_milk = sum(milk_million)) %>%
  arrange(desc(sum_milk))
largest_state2

#This shows 2017 was the year where the most milk was produced, 1975 was the year where the least milk was produced.

```
MAKE SURE TO RUN THE CODE ABOVE SO YOU CAN GET THE DATA!!!!
  
  
  # MILK IS GOOD EVEN THOUGH WE BECOME LACTOSE INTOLERANT OVER TIME
  The year when the most milk was produced in the United States.
The year when the least milk was produced in the United States.
In 2017, report the 5 states that produced the most milk. (Hint: Use arrange)
In 2017, report the 5 states that produced the least milk. (Hint: Use arrange)

## Colorado

## Georgia

## California

## Rhode Island

## Alaska 

## 1997 (James)

## 1998 (Anyone??)

## 1999 (Anna)



