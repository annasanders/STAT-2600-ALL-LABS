milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)

bday_milk <- milk %>%
  filter(year == 1999)

ggplot(data =bday_milk, aes(x = milk_million)) +
  geom_density() + 
  ggtitle('Histogram of milk produced in 1999 by State')

bday_calc <- bday_milk %>%
  summarise(mmean = mean (milk_produced/1000000),
            mmedian = median (milk_produced/1000000))

bday_state <- milk %>%
  filter(year == 1999) %>%
  arrange(desc(milk_produced))

milksub <- milk %>%
  filter(state %in% c('Colorado','California','Wisconsin','Georgia', 'Alaska')) %>%
  select(state, year, milk_million)

ggplot(data = milksub, aes(x = year, y = milk_million, color = state)) +
  geom_point() +
  ggtitle('Pounds of milk over time by state') +
  xlab('Year') +
  ylab('Milk Produced (Million lb)')

largest_state2 <- milk %>%
  group_by(year) %>%
  summarize(sum_milk = sum(milk_million))%>%
  arrange(desc(sum_milk))

milk_now <- milk %>%
  filter(year == 2017) %>%
  arrange (desc(milk_produced)) 

geo_milk <- milk %>%
  filter(state == 'Georgia') %>%
  summarise (gmean = mean(milk_produced/1000000),
             gmedian = median (milk_produced/1000000))

als_milk <- milk %>%
  filter(state == 'Alaska') %>%
  summarise (amean = mean(milk_produced/1000000),
             amedian = median (milk_produced/1000000))
