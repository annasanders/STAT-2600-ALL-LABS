milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)

avg_milk <- milk %>%
  mutate(sum(milk_million))