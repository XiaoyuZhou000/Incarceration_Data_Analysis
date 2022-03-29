source("https://raw.githubusercontent.com/info-201b-wi22/a3-incarceration-XiaoyuZhou000/main/summary.R?token=GHSAT0AAAAAABR6YVNLLT7W5XFODKWJZCDAYRCZ7IA")

library(knitr)
library("mapproj")
library("maps")
library(ggplot2)
library(scales)
library(dplyr)


# This map shows that the average difference of the incarceration rate of White and Black in each state.

state_shape <- map_data("state")
state_abbrevs <- data.frame(state.abb, state.name)
state_diff <- left_join(data_difference, state_abbrevs, by=c('state' = 'state.abb'))
state_diff <- state_diff %>% mutate(region = tolower(state.name))
state_shape <- left_join(state_shape, state_diff)

map <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = rate_diff_mean)) + scale_fill_continuous(low = 'grey', high ='red', labels = scales::label_number_si()) +
  coord_map() +
  labs(title = 'Average Difference of Incarceration Rate of White vs. Black', fill = "avg. diff. of inc. rate")
