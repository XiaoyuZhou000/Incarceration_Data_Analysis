data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)
library(tidyr)

# Chart1

selected_data0 <- na.omit(data %>% select("year", "black_jail_pop", "black_pop_15to64", "white_jail_pop", "white_pop_15to64"))
data_year <- selected_data0 %>% mutate(black_rate = black_jail_pop/black_pop_15to64) %>% mutate(white_rate = white_jail_pop/white_pop_15to64)
data_year_clean <- na.omit(do.call(data.frame, lapply(data_year, function(x) replace(x, is.infinite(x), NA))))
data_year_clean <- data_year_clean %>% group_by(year) %>% summarize(white_rate_year = mean(white_rate), black_rate_year=mean(black_rate))



# Chart2

# (1) incarceration_rate_black_data
selected_data1 <- data %>% select("year", "state", "county_name", "black_jail_pop", "black_pop_15to64")
data_clean1 <- na.omit(selected_data1)
data_clean1 <- data_clean1 %>% mutate(incarceration_rate_black = black_jail_pop/black_pop_15to64 )
data_clean1 <- do.call(data.frame, lapply(data_clean1, function(x) replace(x, is.infinite(x), NA)))
data_clean1 <- na.omit(data_clean1)
state_rate_black_data <- data_clean1 %>% group_by(state) %>% summarize(incarceration_rate_black_state = mean(incarceration_rate_black))

# (2) incarceration_rate_white_data
selected_data2 <- data %>% select("year", "state", "county_name", "white_jail_pop", "white_pop_15to64")
data_clean2 <- na.omit(selected_data2)
data_clean2 <- data_clean2 %>% mutate(incarceration_rate_white = white_jail_pop/white_pop_15to64 )
data_clean2 <- do.call(data.frame, lapply(data_clean2, function(x) replace(x, is.infinite(x), NA)))
data_clean2 <- na.omit(data_clean2)
state_rate_white_data <- data_clean2 %>% group_by(state) %>% summarize(incarceration_rate_white_state = mean(incarceration_rate_white))


black_rate <- state_rate_black_data %>% summarize(rate=mean(incarceration_rate_black_state))
white_rate <- state_rate_white_data %>% summarize(rate=mean(incarceration_rate_white_state))



# Map
selected_data_map <- data %>% select("year", "state", "county_name", "white_jail_pop", "white_pop_15to64", "black_jail_pop", "black_pop_15to64")
data_clean_map <- na.omit(selected_data_map)
data_clean_map <- data_clean_map %>% mutate(incarceration_rate_black = black_jail_pop/black_pop_15to64 ) %>% mutate(incarceration_rate_white = white_jail_pop/white_pop_15to64 )
data_clean_map <- do.call(data.frame, lapply(data_clean_map, function(x) replace(x, is.infinite(x), NA)))
data_clean_map <- na.omit(data_clean_map)
data_difference <- data_clean_map %>% mutate(rate_diff = incarceration_rate_black-incarceration_rate_white)
data_difference <- data_difference %>% group_by(state) %>% summarize(rate_diff_mean = mean(rate_diff))


# This chart is showing the trend of the US annual average incarceration rate of the White people and the Black people from 1970-2018

chart1 <- ggplot(data = data_year_clean) + geom_point(mapping = aes (x=year, y=black_rate_year*100, color="black"))+ geom_line(mapping = aes (x=year, y=black_rate_year*100, group = 1, color="black")) + geom_point(mapping = aes (x=year, y=white_rate_year*100, color="white"))+ geom_line(mapping = aes (x=year, y=white_rate_year*100, group = 1, color="white")) +labs(title="Annual Average Incarceration Rate of Race of White and Black", y="Annual Average Incarceration Rate(%)", x="Year")




# The chart compare the average incarceration rate of White people and Black people from 1970-2018

df <- data.frame(Race=c("Black", "White"), Rate=c(black_rate$rate, white_rate$rate))

chart2 <- ggplot(data=df)  + geom_col(mapping = aes(x=Race, y=Rate*100), fill="grey") + labs(title="Average Incarceration of White vs. Black", y="Rate(%)")




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







# function to calculate the data of the incarceration rate for each race
# ***Fail to run***
IncarcerationRateForRaces <- function(race) {
  str_jail_pop <- paste0(race, "_jail_pop")
  str_pop_15to64 <- paste0(race, "_pop_15to64")
  selected_data <- data %>% select("year", "state", "county_name", str_jail_pop, "black_pop_15to64")
  data_clean <- na.omit(selected_data1)
  incarceration_rate_str <- paste0("incarceration_rate_", race)
  data_clean <- data_clean %>% mutate(incarceration_rate_str = as.character(str_jail_pop)/as.character(str_pop_15to64) )
  data_clean <- do.call(data.frame, lapply(data_clean, function(x) replace(x, is.infinite(x), NA)))
  data_clean <- na.omit(data_clean)
  state_rate_data <- data_clean %>% group_by(state) %>% summarize(incarceration_rate_state = mean(incarceration_rate_str))
  return (state_rate_data)
}
# ***Fail to run***