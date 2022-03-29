source("https://raw.githubusercontent.com/info-201b-wi22/a3-incarceration-XiaoyuZhou000/main/summary.R?token=GHSAT0AAAAAABR6YVNLLT7W5XFODKWJZCDAYRCZ7IA")
library(ggplot2)

# The chart compare the average incarceration rate of White people and Black people from 1970-2018

df <- data.frame(Race=c("Black", "White"), Rate=c(black_rate$rate, white_rate$rate))

chart2 <- ggplot(data=df)  + geom_col(mapping = aes(x=Race, y=Rate*100), fill="grey") + labs(title="Average Incarceration of White vs. Black", y="Rate(%)")
