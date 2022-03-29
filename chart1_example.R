source("https://raw.githubusercontent.com/info-201b-wi22/a3-incarceration-XiaoyuZhou000/main/summary.R?token=GHSAT0AAAAAABR6YVNLLT7W5XFODKWJZCDAYRCZ7IA")
library(ggplot2)


# This chart is showing the trend of the US annual average incarceration rate of the White people and the Black people from 1970-2018

chart1 <- ggplot(data = data_year_clean) + geom_point(mapping = aes (x=year, y=black_rate_year*100, color="black"))+ geom_line(mapping = aes (x=year, y=black_rate_year*100, group = 1, color="black")) + geom_point(mapping = aes (x=year, y=white_rate_year*100, color="white"))+ geom_line(mapping = aes (x=year, y=white_rate_year*100, group = 1, color="white")) +labs(title="Annual Average Incarceration Rate of Race of White and Black", y="Annual Average Incarceration Rate(%)", x="Year")
