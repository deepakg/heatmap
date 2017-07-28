library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggthemes)

t <- read.csv('data/etmgeg_240.txt', skip=47)
str(t)

t %>% filter(YYYYMMDD >= 20160101 & YYYYMMDD <= 20161231) %>% select(YYYYMMDD, TN, TX) -> t.2016

t.2016 %>% mutate(
    date = as.Date(as.character(YYYYMMDD), format="%Y%m%d"),
    year = year(date),
    month = month(date, label=T, abbr=T),
    week = strftime(date,"%W"),
    wday = substring(wday(date, label=T, abbr=T),1,2),
    day = day(date),
    TN = TN / 10,
    TX = TX / 10
) -> t.2016
head(t.2016)

ggplot(data = t.2016, aes(x = day,y = month)) +
geom_tile(aes(fill = TX)) +
scale_x_continuous(breaks=c(1:31), expand=c(0,0)) + coord_equal(ratio = 1) +
scale_fill_viridis(option="magma") + theme_tufte(base_family="Helvetica")
