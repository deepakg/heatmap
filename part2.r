library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggthemes)

t <- read.csv('data/etmgeg_240.txt', skip=47)

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

t.2016$wday <- factor(t.2016$wday, levels=c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"))
t.2016$week <- factor(t.2016$week, levels=rev(sort(unique(t.2016$week))))

ggplot(data = t.2016, aes(x = wday, y = week)) +
geom_tile(aes(fill = TX)) +
coord_equal(ratio = 1) +
scale_fill_viridis(option = "magma") +
theme_tufte(base_family = "Helvetica") +
facet_wrap(~month, nrow = 3, scales="free") +
geom_text(aes(label = day), color = "grey", size = 3) +

# hide y-axis ticks and labels
theme(axis.ticks.y = element_blank()) +
theme(axis.text.y = element_blank()) +

# hide main x and y-axis titles
theme(axis.title.x = element_blank()) +
theme(axis.title.y = element_blank()) +

# move x-axis labels (week names) to top, hide ticks
scale_x_discrete(position = "top") +
theme(axis.ticks.x = element_blank()) +

# move panel title (month names) outside (above week names)
theme(strip.placement = "outside") +
theme(strip.text.x = element_text(size = "14", hjust = 0)) +

# center-aligned plot title
ggtitle("2016: Heatmap of maximum temperatures in Amsterdam") +
theme(plot.title = element_text(size = "16", hjust = 0.5))
