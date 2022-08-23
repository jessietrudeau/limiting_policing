## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Plots the isolation index in the city of Rio de Janeiro shown in Figre 5
## 
library(tidyverse)
library(lubridate)
library(haven)
load("isolation_index_daily.RData")

#Data is just for Rio de Janeiro, clean and reshape
data$margin <- as.numeric(as.Date(data$date)) - as.numeric(as.Date("2020-05-19"))
data$week <- week(data$date)

isolation <- data %>%
  group_by(date, week, margin) %>%
  summarise(iso = mean(isolation_muni),
            iso_a = mean(isolation_arranjo))

# 90 day window before and after the ban on raids
isolation <- subset(isolation, abs(margin) <= 90)

isolation <- isolation %>%
  group_by(week) %>%
  mutate(weeklyavg = mean(iso),
         weeklyavg_a = mean(iso_a))

isolation$ban <- 0
isolation$ban[which(isolation$margin >= 0)] <- 1


pdf("inloco.pdf")
ggplot(isolation, aes(x = margin, y = iso_a)) + geom_line() + geom_vline(xintercept = 0, linetype = "dashed", col = "red") +
  xlab("Days") +ylab("Isolation Index") +theme_bw() + geom_smooth(aes(x = margin, y = weeklyavg_a, col = factor(ban))) + 
  theme(legend.position = "none")
dev.off()
