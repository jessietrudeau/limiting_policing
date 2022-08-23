## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression discontinuity plots in Figure 4
## 
library(tidyverse)
library(gridExtra)
load("fogo_rd.RData")
load("isp_daily.RData")

## Graphically show 4.5 months on either side of the cutpoint
ndays <- 139
fogo_city$sharepol <- fogo_city$n_police/fogo_city$n
fogo_city <- subset(fogo_city, abs(margin) <= ndays)
## Aggregate ISP totals at the metro area-day level instead of precinct level
ispdaily <- isp_group_totals %>%
  group_by(data_fato, margin, FederalBan) %>%
  summarise(hom = sum(`Homicídio doloso`),
            polkilling = sum(`Morte por intervenção de agente do Estado`),
            robbery = sum(rob),
            violent = sum(cvli),
            theft = sum(furto), 
            ext = sum(ext))

#### RD plots
homplot<- ggplot(ispdaily, aes(x = as.Date(data_fato), y = hom)) + 
  geom_point(size = 0.5) + stat_smooth(aes(col=factor(FederalBan)), method = "loess") + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") + 
  theme_bw() + ggtitle("A) Daily Homicides") + theme(legend.position = "Bottom") +xlab("Date") +ylab("Homicides")
rplot<- ggplot(ispdaily, aes(x = as.Date(data_fato), y = robbery)) + 
  geom_point(size = 0.5) + stat_smooth(aes(col=factor(FederalBan)), method = "loess", span = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") + 
  theme_bw() + ggtitle("B) Daily Robberies") + theme(legend.position = "Bottom")+xlab("Date") + ylab("Robberies")
tplot<- ggplot(ispdaily, aes(x = as.Date(data_fato), y = theft)) + 
  geom_point(size = 0.5) + stat_smooth(aes(col=factor(FederalBan)), method = "loess", span = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") + 
  theme_bw() + ggtitle("C) Daily Theft") + theme(legend.position = "Bottom")+xlab("Date") +ylab("Thefts")
eplot<- ggplot(ispdaily, aes(x = as.Date(data_fato), y = ext)) + 
  geom_point(size = 0.5) + stat_smooth(aes(col=factor(FederalBan)), method = "loess", span = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") + 
  theme_bw() + ggtitle("D) Daily Extortion") + theme(legend.position = "Bottom")+xlab("Date") +ylab("Extortion Counts")


pdf("isp_rd.pdf")
grid.arrange(homplot, rplot, tplot, eplot, nrow= 2)
dev.off()

