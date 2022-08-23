## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression discontinuity plots in Figure 2
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

####### RD plots 
polkillingplot<- ggplot(ispdaily, aes(x = as.Date(data_fato), y = polkilling)) + 
  geom_point(size = 0.5) + stat_smooth(aes(col=factor(FederalBan)), method = "loess") + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") + 
  theme_bw() + ggtitle("A) Daily Police Killings") + theme(legend.position = "Bottom") + xlab("Date") +ylab("Police Killings")
sharepoliceplot <- ggplot(fogo_city, aes(x = as.Date(data_ocorrencia), y = sharepol)) + geom_point(size = 0.5) + stat_smooth(aes(col = factor(FederalBan)), method = "loess", span = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") +theme_bw() + ggtitle("B) Share of Shootings with Police Presence") + theme(legend.position = "Bottom") +
  xlab("Date") + ylab("Share of Shootings")
policeplot <- ggplot(fogo_city, aes(x = as.Date(data_ocorrencia), y = n_police)) + geom_point(size = 0.5) + stat_smooth(aes(col = factor(FederalBan)), method = "loess") + ylim(0,30) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") +theme_bw() + ggtitle("C) Police at Shootings (numerator)") + theme(legend.position = "None") +
  xlab("Date") + ylab("Police Shootings")
shootingsplot <- ggplot(fogo_city, aes(x = as.Date(data_ocorrencia), y = n)) + geom_point(size = 0.5) + stat_smooth(aes(col = factor(FederalBan)), method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype = 'dashed', col = "black") +theme_bw() + ggtitle("D) Daily Shootings (denominator)") +
  theme(legend.position = "None") + xlab("Date") + ylab("Shootings")

pdf("sharepolice_rdplot.pdf",width = 10, height = 10)
grid.arrange(polkillingplot, sharepoliceplot, policeplot, shootingsplot)
dev.off()

