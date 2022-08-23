## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 5
## 
library(tidyverse)
library(lubridate)
library(stargazer)
library(plm)
library(lmtest)
library(multiwayvcov)

load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))
fogo_dp$week <- week(as.Date(fogo_dp$data_ocorrencia))

isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
isp19_group_totals$weekday <- weekdays(as.Date((isp19_group_totals$data_fato)))
isp19_group_totals$month <- as.factor(format(as.Date(isp19_group_totals$data_fato), "%m"))

fogo_pe$weekday <- weekdays(as.Date(fogo_pe$data_ocorrencia))
fogo_pe$month <- as.factor(format(as.Date(fogo_pe$data_ocorrencia), "%m"))
fogo_pe$year <- as.factor(format(as.Date(fogo_pe$data_ocorrencia), "%Y"))
fogo_pe$week <- week(as.Date(fogo_pe$data_ocorrencia))

## Add share police variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0

colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"
colnames(isp19_group_totals)[26] <- "polkilling"
colnames(isp19_group_totals)[16] <- "hom"

isp_group_totals <- isp_group_totals[,c("cisp", "data_fato", "aisp", "polkilling", "hom", "margin", "FederalBan", "rob", "furto", "ext", 
                                        "weekday", "month")]
isp19_group_totals <- isp19_group_totals[,c("cisp", "data_fato", "aisp", "polkilling", "hom", "margin", "FederalBan", "rob", "furto", "ext", 
                                        "weekday", "month")]
isp <- rbind(isp_group_totals, isp19_group_totals)
isp$week <- week(isp$data_fato)

fogo_pe$sharepolice <- fogo_pe$n_police/fogo_pe$n
fogo_pe$sharepolice[which(is.na(fogo_pe$sharepolice)==T)] <- 0

########## DID ##########
fogo90 <- subset(fogo_dp, week <= 33 & week >= 8)
isp90 <- subset(isp, week <= 33 & week >= 8)

### DID using fogo data 
s1 <- lm(sharepolice ~ FederalBan +factor(dp) + factor(week) + factor(weekday), fogo90)
sreg1 <- coeftest(s1, vcov=vcovHC(s1, type = "HC0", cluster = "dp"))
d1 <- lm(n_deaths ~ FederalBan +factor(dp) + factor(week) + factor(weekday), fogo90)
dreg1 <- coeftest(d1, vcov=vcovHC(d1, type = "HC0", cluster = "dp"))
i1 <- lm(n_injuries ~ FederalBan +factor(dp) + factor(week) + factor(weekday), fogo90)
ireg1 <- coeftest(i1, vcov=vcovHC(i1, type = "HC0", cluster = "dp"))
sh1 <- lm(n ~ FederalBan +factor(dp) + factor(week) + factor(weekday), fogo90)
shreg1 <- coeftest(sh1, vcov=vcovHC(sh1, type = "HC0", cluster = "dp"))

### DID using isp data 
## Homicides
h1 <- lm(hom ~ FederalBan +factor(cisp) + factor(week) + factor(weekday), isp90)
hreg1 <- coeftest(h1, vcov=vcovHC(h1, type = "HC0", cluster = "cisp"))
## Police Killings
p1 <- lm(polkilling ~ FederalBan +factor(cisp) + factor(week) + factor(weekday), isp90)
preg1 <- coeftest(h1, vcov=vcovHC(h1, type = "HC0", cluster = "cisp"))


stargazer(shreg1, dreg1, ireg1, sreg1, preg1, hreg1,
          keep = c("FederalBan"), column.labels = c("Shootings", "Deaths", "Injuries", "Share Police", "Police Killings", "Homicides"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(1, 1, 1, 1, 1, 1),  
          add.lines = list(c("Observations", length(isp90$cisp),length(isp90$cisp),length(isp90$cisp),length(isp90$cisp),length(isp90$cisp),length(isp90$cisp) )))


