## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 4
## 
library(tidyverse)
library(stargazer)
library(tidyverse)
library(rdrobust)
library(rdd)
library(gridExtra)
library(clubSandwich)
load("isp_daily.RData")

#### Cleaning data ####
## Which precincts have the highest pre-treatment violence in 2019
homs19 <- isp19_group_totals %>%
  group_by(cisp) %>%
  summarise(cvli = sum(cvli))
mostviolent <- homs19$cisp[which(homs19$cvli >= quantile(homs19$cvli)[4])] # top 25%
## Add month, year, day controls 
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Rename DV
colnames(isp_group_totals)[17] <- "hom"


#### Set bandwidth ####
isp_high <- subset(isp_group_totals, cisp %in% mostviolent)
## narrow 30 day bandwidth
isp30 <- subset(isp_high, abs(margin) <= 30)
#### Optimal bandwidth 
Z = data.frame(cbind(factor(isp_high$month),factor(isp_high$weekday), factor(isp_high$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
## Homicides 
bw_hom1high <- rdbwselect(y = isp_high$hom, x = isp_high$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
hom1cct <- bw_hom1high$bws[1]
bw_hom2high <- rdbwselect(y = isp_high$hom, x = isp_high$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
hom2cct <- bw_hom2high$bws[1]
bw_hom3high <- rdbwselect(y = isp_high$hom, x = isp_high$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
hom3cct <- bw_hom3high$bws[1]
hom1 <- subset(isp_high, abs(margin) <= round(hom1cct)) 
hom2 <- subset(isp_high, abs(margin) <= round(hom2cct)) 
hom3 <- subset(isp_high, abs(margin) <= round(hom3cct)) 


#### REGRESSIONS - HOMICIDES ####
## 30 day narrow bandwidth
h1 <- lm(hom ~ margin+FederalBan + margin*FederalBan  +factor(cisp) + factor(weekday), isp30)
h2 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +factor(cisp) + factor(weekday), isp30)
h3 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +  I(margin^3) + I(margin^3)*FederalBan +factor(cisp) + factor(weekday), isp30)
hreg1 <- coeftest(h1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
hreg2 <- coeftest(h2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
hreg3 <- coeftest(h3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Optimal bandwidth
h1o <- lm(hom ~ margin+FederalBan + margin*FederalBan  +factor(cisp) + factor(weekday), hom1)
h2o <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +factor(cisp) + factor(weekday), hom2)
h3o <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan + I(margin^3) + I(margin^3)*FederalBan +factor(cisp) + factor(weekday), hom3)
horeg1 <- coeftest(h1o, vcov=vcovCL, type = "HC1", cluster = ~cisp)
horeg2 <- coeftest(h2o, vcov=vcovCL, type = "HC1", cluster = ~cisp)
horeg3 <- coeftest(h3o, vcov=vcovCL, type = "HC1", cluster = ~cisp)


.130/mean(isp30$hom[which(isp30$margin<0)]) 
.251/mean(isp30$hom[which(isp30$margin<0)]) 
.205/mean(isp30$hom[which(isp30$margin<0)]) 

.094/mean(hom1$hom[which(hom1$margin<0)]) 
.193/mean(hom2$hom[which(hom2$margin<0)]) 
.241/mean(hom3$hom[which(hom3$margin<0)]) 
## Most conservative estimate is also the stat signif one -- 65% decrease in homicides 
mean(0.7336634, 1.416535, 1.156931, 0.5371429, 1.095338, 1.403613)
#[1] 0.7336634 Mean decrease is 73%


stargazer(hreg1,horeg1,hreg2,horeg2,hreg3,horeg3,
          keep = c("\\bFederalBan\\b"), 
          title = "Homicides (Top Quartile)",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(hom1cct), 30, round(hom2cct), 30, round(hom3cct)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))

