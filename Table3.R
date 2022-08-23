## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 3
## 
library(tidyverse)
library(stargazer)
library(sandwich)

load("isp_daily.RData")
## Add month, year, day controls 
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Rename variables
colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"

#### Define bandwidth ####
## Narrow bandwidth
isp30 <- subset(isp_group_totals, abs(margin) <= 30)

## Calonico, Cattaneo, and Titiunik robust bandwidth
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
## Homicides 
bw_hom1 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_hom1 <- bw_hom1$bws[1]
bw_hom2 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_hom2 <- bw_hom2$bws[1]
bw_hom3 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_hom3 <- bw_hom3$bws[1]
## Robberies 
bw_rob1 <- rdbwselect(y = isp_group_totals$rob, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_rob1 <- bw_rob1$bws[1]
bw_rob2 <- rdbwselect(y = isp_group_totals$rob, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_rob2 <- bw_rob2$bws[1]
bw_rob3 <- rdbwselect(y = isp_group_totals$rob, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_rob3 <- bw_rob3$bws[1]
## Theft  
bw_furto1 <- rdbwselect(y = isp_group_totals$furto, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_furto1 <- bw_furto1$bws[1]
bw_furto2 <- rdbwselect(y = isp_group_totals$furto, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_furto2 <- bw_furto2$bws[1]
bw_furto3 <- rdbwselect(y = isp_group_totals$furto, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_furto3 <- bw_furto3$bws[1]
## Extortion  
bw_ext1 <- rdbwselect(y = isp_group_totals$ext, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_ext1 <- bw_ext1$bws[1]
bw_ext2 <- rdbwselect(y = isp_group_totals$ext, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_ext2 <- bw_ext2$bws[1]
bw_ext3 <- rdbwselect(y = isp_group_totals$ext, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_ext3 <- bw_ext3$bws[1]

## Subset data frames by bandwidth
hom1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_hom1)) 
hom2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_hom2)) 
hom3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_hom3)) 
rob1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_rob1)) 
rob2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_rob2)) 
rob3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_rob3)) 
furto1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_furto1)) 
furto2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_furto2)) 
furto3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_furto3)) 
ext1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_ext1)) 
ext2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_ext2)) 
ext3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_ext3)) 


#### Run Regressions ####
## HOMICIDES
# Linear
h1 <- lm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
hreg1 <- coeftest(h1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
h2 <- lm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), hom1)
hreg2 <- coeftest(h2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Quadratic
h3 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
hreg3 <- coeftest(h3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
h4 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), hom2)
hreg4 <- coeftest(h4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Cubic
h5 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
hreg5 <- coeftest(h5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
h6 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), hom3)
hreg6 <- coeftest(h6, vcov=vcovCL, type = "HC1", cluster = ~cisp)

.063/mean(isp30$hom[which(isp30$margin<0)]) 
.094/mean(isp30$hom[which(isp30$margin<0)]) 
.056/mean(isp30$hom[which(isp30$margin<0)]) 

.061/mean(hom1$hom[which(hom1$margin<0)]) 
.091/mean(hom2$hom[which(hom2$margin<0)]) 
.086/mean(hom3$hom[which(hom3$margin<0)]) 

## ROBBERIES
# Linear
r1 <- lm(rob ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
rreg1 <- coeftest(r1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
r2 <- lm(rob ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), rob1)
rreg2 <- coeftest(r2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Quadratic
r3 <- lm(rob ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
rreg3 <- coeftest(r3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
r4 <- lm(rob ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), rob2)
rreg4 <- coeftest(r4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Cubic
r5 <- lm(rob ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
rreg5 <- coeftest(r5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
r6 <- lm(rob ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), rob3)
rreg6 <- coeftest(r6, vcov=vcovCL, type = "HC1", cluster = ~cisp)

## THEFT
# Linear
f1 <- lm(furto ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
freg1 <- coeftest(f1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
f2 <- lm(furto ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), furto1)
freg2 <- coeftest(f2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Quadratic
f3 <- lm(furto ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
freg3 <- coeftest(f3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
f4 <- lm(furto ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), furto2)
freg4 <- coeftest(f4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Cubic
f5 <- lm(furto ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
freg5 <- coeftest(f5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
f6 <- lm(furto ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), furto3)
freg6 <- coeftest(f6, vcov=vcovCL, type = "HC1", cluster = ~cisp)

## EXTORTION
# Linear
e1 <- lm(ext ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
ereg1 <- coeftest(e1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
e2 <- lm(ext ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), ext1)
ereg2 <- coeftest(e2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Quadratic
e3 <- lm(ext ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
ereg3 <- coeftest(e3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
e4 <- lm(ext ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), ext2)
ereg4 <- coeftest(e4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
# Cubic
e5 <- lm(ext ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
ereg5 <- coeftest(e5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
e6 <- lm(ext ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), ext3)
ereg6 <- coeftest(e6, vcov=vcovCL, type = "HC1", cluster = ~cisp)

stargazer(hreg1,hreg2,hreg3,hreg4,hreg5,hreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_hom1), 30, round(cct_bw_hom2), 30, round(cct_bw_hom3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))
stargazer(rreg1,rreg2,rreg3,rreg4,rreg5,rreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_rob1), 30, round(cct_bw_rob2), 30, round(cct_bw_rob3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(rob1$margin), length(isp30$margin), 
                             length(rob2$margin), length(isp30$margin), length(rob3$margin))))
stargazer(freg1,freg2,freg3,freg4,freg5,freg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_furto1), 30, round(cct_bw_furto2), 30, round(cct_bw_furto3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(furto1$margin), length(isp30$margin), 
                             length(furto2$margin), length(isp30$margin), length(furto3$margin))))
stargazer(ereg1,ereg2,ereg3,ereg4,ereg5,ereg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_ext1), 30, round(cct_bw_ext2), 30, round(cct_bw_ext3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(ext1$margin), length(isp30$margin), 
                             length(ext2$margin), length(isp30$margin), length(ext3$margin))))

