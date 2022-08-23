## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces Tables B1-B18 in the supplementary appendix
## Note: This file is meant to be run from top to bottom, i.e., some of the edits or data cleaning procedures at the top are carried through to later tables 
## 
library(tidyverse)
library(rdrobust)
library(stargazer)
library(pscl)

load("fogo_rd.RData")
load("isp_daily.RData")
#### CLEANING DATA ####
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))

isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))

## Add share police variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0

colnames(isp_group_totals)[32] <- "polkilling"

## Changing DVs so they can be logged
fogo_dp$logshare <- log(fogo_dp$sharepolice + 0.000000000001)
isp_group_totals$logpolkilling <- log(isp_group_totals$polkilling + 0.000000000001)

fogo30 <- subset(fogo_dp, abs(margin) <= 30)
isp30 <- subset(isp_group_totals, abs(margin) <= 30)

#### TABLE B1 ####
## Log Share Shootings
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$logshare, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$logshare, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$logshare, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw3 <- bw_fc3$bws[1]
fogo_cct_lin <- subset(fogo_dp, abs(margin) <= round(cct_bw1)) #Shootings, Linear spec 
fogo_cct_qua <- subset(fogo_dp, abs(margin) <= round(cct_bw1)) #Shootings, Quad spec 
fogo_cct_cub <- subset(fogo_dp, abs(margin) <= round(cct_bw1)) #Shootings, Cubic spec 
n1 <- lm(logshare ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n2 <- lm(logshare ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_lin)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(logshare ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(logshare ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_qua)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(logshare ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(logshare ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_cub)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Log Police Killings 
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_isp1 <- rdbwselect(y = isp_group_totals$logpolkilling, x = isp_group_totals$margin, p = 1, q = 2, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1 <- bw_isp1$bws[1]
bw_isp2 <- rdbwselect(y = isp_group_totals$logpolkilling, x = isp_group_totals$margin, p = 2, q = 3, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2 <- bw_isp2$bws[1]
bw_isp3 <- rdbwselect(y = isp_group_totals$logpolkilling, x = isp_group_totals$margin, p = 3, q = 4, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3 <- bw_isp3$bws[1]
isp_cct1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1)) #linear
isp_cct2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2)) #quadratic
isp_cct3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3)) #cubic
l1 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1)
lreg1 <- coeftest(l1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2 <- coeftest(l2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2)
lreg3 <- coeftest(l3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4 <- coeftest(l4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6 <- lm(logpolkilling ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3)
lreg5 <- coeftest(l5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6 <- coeftest(l6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table Output 
## Linear 
stargazer(lreg1, lreg2, nreg1, nreg2,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Linear",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1), 30, round(cct_bw1)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1$margin), 
                             length(fogo30$margin), length(fogo_cct_lin$margin))))
## Quadratic
stargazer(lreg3, lreg4, nreg3, nreg4,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Quad",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp2), 30, round(cct_bw2)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct2$margin), 
                             length(fogo30$margin), length(fogo_cct_qua$margin))))
## Cubic
stargazer(lreg5, lreg6, nreg5, nreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Cubic",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp3), 30, round(cct_bw3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct3$margin), 
                             length(fogo30$margin), length(fogo_cct_cub$margin))))

#### TABLE B2 ####
## narrow isp window
isp1 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp30)
isp2 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
isp3 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
ireg1 <- coeftest(isp1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2 <- coeftest(isp2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3 <- coeftest(isp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## CCT optimal bandwidth window
isp1.1 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp_cct1)
isp2.1 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct2)
isp3.1 <- glm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct3)
ireg1.1 <- coeftest(isp1.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2.1 <- coeftest(isp2.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3.1 <- coeftest(isp3.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table output
stargazer(ireg1, ireg1.1, ireg2, ireg2.1, ireg3, ireg3.1,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Poisson",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1), 30, round(cct_bw_isp2), 30, round(cct_bw_isp3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1$margin), length(isp30$margin), length(isp_cct2$margin),
                             length(isp30$margin), length(isp_cct3$margin))))

#### TABLE B3 ####
## Calculate bandwidths - Log Homicides
bw_isp1 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 1, q = 2, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1 <- bw_isp1$bws[1]
bw_isp2 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 2, q = 3, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2 <- bw_isp2$bws[1]
bw_isp3 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 3, q = 4, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3 <- bw_isp3$bws[1]
isp_cct1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1)) #linear
isp_cct2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2)) #quadratic
isp_cct3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3)) #cubic
## Models
l1 <- lm(loghom ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2 <- lm(loghom ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1)
lreg1 <- coeftest(l1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2 <- coeftest(l2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2)
lreg3 <- coeftest(l3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4 <- coeftest(l4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3)
lreg5 <- coeftest(l5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6 <- coeftest(l6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Calculate bandwidths - log robberies
bw_isp1.1 <- rdbwselect(y = isp_group_totals$logrob, x = isp_group_totals$margin, p = 1, q = 2, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1.1 <- bw_isp1.1$bws[1]
bw_isp2.1 <- rdbwselect(y = isp_group_totals$logrob, x = isp_group_totals$margin, p = 2, q = 3, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2.1 <- bw_isp2.1$bws[1]
bw_isp3.1 <- rdbwselect(y = isp_group_totals$logrob, x = isp_group_totals$margin, p = 3, q = 4, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3.1 <- bw_isp3.1$bws[1]
isp_cct1.1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1.1)) #linear
isp_cct2.1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2.1)) #quadratic
isp_cct3.1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3.1)) #cubic
## Models
l1.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1.1)
lreg1.1 <- coeftest(l1.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2.1 <- coeftest(l2.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2.1)
lreg3.1 <- coeftest(l3.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4.1 <- coeftest(l4.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6.1 <- lm(logrob ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3.1)
lreg5.1 <- coeftest(l5.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6.1 <- coeftest(l6.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Calculate bandwidths - log theft
bw_isp1.2 <- rdbwselect(y = isp_group_totals$logfurto, x = isp_group_totals$margin, p = 1, q = 2, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1.2 <- bw_isp1.2$bws[1]
bw_isp2.2 <- rdbwselect(y = isp_group_totals$logfurto, x = isp_group_totals$margin, p = 2, q = 3, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2.2 <- bw_isp2.2$bws[1]
bw_isp3.2 <- rdbwselect(y = isp_group_totals$logfurto, x = isp_group_totals$margin, p = 3, q = 4, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3.2 <- bw_isp3.2$bws[1]
isp_cct1.2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1.2)) #linear
isp_cct2.2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2.2)) #quadratic
isp_cct3.2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3.2)) #cubic
## Models
l1.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1.2)
lreg1.2 <- coeftest(l1.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2.2 <- coeftest(l2.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2.2)
lreg3.2 <- coeftest(l3.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4.2 <- coeftest(l4.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6.2 <- lm(logfurto ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3.2)
lreg5.2 <- coeftest(l5.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6.2 <- coeftest(l6.2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Calculate bandwidths - log extortion
bw_isp1.3 <- rdbwselect(y = isp_group_totals$logext, x = isp_group_totals$margin, p = 1, q = 2, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1.3 <- bw_isp1.3$bws[1]
bw_isp2.3 <- rdbwselect(y = isp_group_totals$logext, x = isp_group_totals$margin, p = 2, q = 3, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2.3 <- bw_isp2.3$bws[1]
bw_isp3.3 <- rdbwselect(y = isp_group_totals$logext, x = isp_group_totals$margin, p = 3, q = 4, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3.3 <- bw_isp3.3$bws[1]
isp_cct1.3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1.3)) #linear
isp_cct2.3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2.3)) #quadratic
isp_cct3.3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3.3)) #cubic
## Models
l1.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1.3)
lreg1.3 <- coeftest(l1.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2.3 <- coeftest(l2.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2.3)
lreg3.3 <- coeftest(l3.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4.3 <- coeftest(l4.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6.3 <- lm(logext ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3.3)
lreg5.3 <- coeftest(l5.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6.3 <- coeftest(l6.3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table Output 
stargazer(lreg1,lreg2,lreg3,lreg4,lreg5,lreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1), 30, round(cct_bw_isp2), 30, round(cct_bw_isp3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1$margin), length(isp30$margin), 
                             length(isp_cct2$margin), length(isp30$margin), length(isp_cct3$margin))))
stargazer(lreg1.1, lreg2.1, lreg3.1, lreg4.1, lreg5.1, lreg6.1,
          keep = c("\\bFederalBan\\b"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1.1), 30, round(cct_bw_isp2.1), 30, round(cct_bw_isp3.1)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1.1$margin), length(isp30$margin), 
                             length(isp_cct2.1$margin), length(isp30$margin), length(isp_cct3.1$margin))))
stargazer(lreg1.2, lreg2.2, lreg3.2, lreg4.2, lreg5.2, lreg6.2,
          keep = c("\\bFederalBan\\b"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1.2), 30, round(cct_bw_isp2.2), 30, round(cct_bw_isp3.2)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1.2$margin), length(isp30$margin), 
                             length(isp_cct2.2$margin), length(isp30$margin), length(isp_cct3.2$margin))))
stargazer(lreg1.3, lreg2.3, lreg3.3, lreg4.3, lreg5.3, lreg6.3,
          keep = c("\\bFederalBan\\b"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1.3), 30, round(cct_bw_isp2.3), 30, round(cct_bw_isp3.3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1.3$margin), length(isp30$margin), 
                             length(isp_cct2.3$margin), length(isp30$margin), length(isp_cct3.3$margin))))


#### TABLE B4 ####
## Calculate bandwidths - homicides
bw_isp1.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 1, q = 2, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1.4 <- bw_isp1.4$bws[1]
bw_isp2.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 2, q = 3, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2.4 <- bw_isp2.4$bws[1]
bw_isp3.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 3, q = 4, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3.4 <- bw_isp3.4$bws[1]
isp_cct1.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1.4)) #linear
isp_cct2.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2.4)) #quadratic
isp_cct3.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3.4)) #cubic
## Poisson models 
isp1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp30)
isp2 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
isp3 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
ireg1 <- coeftest(isp1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2 <- coeftest(isp2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3 <- coeftest(isp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
isp1.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp_cct1.4)
isp2.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct2.4)
isp3.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct3.4)
ireg1.1 <- coeftest(isp1.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2.1 <- coeftest(isp2.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3.1 <- coeftest(isp3.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table output
stargazer(ireg1, ireg1.1, ireg2, ireg2.1, ireg3, ireg3.1,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides: Poisson",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1.4), 30, round(cct_bw_isp2.4), 30, round(cct_bw_isp3.4)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1.4$margin), length(isp30$margin), length(isp_cct2.4$margin),
                             length(isp30$margin), length(isp_cct3.4$margin))))

#### TABLE B5 ####
## Define which precincts had the highest # of violence in 2019
homs19 <- isp19_group_totals %>%
  group_by(cisp) %>%
  summarise(cvli = sum(cvli))
mostviolent <- homs19$cisp[which(homs19$cvli >= quantile(homs19$cvli)[4])] # top 75%
## Add datetime vars and var labels
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
colnames(isp_group_totals)[17] <- "hom"
isp_group_totals <- subset(isp_group_totals, cisp %in% mostviolent)
isp_group_totals$loghom <- log(isp_group_totals$hom + 0.000000000001)
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
## Calculate optimal bandwidth
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_isp1 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 1, q = 2, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1 <- bw_isp1$bws[1]
bw_isp2 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 2, q = 3, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2 <- bw_isp2$bws[1]
bw_isp3 <- rdbwselect(y = isp_group_totals$loghom, x = isp_group_totals$margin, p = 3, q = 4, 
                      covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3 <- bw_isp3$bws[1]
isp_cct1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1)) #linear
isp_cct2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2)) #quadratic
isp_cct3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3)) #cubic
## Models
l1 <- lm(loghom ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday), isp30)
l2 <- lm(loghom ~ margin+FederalBan + margin*FederalBan  +factor(cisp)  + factor(weekday)+ factor(month), isp_cct1)
lreg1 <- coeftest(l1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg2 <- coeftest(l2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l3 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday), isp30)
l4 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct2)
lreg3 <- coeftest(l3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg4 <- coeftest(l4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
l5 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
l6 <- lm(loghom ~ margin+FederalBan + margin*FederalBan +I(margin^2) + FederalBan*I(margin^2) +  I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday)+ factor(month), isp_cct3)
lreg5 <- coeftest(l5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
lreg6 <- coeftest(l6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table output
stargazer(lreg1,lreg2,lreg3,lreg4,lreg5,lreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1), 30, round(cct_bw_isp2), 30, round(cct_bw_isp3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1$margin), length(isp30$margin), 
                             length(isp_cct2$margin), length(isp30$margin), length(isp_cct3$margin))))

#### TABLE B6 ####
## Using same dataframe as above, calculate optimal bandwidth for Poisson reg
bw_isp1.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 1, q = 2, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1.4 <- bw_isp1.4$bws[1]
bw_isp2.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 2, q = 3, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2.4 <- bw_isp2.4$bws[1]
bw_isp3.4 <- rdbwselect(y = isp_group_totals$hom, x = isp_group_totals$margin, p = 3, q = 4, 
                        covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3.4 <- bw_isp3.4$bws[1]
isp_cct1.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1.4)) #linear
isp_cct2.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2.4)) #quadratic
isp_cct3.4 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3.4)) #cubic
## Models
isp1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp30)
isp2 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
isp3 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp30)
ireg1 <- coeftest(isp1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2 <- coeftest(isp2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3 <- coeftest(isp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
isp1.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday),family = poisson(link = "log"), isp_cct1.4)
isp2.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct2.4)
isp3.1 <- glm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday),family = poisson(link = "log"), isp_cct3.4)
ireg1.1 <- coeftest(isp1.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg2.1 <- coeftest(isp2.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ireg3.1 <- coeftest(isp3.1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table output
stargazer(ireg1, ireg1.1, ireg2, ireg2.1, ireg3, ireg3.1,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides: Poisson",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1.4), 30, round(cct_bw_isp2.4), 30, round(cct_bw_isp3.4)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1.4$margin), length(isp30$margin), length(isp_cct2.4$margin),
                             length(isp30$margin), length(isp_cct3.4$margin))))


#### TABLE B7 ####
## Bandwidths and data frames for "all shooting events" in panel 1
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1_n <- rdbwselect(y = fogo_dp$n, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_n <- bw_fc1_n$bws[1]
bw_fc1_ninj <- rdbwselect(y = fogo_dp$n_injuries, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ninj <- bw_fc1_ninj$bws[1]
bw_fc1_ndea <- rdbwselect(y = fogo_dp$n_deaths, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ndea <- bw_fc1_ndea$bws[1]
fogo_cct_n <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_n)) #Shootings, quad spec 
fogo_cct_ninj <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ninj)) #Shootings, quad spec 
fogo_cct_ndea <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ndea)) #Shootings, quad spec 
## Models
n1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2) + factor(dp) + factor(weekday), fogo30)
n2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_n)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ninj)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ndea)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events involving police" in panel 2
Y = data.frame(cbind(factor(fogo_dp_cops$month), factor(fogo_dp_cops$weekday), factor(fogo_dp_cops$dp)))
colnames(Y) = c("Month", "Weekday", "CISP")
bw_fc2_n <- rdbwselect(y = fogo_dp_cops$n, x = fogo_dp_cops$margin, p = 2, q = 3, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_n <- bw_fc2_n$bws[1]
bw_fc2_ninj <- rdbwselect(y = fogo_dp_cops$n_injuries, x = fogo_dp_cops$margin, p = 2, q = 3, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ninj <- bw_fc2_ninj$bws[1]
bw_fc2_ndea <- rdbwselect(y = fogo_dp_cops$n_deaths, x = fogo_dp_cops$margin, p = 2, q = 3, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ndea <- bw_fc2_ndea$bws[1]
fogo_cct2_n <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_n)) #Shootings, quad spec 
fogo_cct2_ninj <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ninj)) #Injuries, quad spec 
fogo_cct2_ndea <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ndea)) #Deaths, quad spec 
## Models
p1 <- lm(n ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday), cops30)
p2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_n)
preg1 <- coeftest(p1, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg2 <- coeftest(p2, vcov=vcovCL, type = "HC1", cluster = ~dp)
p3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday), cops30)
p4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday) + factor(month), fogo_cct2_ninj)
preg3 <- coeftest(p3, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg4 <- coeftest(p4, vcov=vcovCL, type = "HC1", cluster = ~dp)
p5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday), cops30)
p6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_ndea)
preg5 <- coeftest(p5, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg6 <- coeftest(p6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events during ordinary policing" in panel 3
Z = data.frame(cbind(factor(fogo_op_all$month), factor(fogo_op_all$weekday), factor(fogo_op_all$dp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_fc3_n <- rdbwselect(y = fogo_op_all$n, x = fogo_op_all$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_n <- bw_fc3_n$bws[1]
bw_fc3_ninj <- rdbwselect(y = fogo_op_all$n_injuries, x = fogo_op_all$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ninj <- bw_fc3_ninj$bws[1]
bw_fc3_ndea <- rdbwselect(y = fogo_op_all$n_deaths, x = fogo_op_all$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ndea <- bw_fc3_ndea$bws[1]
fogo_cct3_n <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_n)) #Shootings, quad spec 
fogo_cct3_ninj <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ninj)) #Injuries, quad spec 
fogo_cct3_ndea <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ndea)) #Deaths, quad spec 
## Models
o1 <- lm(n ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), ops30)
o2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_n)
oreg1 <- coeftest(o1, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg2 <- coeftest(o2, vcov=vcovCL, type = "HC1", cluster = ~dp)
o3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), ops30)
o4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ninj)
oreg3 <- coeftest(o3, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg4 <- coeftest(o4, vcov=vcovCL, type = "HC1", cluster = ~dp)
o5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) +factor(dp)  + factor(weekday), ops30)
o6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ndea)
oreg5 <- coeftest(o5, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg6 <- coeftest(o6, vcov=vcovCL, type = "HC1", cluster = ~dp)
##Table output
## Panel 1: All
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fogo_cct_n$margin), 
                             length(fogo30$margin), length(fogo_cct_ninj$margin), length(fogo30$margin), length(fogo_cct_ndea$margin))))
##Panel 2: Just Police Shootings
stargazer(preg1, preg2, preg3, preg4,preg5, preg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc2_n), 30, round(cct_bw_fc2_ninj), 30, round(cct_bw_fc2_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(cops30$margin), length(fogo_cct2_n$margin), 
                             length(cops30$margin), length(fogo_cct2_ninj$margin), length(cops30$margin), length(fogo_cct2_ndea$margin))))
##Panel 3: Just Ordinary Policing
stargazer(oreg1, oreg2, oreg3, oreg4,oreg5, oreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc3_n), 30, round(cct_bw_fc3_ninj), 30, round(cct_bw_fc3_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(ops30$margin), length(fogo_cct3_n$margin), 
                             length(ops30$margin), length(fogo_cct3_ninj$margin), length(ops30$margin), length(fogo_cct3_ndea$margin))))


#### TABLE B8 ####
## Bandwidths and data frames for "all shooting events" in panel 1
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1_n <- rdbwselect(y = fogo_dp$n, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_n <- bw_fc1_n$bws[1]
bw_fc1_ninj <- rdbwselect(y = fogo_dp$n_injuries, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ninj <- bw_fc1_ninj$bws[1]
bw_fc1_ndea <- rdbwselect(y = fogo_dp$n_deaths, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ndea <- bw_fc1_ndea$bws[1]
fogo_cct_n <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_n)) #Shootings, cubic spec 
fogo_cct_ninj <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ninj)) #Shootings, cubic spec 
fogo_cct_ndea <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ndea)) #Shootings, cubic spec 
## Models
n1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(dp) + factor(weekday), fogo30)
n2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct_n)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday), fogo30)
n4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ninj)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday), fogo30)
n6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ndea)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events involving police" in panel 2
Y = data.frame(cbind(factor(fogo_dp_cops$month), factor(fogo_dp_cops$weekday), factor(fogo_dp_cops$dp)))
colnames(Y) = c("Month", "Weekday", "CISP")
bw_fc2_n <- rdbwselect(y = fogo_dp_cops$n, x = fogo_dp_cops$margin, p = 3, q = 4, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_n <- bw_fc2_n$bws[1]
bw_fc2_ninj <- rdbwselect(y = fogo_dp_cops$n_injuries, x = fogo_dp_cops$margin, p = 3, q = 4, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ninj <- bw_fc2_ninj$bws[1]
bw_fc2_ndea <- rdbwselect(y = fogo_dp_cops$n_deaths, x = fogo_dp_cops$margin, p = 3, q = 4, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ndea <- bw_fc2_ndea$bws[1]
fogo_cct2_n <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_n)) #Shootings, Linear spec 
fogo_cct2_ninj <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ninj)) #Injuries, Linear spec 
fogo_cct2_ndea <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ndea)) #Deaths, Linear spec 
## Models
p1 <- lm(n ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), cops30)
p2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_n)
preg1 <- coeftest(p1, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg2 <- coeftest(p2, vcov=vcovCL, type = "HC1", cluster = ~dp)
p3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), cops30)
p4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday) + factor(month), fogo_cct2_ninj)
preg3 <- coeftest(p3, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg4 <- coeftest(p4, vcov=vcovCL, type = "HC1", cluster = ~dp)
p5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan+  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), cops30)
p6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_ndea)
preg5 <- coeftest(p5, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg6 <- coeftest(p6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events during ordinary policing" in panel 3
Z = data.frame(cbind(factor(fogo_op_all$month), factor(fogo_op_all$weekday), factor(fogo_op_all$dp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_fc3_n <- rdbwselect(y = fogo_op_all$n, x = fogo_op_all$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_n <- bw_fc3_n$bws[1]
bw_fc3_ninj <- rdbwselect(y = fogo_op_all$n_injuries, x = fogo_op_all$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ninj <- bw_fc3_ninj$bws[1]
bw_fc3_ndea <- rdbwselect(y = fogo_op_all$n_deaths, x = fogo_op_all$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ndea <- bw_fc3_ndea$bws[1]
fogo_cct3_n <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_n)) #Shootings, Linear spec 
fogo_cct3_ninj <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ninj)) #Injuries, Linear spec 
fogo_cct3_ndea <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ndea)) #Deaths, Linear spec 
## Models
o1 <- lm(n ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), ops30)
o2 <- lm(n~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_n)
oreg1 <- coeftest(o1, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg2 <- coeftest(o2, vcov=vcovCL, type = "HC1", cluster = ~dp)
o3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday), ops30)
o4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ninj)
oreg3 <- coeftest(o3, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg4 <- coeftest(o4, vcov=vcovCL, type = "HC1", cluster = ~dp)
o5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan +  I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday), ops30)
o6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +  I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3)+factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ndea)
oreg5 <- coeftest(o5, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg6 <- coeftest(o6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Table output
##Panel 1: All
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fogo_cct_n$margin), 
                             length(fogo30$margin), length(fogo_cct_ninj$margin), length(fogo30$margin), length(fogo_cct_ndea$margin))))
##Panel 2: Just Police Shootings
stargazer(preg1, preg2, preg3, preg4,preg5, preg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc2_n), 30, round(cct_bw_fc2_ninj), 30, round(cct_bw_fc2_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(cops30$margin), length(fogo_cct2_n$margin), 
                             length(cops30$margin), length(fogo_cct2_ninj$margin), length(cops30$margin), length(fogo_cct2_ndea$margin))))
##Panel 3: Just Ordinary Policing
stargazer(oreg1, oreg2, oreg3, oreg4,oreg5, oreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc3_n), 30, round(cct_bw_fc3_ninj), 30, round(cct_bw_fc3_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(ops30$margin), length(fogo_cct3_n$margin), 
                             length(ops30$margin), length(fogo_cct3_ninj$margin), length(ops30$margin), length(fogo_cct3_ndea$margin))))


#### TABLE B9 ####
## Reload data to reset cutpoint(s)
load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Change margin 
isp_group_totals$margin <- isp_group_totals$margin - 17
fogo_dp$margin <- fogo_dp$margin - 17
## Add share police and homicide variables
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"


## Optimal Bandwidth 
## Police Killings 
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_isp1 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 1, q = 2,  covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1 <- bw_isp1$bws[1]
bw_isp2 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 2, q = 3,  covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2 <- bw_isp2$bws[1]
bw_isp3 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3 <- bw_isp3$bws[1]
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
isp_cct1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1)) #linear
isp_cct2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2)) #quadratic
isp_cct3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3)) #cubic
## Prop shootings with police 
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 1, q = 2,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 2, q = 3,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 3, q = 4,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc3 <- bw_fc3$bws[1]
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
fogo_cct1 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1)) #linear
fogo_cct2 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc2)) #quadratic
fogo_cct3 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc3)) #cubic

## Models = share police at shootings
m3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + factor(dp)  + factor(weekday), fogo30)
reg3 <- coeftest(m3, vcov=vcovCL, type = "HC1", cluster = ~dp)
m4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + factor(dp) + factor(month) + factor(weekday), fogo_cct1)
reg4 <- coeftest(m4, vcov=vcovCL, type = "HC1", cluster = ~dp)
q3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(dp) + factor(weekday), fogo30)
qreg3 <- coeftest(q3, vcov=vcovCL, type = "HC1", cluster = ~dp)
q4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(dp) + factor(weekday), fogo_cct2)
qreg4 <- coeftest(q4, vcov=vcovCL, type = "HC1", cluster = ~dp)
c3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
creg3 <- coeftest(c3, vcov=vcovCL, type = "HC1", cluster = ~dp)
c4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo_cct3)
creg4 <- coeftest(c4, vcov=vcovCL, type = "HC1", cluster = ~dp)

## Models - police killings
isp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday), isp30)
isp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), isp_cct1)
ispreg3 <- coeftest(isp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ispreg4 <- coeftest(isp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
qisp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
qisp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), isp_cct2)
qispreg3 <- coeftest(qisp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
qispreg4 <- coeftest(qisp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
cisp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
cisp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp) + factor(month) + factor(weekday), isp_cct3)
cispreg3 <- coeftest(cisp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
cispreg4 <- coeftest(cisp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)

## Table Output 
## Linear 
stargazer(ispreg3, ispreg4, reg3, reg4,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Linear",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp1), 30, round(cct_bw_fc1)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct1$margin), 
                             length(fogo30$margin), length(fogo_cct1$margin))))
## Quadratic
stargazer(qispreg3, qispreg4, qreg3, qreg4,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Quad",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp2), 30, round(cct_bw_fc2)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct2$margin), 
                             length(fogo30$margin), length(fogo_cct2$margin))))
## Cubic
stargazer(cispreg3, cispreg4, creg3, creg4,
          keep = c("\\bFederalBan\\b"), column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Cubic",
          column.separate = c(2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_isp3), 30, round(cct_bw_fc3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(isp_cct3$margin), 
                             length(fogo30$margin), length(fogo_cct3$margin))))


#### TABLE B10 ####
## Change cutpoint 
fogo_dp_cops$margin <- fogo_dp_cops$margin - 17
fogo_dp_nocops$margin <- fogo_dp_nocops$margin - 17
## set datetime
fogo_dp_cops$weekday <- weekdays(as.Date(fogo_dp_cops$data_ocorrencia))
fogo_dp_cops$month <- as.factor(format(as.Date(fogo_dp_cops$data_ocorrencia), "%m"))
fogo_op_all$weekday <- weekdays(as.Date(fogo_op_all$data_ocorrencia))
fogo_op_all$month <- as.factor(format(as.Date(fogo_op_all$data_ocorrencia), "%m"))
fogo_op_all <- subset(fogo_op_all, polpresent==1)
fogo_op_all$margin <- fogo_op_all$margin - 17
## Subset narrow bandwidth
cops30 <- subset(fogo_dp_cops, abs(margin) <= 30)
nocops30 <- subset(fogo_dp_nocops, abs(margin) <= 30)
ops30 <- subset(fogo_op_all, abs(margin) <= 30)
## Bandwidths and data frames for "all shooting events" in panel 1
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1_n <- rdbwselect(y = fogo_dp$n, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_n <- bw_fc1_n$bws[1]
bw_fc1_ninj <- rdbwselect(y = fogo_dp$n_injuries, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ninj <- bw_fc1_ninj$bws[1]
bw_fc1_ndea <- rdbwselect(y = fogo_dp$n_deaths, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1_ndea <- bw_fc1_ndea$bws[1]
fogo_cct_n <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_n)) #Shootings, Linear spec 
fogo_cct_ninj <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ninj)) #Shootings, Linear spec 
fogo_cct_ndea <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1_ndea)) #Shootings, Linear spec 
## Models
n1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n2 <- lm(n~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_n)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ninj)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct_ndea)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events involving police" in panel 2
Y = data.frame(cbind(factor(fogo_dp_cops$month), factor(fogo_dp_cops$weekday), factor(fogo_dp_cops$dp)))
colnames(Y) = c("Month", "Weekday", "CISP")
bw_fc2_n <- rdbwselect(y = fogo_dp_cops$n, x = fogo_dp_cops$margin, p = 1, q = 2, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_n <- bw_fc2_n$bws[1]
bw_fc2_ninj <- rdbwselect(y = fogo_dp_cops$n_injuries, x = fogo_dp_cops$margin, p = 1, q = 2, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ninj <- bw_fc2_ninj$bws[1]
bw_fc2_ndea <- rdbwselect(y = fogo_dp_cops$n_deaths, x = fogo_dp_cops$margin, p = 1, q = 2, covs = Y, cluster = Y$CISP, bwselect = "mserd")
cct_bw_fc2_ndea <- bw_fc2_ndea$bws[1]
fogo_cct2_n <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_n)) #Shootings, Linear spec 
fogo_cct2_ninj <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ninj)) #Injuries, Linear spec 
fogo_cct2_ndea <- subset(fogo_dp_cops, abs(margin) <= round(cct_bw_fc2_ndea)) #Deaths, Linear spec 
## Models
p1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), cops30)
p2 <- lm(n~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_n)
preg1 <- coeftest(p1, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg2 <- coeftest(p2, vcov=vcovCL, type = "HC1", cluster = ~dp)
p3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), cops30)
p4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday) + factor(month), fogo_cct2_ninj)
preg3 <- coeftest(p3, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg4 <- coeftest(p4, vcov=vcovCL, type = "HC1", cluster = ~dp)
p5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), cops30)
p6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct2_ndea)
preg5 <- coeftest(p5, vcov=vcovCL, type = "HC1", cluster = ~dp)
preg6 <- coeftest(p6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Bandwidths and data frames for "just shooting events during ordinary policing" in panel 3
Z = data.frame(cbind(factor(fogo_op_all$month), factor(fogo_op_all$weekday), factor(fogo_op_all$dp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_fc3_n <- rdbwselect(y = fogo_op_all$n, x = fogo_op_all$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_n <- bw_fc3_n$bws[1]
bw_fc3_ninj <- rdbwselect(y = fogo_op_all$n_injuries, x = fogo_op_all$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ninj <- bw_fc3_ninj$bws[1]
bw_fc3_ndea <- rdbwselect(y = fogo_op_all$n_deaths, x = fogo_op_all$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_fc3_ndea <- bw_fc3_ndea$bws[1]
fogo_cct3_n <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_n)) #Shootings, Linear spec 
fogo_cct3_ninj <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ninj)) #Injuries, Linear spec 
fogo_cct3_ndea <- subset(fogo_op_all, abs(margin) <= round(cct_bw_fc3_ndea)) #Deaths, Linear spec 
## Models
o1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), ops30)
o2 <- lm(n~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_n)
oreg1 <- coeftest(o1, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg2 <- coeftest(o2, vcov=vcovCL, type = "HC1", cluster = ~dp)
o3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), ops30)
o4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ninj)
oreg3 <- coeftest(o3, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg4 <- coeftest(o4, vcov=vcovCL, type = "HC1", cluster = ~dp)
o5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), ops30)
o6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fogo_cct3_ndea)
oreg5 <- coeftest(o5, vcov=vcovCL, type = "HC1", cluster = ~dp)
oreg6 <- coeftest(o6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Table output
##Panel 1: All
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fogo_cct_n$margin), 
                             length(fogo30$margin), length(fogo_cct_ninj$margin), length(fogo30$margin), length(fogo_cct_ndea$margin))))
##Panel 2: Just Police Shootings
stargazer(preg1, preg2, preg3, preg4,preg5, preg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc2_n), 30, round(cct_bw_fc2_ninj), 30, round(cct_bw_fc2_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(cops30$margin), length(fogo_cct2_n$margin), 
                             length(cops30$margin), length(fogo_cct2_ninj$margin), length(cops30$margin), length(fogo_cct2_ndea$margin))))
##Panel 3: Just Ordinary Policing
stargazer(oreg1, oreg2, oreg3, oreg4,oreg5, oreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc3_n), 30, round(cct_bw_fc3_ninj), 30, round(cct_bw_fc3_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(ops30$margin), length(fogo_cct3_n$margin), 
                             length(ops30$margin), length(fogo_cct3_ninj$margin), length(ops30$margin), length(fogo_cct3_ndea$margin))))


#### TABLE B11 ####
colnames(isp_group_totals)[17] <- "hom"
colnames(isp30)[17] <- "hom"
#### Set optimal bandwidths
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
## Subset data frames 
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
## Models
## Homicides
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
## Robberies
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
## Theft
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
## Extortion
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

## Table output
stargazer(hreg1,hreg2,hreg3,hreg4,hreg5,hreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_hom1), 30, round(cct_bw_hom2), 30, round(cct_bw_hom3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))
stargazer(freg1,freg2,freg3,freg4,freg5,freg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_furto1), 30, round(cct_bw_furto2), 30, round(cct_bw_furto3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(furto1$margin), length(isp30$margin), 
                             length(furto2$margin), length(isp30$margin), length(furto3$margin))))
stargazer(rreg1,rreg2,rreg3,rreg4,rreg5,rreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_rob1), 30, round(cct_bw_rob2), 30, round(cct_bw_rob3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(rob1$margin), length(isp30$margin), 
                             length(rob2$margin), length(isp30$margin), length(rob3$margin))))
stargazer(ereg1,ereg2,ereg3,ereg4,ereg5,ereg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_ext1), 30, round(cct_bw_ext2), 30, round(cct_bw_ext3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(ext1$margin), length(isp30$margin), 
                             length(ext2$margin), length(isp30$margin), length(ext3$margin))))


#### TABLE B12 ####
## Reload data
load("isp_daily.RData")
## Add month, year, day controls 
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
colnames(isp_group_totals)[17] <- "hom"
## Subset to which precincts have the highest # of violence 
homs19 <- isp19_group_totals %>%
  group_by(cisp) %>%
  summarise(cvli = sum(cvli))
mostviolent <- homs19$cisp[which(homs19$cvli >= quantile(homs19$cvli)[4])] # top 75%
isp_high <- subset(isp_group_totals, cisp %in% mostviolent)
## Change cutpoint
isp_high$margin <- isp_high$margin - 17
isp30 <- subset(isp_high, abs(margin) <= 30)

#### Set optimal bandwidth 
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
## Models
h1 <- lm(hom ~ margin+FederalBan + margin*FederalBan  +factor(cisp) + factor(weekday), isp30)
h2 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +factor(cisp) + factor(weekday), isp30)
h3 <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +  I(margin^3) + I(margin^3)*FederalBan +factor(cisp) + factor(weekday), isp30)
hreg1 <- coeftest(h1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
hreg2 <- coeftest(h2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
hreg3 <- coeftest(h3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
h1o <- lm(hom ~ margin+FederalBan + margin*FederalBan  +factor(cisp) + factor(weekday), hom1)
h2o <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan +factor(cisp) + factor(weekday), hom2)
h3o <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + I(margin^2)*FederalBan + I(margin^3) + I(margin^3)*FederalBan +factor(cisp) + factor(weekday), hom3)
horeg1 <- coeftest(h1o, vcov=vcovCL, type = "HC1", cluster = ~cisp)
horeg2 <- coeftest(h2o, vcov=vcovCL, type = "HC1", cluster = ~cisp)
horeg3 <- coeftest(h3o, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Table output
stargazer(hreg1,horeg1,hreg2,horeg2,hreg3,horeg3,
          keep = c("\\bFederalBan\\b"), 
          title = "Homicides (Top Quartile)",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(hom1cct), 30, round(hom2cct), 30, round(hom3cct)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))


#### TABLE B13 ####
## Reload data for tables B13-14
load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Add share police and homicide variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"
## Subset to just many crim group precincts (taken from Disque Denuncia map)
isp_group_totals <- subset(isp_group_totals, cisp %in% c("006a. Cidade Nova", "021a. Bonsucesso","031a. Ricardo Albuquerque","034a. Bangu","037a. Ilha do Governador",
                                                         "038a. Braz de Pina","039a. Pavuna","050a. Itaguaí","054a. Belford Roxo","056a. Comendador Soares","059a. Duque de Caxias",
                                                         "060a. Campos Elíseos","064a. Vilar dos Teles","071a. Itaboraí","074a. Alcantara","078a. Fonseca"))
fogo_dp <- subset(fogo_dp, dp %in% c(6, 21, 31, 34, 37, 38, 39, 50, 54, 56, 59, 60, 64, 71, 74, 78))

## Set bandwidths
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
## Polkilling  
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_p1 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p1 <- bw_p1$bws[1]
bw_p2 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p2 <- bw_p2$bws[1]
bw_p3 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p3 <- bw_p3$bws[1]
p1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p1)) 
p2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p2)) 
p3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p3)) 
### Share police
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc3 <- bw_fc3$bws[1]
fc1 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1)) 
fc2 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc2)) 
fc3 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc3)) 
## Models - police killings
# Linear
po1 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
poreg1 <- coeftest(po1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po2 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), p1)
poreg2 <- coeftest(po2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
poreg3 <- coeftest(po3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), p2)
poreg4 <- coeftest(po4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po5 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
poreg5 <- coeftest(po5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po6 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), p3)
poreg6 <- coeftest(po6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Models - Share police
n1 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n2 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)   +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg3<- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Table output
stargazer(poreg1,poreg2,poreg3,poreg4,poreg5,poreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Police Killings",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_p1), 30, round(cct_bw_p2), 30, round(cct_bw_p3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(p1$margin), length(isp30$margin), 
                             length(p2$margin), length(isp30$margin), length(p3$margin))))
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Share Police",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1), 30, round(cct_bw_fc2), 30, round(cct_bw_fc3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fc1$margin), length(fogo30$margin), 
                             length(fc2$margin), length(fogo30$margin), length(fc3$margin))))
                             
#### TABLE B14 ####
## Set bandwidths
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
## Models
## Homicides
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
## Robberies
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
## Theft
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

## Table output
stargazer(hreg1,hreg2,hreg3,hreg4,hreg5,hreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_hom1), 30, round(cct_bw_hom2), 30, round(cct_bw_hom3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))
stargazer(freg1,freg2,freg3,freg4,freg5,freg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_furto1), 30, round(cct_bw_furto2), 30, round(cct_bw_furto3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(furto1$margin), length(isp30$margin), 
                             length(furto2$margin), length(isp30$margin), length(furto3$margin))))
stargazer(rreg1,rreg2,rreg3,rreg4,rreg5,rreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_rob1), 30, round(cct_bw_rob2), 30, round(cct_bw_rob3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(rob1$margin), length(isp30$margin), 
                             length(rob2$margin), length(isp30$margin), length(rob3$margin))))
stargazer(ereg1,ereg2,ereg3,ereg4,ereg5,ereg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_ext1), 30, round(cct_bw_ext2), 30, round(cct_bw_ext3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(ext1$margin), length(isp30$margin), 
                             length(ext2$margin), length(isp30$margin), length(ext3$margin))))



#### TABLE B15 ####
## Reload data for tables B15-16
load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Add share police and homicide variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"
## Subset to just milicia group precincts (taken from Disque Denuncia map)
isp_group_totals <- subset(isp_group_totals, cisp %in% c("016a. Barra da Tijuca", "028a. Campinho", "032a. Taquara", "035a. Campo Grande",
                                                         "036a. Santa Cruz","041a. Tanque","043a. Pedra de Guaratiba","048a. Seropédica", "056a. Comendador Soares"))
fogo_dp <- subset(fogo_dp, dp %in% c(16, 28, 32, 35, 36, 41, 43, 48, 56))

## Set bandwidths
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
## Polkilling  
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_p1 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p1 <- bw_p1$bws[1]
bw_p2 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p2 <- bw_p2$bws[1]
bw_p3 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p3 <- bw_p3$bws[1]
p1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p1)) 
p2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p2)) 
p3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p3)) 
### Share police
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc3 <- bw_fc3$bws[1]
fc1 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1)) 
fc2 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc2)) 
fc3 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc3)) 
## Models - police killings
# Linear
po1 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
poreg1 <- coeftest(po1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po2 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), p1)
poreg2 <- coeftest(po2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
poreg3 <- coeftest(po3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), p2)
poreg4 <- coeftest(po4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po5 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
poreg5 <- coeftest(po5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po6 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), p3)
poreg6 <- coeftest(po6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Models - Share police
n1 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n2 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)   +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg3<- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Table output
stargazer(poreg1,poreg2,poreg3,poreg4,poreg5,poreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Police Killings",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_p1), 30, round(cct_bw_p2), 30, round(cct_bw_p3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(p1$margin), length(isp30$margin), 
                             length(p2$margin), length(isp30$margin), length(p3$margin))))
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Share Police",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1), 30, round(cct_bw_fc2), 30, round(cct_bw_fc3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fc1$margin), length(fogo30$margin), 
                             length(fc2$margin), length(fogo30$margin), length(fc3$margin))))


#### TABLE B16 ####
## Set bandwidths
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
## Models
## Homicides
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
## Robberies
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
## Theft
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

## Table output
stargazer(hreg1,hreg2,hreg3,hreg4,hreg5,hreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_hom1), 30, round(cct_bw_hom2), 30, round(cct_bw_hom3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))
stargazer(freg1,freg2,freg3,freg4,freg5,freg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_furto1), 30, round(cct_bw_furto2), 30, round(cct_bw_furto3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(furto1$margin), length(isp30$margin), 
                             length(furto2$margin), length(isp30$margin), length(furto3$margin))))
stargazer(rreg1,rreg2,rreg3,rreg4,rreg5,rreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_rob1), 30, round(cct_bw_rob2), 30, round(cct_bw_rob3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(rob1$margin), length(isp30$margin), 
                             length(rob2$margin), length(isp30$margin), length(rob3$margin))))
stargazer(ereg1,ereg2,ereg3,ereg4,ereg5,ereg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_ext1), 30, round(cct_bw_ext2), 30, round(cct_bw_ext3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(ext1$margin), length(isp30$margin), 
                             length(ext2$margin), length(isp30$margin), length(ext3$margin))))


#### TABLE B17 ####
## Reload data for tables B15-16
load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
## Add share police and homicide variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"
## Subset to just Zona Sul precincts (taken from Disque Denuncia map)
isp_group_totals <- subset(isp_group_totals, cisp %in% c("009a. Catete","010a. Botafogo","011a. Rocinha","012a. Copacabana","013a. Ipanema" ,"014a. Leblon","015a. Gávea"))
fogo_dp <- subset(fogo_dp, dp %in% c(9, 10, 11, 12, 13, 14, 15))

## Set bandwidths
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
## Polkilling  
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
bw_p1 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 1, q = 2, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p1 <- bw_p1$bws[1]
bw_p2 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 2, q = 3, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p2 <- bw_p2$bws[1]
bw_p3 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_p3 <- bw_p3$bws[1]
p1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p1)) 
p2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p2)) 
p3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_p3)) 
### Share police
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 1, q = 2, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 2, q = 3, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 3, q = 4, covs = W, cluster = W$CISP, bwselect = "mserd")
cct_bw_fc3 <- bw_fc3$bws[1]
fc1 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1)) 
fc2 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc2)) 
fc3 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc3)) 
## Models - police killings
# Linear
po1 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp)  + factor(weekday), isp30)
poreg1 <- coeftest(po1, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po2 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), p1)
poreg2 <- coeftest(po2, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
poreg3 <- coeftest(po3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), p2)
poreg4 <- coeftest(po4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po5 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp)  + factor(weekday), isp30)
poreg5 <- coeftest(po5, vcov=vcovCL, type = "HC1", cluster = ~cisp)
po6 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+ factor(cisp) + factor(month) + factor(weekday), p3)
poreg6 <- coeftest(po6, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## Models - Share police
n1 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday), fogo30)
n2 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~dp)
n3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)   +factor(dp)  + factor(weekday), fogo30)
n4 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2)  +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg3<- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~dp)
n5 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
n6 <- lm(sharepolice~ margin+FederalBan + margin*FederalBan  + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday)+ factor(month), fc1)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~dp)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~dp)
## Table output
stargazer(poreg1,poreg2,poreg3,poreg4,poreg5,poreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Police Killings",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_p1), 30, round(cct_bw_p2), 30, round(cct_bw_p3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(p1$margin), length(isp30$margin), 
                             length(p2$margin), length(isp30$margin), length(p3$margin))))
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Share Police",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1), 30, round(cct_bw_fc2), 30, round(cct_bw_fc3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fc1$margin), length(fogo30$margin), 
                             length(fc2$margin), length(fogo30$margin), length(fc3$margin))))

#### TABLE B18 ####
## Set bandwidths
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
## Models
## Homicides
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
## Robberies
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
## Theft
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

## Table output
stargazer(hreg1,hreg2,hreg3,hreg4,hreg5,hreg6,
          keep = c("\\bFederalBan\\b"), column.labels = c("Linear", "Quadratic", "Cubic"), 
          title = "Homicides",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_hom1), 30, round(cct_bw_hom2), 30, round(cct_bw_hom3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(hom1$margin), length(isp30$margin), 
                             length(hom2$margin), length(isp30$margin), length(hom3$margin))))
stargazer(freg1,freg2,freg3,freg4,freg5,freg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Theft",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_furto1), 30, round(cct_bw_furto2), 30, round(cct_bw_furto3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(furto1$margin), length(isp30$margin), 
                             length(furto2$margin), length(isp30$margin), length(furto3$margin))))
stargazer(rreg1,rreg2,rreg3,rreg4,rreg5,rreg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Robberies",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_rob1), 30, round(cct_bw_rob2), 30, round(cct_bw_rob3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(rob1$margin), length(isp30$margin), 
                             length(rob2$margin), length(isp30$margin), length(rob3$margin))))
stargazer(ereg1,ereg2,ereg3,ereg4,ereg5,ereg6,
          keep = c("\\bFederalBan\\b"), #column.labels = c("Police Killings", "Share of Police Shootings"), 
          title = "Extortion",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_ext1), 30, round(cct_bw_ext2), 30, round(cct_bw_ext3)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(isp30$margin), length(ext1$margin), length(isp30$margin), 
                             length(ext2$margin), length(isp30$margin), length(ext3$margin))))


