## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 1
## 
library(tidyverse)
library(flextable)
library(stargazer)

load("fogo_rd.RData")
load("isp_daily.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))

isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))

## Add share police and pol killing variables 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"

##### DEFINE BANDWIDTHS #####
## Prop shootings with police 
W = data.frame(cbind(factor(fogo_dp$month), factor(fogo_dp$weekday), factor(fogo_dp$dp)))
colnames(W) = c("Month", "Weekday", "CISP")
bw_fc1 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 1, q = 2,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc1 <- bw_fc1$bws[1]
bw_fc2 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 2, q = 3,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc2 <- bw_fc2$bws[1]
bw_fc3 <- rdbwselect(y = fogo_dp$sharepolice, x = fogo_dp$margin, p = 3, q = 4,  covs = W, cluster = W$dp, bwselect = "mserd")
cct_bw_fc3 <- bw_fc3$bws[1]
## Subset data frames according to bandwidth
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
fogo_cct1 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc1)) #linear
fogo_cct2 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc2)) #quadratic
fogo_cct3 <- subset(fogo_dp, abs(margin) <= round(cct_bw_fc3)) #cubic

## Police Killings 
Z = data.frame(cbind(factor(isp_group_totals$month),factor(isp_group_totals$weekday), factor(isp_group_totals$cisp)))
colnames(Z) = c("Month", "Weekday", "CISP")
## Calculate bandwidths 
bw_isp1 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 1, q = 2,  covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp1 <- bw_isp1$bws[1]
bw_isp2 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 2, q = 3,  covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp2 <- bw_isp2$bws[1]
bw_isp3 <- rdbwselect(y = isp_group_totals$polkilling, x = isp_group_totals$margin, p = 3, q = 4, covs = Z, cluster = Z$CISP, bwselect = "mserd")
cct_bw_isp3 <- bw_isp3$bws[1]
## Subset data frames according to bandwidth
isp30 <- subset(isp_group_totals, abs(margin) <= 30)
isp_cct1 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp1)) #linear
isp_cct2 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp2)) #quadratic
isp_cct3 <- subset(isp_group_totals, abs(margin) <= round(cct_bw_isp3)) #cubic


#### RUN REGRESSIONS ####
#### FOGO CRUZADO - SHARE OF POLICE AT SHOOTING
## linear 
m3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + factor(dp)  + factor(weekday), fogo30)
reg3 <- coeftest(m3, vcov=vcovCL, type = "HC1", cluster = ~dp)
m4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + factor(dp) + factor(month) + factor(weekday), fogo_cct1)
reg4 <- coeftest(m4, vcov=vcovCL, type = "HC1", cluster = ~dp)
## quadratic   
q3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(dp) + factor(weekday), fogo30)
qreg3 <- coeftest(q3, vcov=vcovCL, type = "HC1", cluster = ~dp)
q4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(dp) + factor(weekday), fogo_cct2)
qreg4 <- coeftest(q4, vcov=vcovCL, type = "HC1", cluster = ~dp)
## cubic
c3 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo30)
creg3 <- coeftest(c3, vcov=vcovCL, type = "HC1", cluster = ~dp)
c4 <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2)+ I(margin^3) + FederalBan*I(margin^3) +factor(dp)  + factor(weekday), fogo_cct3)
creg4 <- coeftest(c4, vcov=vcovCL, type = "HC1", cluster = ~dp)


#### ISP - POLICE KILLINGS
## linear  
isp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday), isp30)
isp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(month) + factor(weekday), isp_cct1)
ispreg3 <- coeftest(isp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
ispreg4 <- coeftest(isp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## quadratic  
qisp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp)  + factor(weekday), isp30)
qisp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + factor(cisp) + factor(month) + factor(weekday), isp_cct2)
qispreg3 <- coeftest(qisp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
qispreg4 <- coeftest(qisp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)
## cubic  
cisp3 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp)  + factor(weekday), isp30)
cisp4 <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3) +factor(cisp) + factor(month) + factor(weekday), isp_cct3)
cispreg3 <- coeftest(cisp3, vcov=vcovCL, type = "HC1", cluster = ~cisp)
cispreg4 <- coeftest(cisp4, vcov=vcovCL, type = "HC1", cluster = ~cisp)




#### MAKE TABLE ####
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

