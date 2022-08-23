## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 2
## 
library(tidyverse)
library(stargazer)

load("fogo_rd.RData")
## Add month, year, day controls 
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
fogo_dp$month <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%m"))
fogo_dp$year <- as.factor(format(as.Date(fogo_dp$data_ocorrencia), "%Y"))

fogo_dp_cops$weekday <- weekdays(as.Date(fogo_dp_cops$data_ocorrencia))
fogo_dp_cops$month <- as.factor(format(as.Date(fogo_dp_cops$data_ocorrencia), "%m"))

fogo_op_all$weekday <- weekdays(as.Date(fogo_op_all$data_ocorrencia))
fogo_op_all$month <- as.factor(format(as.Date(fogo_op_all$data_ocorrencia), "%m"))
fogo_op_all <- subset(fogo_op_all, polpresent==1)

## Fixed bandwidth dataframes (narrow, 30 days)
fogo30 <- subset(fogo_dp, abs(margin) <= 30)
cops30 <- subset(fogo_dp_cops, abs(margin) <= 30)
nocops30 <- subset(fogo_dp_nocops, abs(margin) <= 30)
ops30 <- subset(fogo_op_all, abs(margin) <= 30)

#### PANEL A #### 
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

## Checking proportions 
.071/mean(fogo30$n[which(fogo30$margin<0)]) 
.028/mean(fogo30$n_injuries[which(fogo30$margin<0)]) 
.03/mean(fogo30$n_deaths[which(fogo30$margin<0)]) 

.097/mean(fogo_cct_n$n[which(fogo_cct_n$margin<0)]) 
.033/mean(fogo_cct_ninj$n_injuries[which(fogo_cct_ninj$margin<0)]) 
.036/mean(fogo_cct_ndea$n_deaths[which(fogo_cct_ndea$margin<0)]) 

#### PANEL B ####
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

#checking proportions
.046/mean(cops30$n[which(cops30$margin<0)]) 
.026/mean(cops30$n_injuries[which(cops30$margin<0)]) 
.031/mean(cops30$n_deaths[which(cops30$margin<0)]) 

.045/mean(fogo_cct2_n$n[which(fogo_cct2_n$margin<0)]) 
.03/mean(fogo_cct2_ninj$n_injuries[which(fogo_cct2_ninj$margin<0)]) 
.033/mean(fogo_cct2_ndea$n_deaths[which(fogo_cct2_ndea$margin<0)]) 

#### PANEL C ####
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

.018/mean(fogo_cct3_n$n[which(fogo_cct3_n$margin<0)]) 
.03/mean(fogo_cct3_ninj$n_injuries[which(fogo_cct3_ninj$margin<0)]) 


#### PRINTING TABLES ####
## Panel 1: All
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6,
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Fixed", "Optimal", "Fixed", "Optimal", "Fixed", "Optimal"),
                           c("Observations", length(fogo30$margin), length(fogo_cct_n$margin), 
                             length(fogo30$margin), length(fogo_cct_ninj$margin), length(fogo30$margin), length(fogo_cct_ndea$margin))))
## Panel 2: Just Police Shootings
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
