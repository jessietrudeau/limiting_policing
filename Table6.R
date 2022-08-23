## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the regression models reported in Table 6
## 
load("fogo_rd.RData")

# Add date characteristics 
pe_final <- fogo_pe
pe_final$weekday <- weekdays(as.Date(pe_final$data_ocorrencia))
pe_final$month <- as.factor(format(as.Date(pe_final$data_ocorrencia), "%m"))
pe_final$year <- as.factor(format(as.Date(pe_final$data_ocorrencia), "%Y"))
pe_final$week <- week(as.Date(pe_final$data_ocorrencia))
pe_final$margin <- as.numeric(as.Date(pe_final$data_ocorrencia)) - as.numeric(as.Date("2020-05-19"))
pe_final$FederalBan <- 0
pe_final$FederalBan[which(pe_final$data_ocorrencia > "2020-05-19")] <- 1

# Now subset data to smaller DFs and define bandwidth
pe30 <- subset(pe_final, abs(margin) <= 30)

W = data.frame(cbind(factor(pe_final$month), factor(pe_final$weekday), factor(pe_final$nome_cidade)))
colnames(W) = c("Month", "Weekday", "City")
bw_fc1_n <- rdbwselect(y = pe_final$n, x = pe_final$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_n <- bw_fc1_n$bws[1]
bw_fc1_ninj <- rdbwselect(y = pe_final$n_injuries, x = pe_final$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_ninj <- bw_fc1_ninj$bws[1]
bw_fc1_ndea <- rdbwselect(y = pe_final$n_deaths, x = pe_final$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_ndea <- bw_fc1_ndea$bws[1]
fogo_cct_n <- subset(pe_final, abs(margin) <= round(cct_bw_fc1_n)) #Shootings, Linear spec 
fogo_cct_ninj <- subset(pe_final, abs(margin) <= round(cct_bw_fc1_ninj)) #Injuries, Linear spec 
fogo_cct_ndea <- subset(pe_final, abs(margin) <= round(cct_bw_fc1_ndea)) #Deaths, Linear spec 

## Panel A models 
n1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n2 <- lm(n~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_n)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
n3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_ninj)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
n5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_ndea)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)


##Panel A: All
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6, 
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence in Recife",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Narrow", "Optimal", "Narrow", "Optimal", "Narrow", "Optimal"),
                           c("Observations", length(pe30$margin), length(fogo_cct_n$margin), 
                             length(pe30$margin), length(fogo_cct_ninj$margin), length(pe30$margin), length(fogo_cct_ndea$margin))))


### Data frame for Panel B ###
fogo_pe_cops <- subset(pe_final, polpresent ==1)

# Now subset data to smaller DFs and define bandwidth
pe30 <- subset(fogo_pe_cops, abs(margin) <= 30)

W = data.frame(cbind(factor(fogo_pe_cops$month), factor(fogo_pe_cops$weekday), factor(fogo_pe_cops$nome_cidade)))
colnames(W) = c("Month", "Weekday", "City")
bw_fc1_n <- rdbwselect(y = fogo_pe_cops$n, x = fogo_pe_cops$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_n <- bw_fc1_n$bws[1]
bw_fc1_ninj <- rdbwselect(y = fogo_pe_cops$n_injuries, x = fogo_pe_cops$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_ninj <- bw_fc1_ninj$bws[1]
bw_fc1_ndea <- rdbwselect(y = fogo_pe_cops$n_deaths, x = fogo_pe_cops$margin, p = 1, q = 2, covs = W, cluster = W$City, bwselect = "mserd")
cct_bw_fc1_ndea <- bw_fc1_ndea$bws[1]
fogo_cct_n <- subset(fogo_pe_cops, abs(margin) <= round(cct_bw_fc1_n)) #Shootings, Linear spec 
fogo_cct_ninj <- subset(fogo_pe_cops, abs(margin) <= round(cct_bw_fc1_ninj)) #Injuries, Linear spec 
fogo_cct_ndea <- subset(fogo_pe_cops, abs(margin) <= round(cct_bw_fc1_ndea)) #Deaths, Linear spec 

## Panel B models 
n1 <- lm(n ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n2 <- lm(n~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_n)
nreg1 <- coeftest(n1, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg2 <- coeftest(n2, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
n3 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n4 <- lm(n_injuries ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_ninj)
nreg3 <- coeftest(n3, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg4 <- coeftest(n4, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
n5 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday), pe30)
n6 <- lm(n_deaths ~ margin+FederalBan + margin*FederalBan  +factor(nome_cidade)  + factor(weekday)+ factor(month), fogo_cct_ndea)
nreg5 <- coeftest(n5, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)
nreg6 <- coeftest(n6, vcov=vcovCL, type = "HC1", cluster = ~nome_cidade)

##Panel B: Just cops 
stargazer(nreg1,nreg2,nreg3,nreg4,nreg5,nreg6, 
          keep = c("FederalBan"), column.labels = c("Shootings", "Injuries", "Deaths"), 
          title = "Effect of Police Operation Ban on Violence in Recife",
          column.separate = c(2, 2, 2),  
          add.lines = list(c("Bandwidth", 30, round(cct_bw_fc1_n), 30, round(cct_bw_fc1_ninj), 30, round(cct_bw_fc1_ndea)),
                           c("Bandwidth Type", "Narrow", "Optimal", "Narrow", "Optimal", "Narrow", "Optimal"),
                           c("Observations", length(pe30$margin), length(fogo_cct_n$margin), 
                             length(pe30$margin), length(fogo_cct_ninj$margin), length(pe30$margin), length(fogo_cct_ndea$margin))))

