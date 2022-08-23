## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces Figures B1-B4 in the supplementary appendix
## Note: This file is meant to be run from top to bottom, i.e., some of the edits or data cleaning procedures at the top are carried through to later tables 
## 
library(tidyverse)
library(rdrobust)
load("fogo_rd.RData")
load("isp_daily.RData")
## Add var labels
fogo_dp$weekday <- weekdays(as.Date(fogo_dp$data_ocorrencia))
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
## Add share police variable 
fogo_dp$sharepolice <- fogo_dp$n_police/fogo_dp$n
fogo_dp$sharepolice[which(is.na(fogo_dp$sharepolice)==T)] <- 0
colnames(isp_group_totals)[32] <- "polkilling"
colnames(isp_group_totals)[17] <- "hom"

#### FIGURE B1 ####
## Loop to calculate bandwidths 
bws <- 1:60
ests_lin <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_lin) <- c("est", "lb", "ub")
ests_qua <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_qua) <- c("est", "lb", "ub")
ests_cub <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_cub) <- c("est", "lb", "ub")

for(i in 1:length(bws)) {
  bw <- bws[i]
  # Subset data frame 
  df_short <- subset(isp_group_totals, abs(margin) <= bw)
  # Run models
  lin_pc <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday), df_short)
  qua_pc <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(cisp) + factor(weekday), df_short)
  cub_pc <- lm(polkilling ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(cisp) + factor(weekday), df_short)
  lin_pc_se <- coeftest(lin_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  qua_pc_se <- coeftest(qua_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  cub_pc_se <- coeftest(cub_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  # Store Ban and confint coefficients
  ests_lin[i,1] <- lin_pc_se[3,1]
  ests_qua[i,1] <- qua_pc_se[3,1]
  ests_cub[i,1] <- cub_pc_se[3,1]
  ests_lin[i,2] <- lin_pc_se[3,1] - lin_pc_se[3,2]
  ests_qua[i,2] <- qua_pc_se[3,1] - qua_pc_se[3,2]
  ests_cub[i,2] <- cub_pc_se[3,1] - cub_pc_se[3,2]
  ests_lin[i,3] <- lin_pc_se[3,1] + lin_pc_se[3,2]
  ests_qua[i,3] <- qua_pc_se[3,1] + qua_pc_se[3,2]
  ests_cub[i,3] <- cub_pc_se[3,1] + cub_pc_se[3,2]
}

ests_lin$bw <- 1:60
pdf("polkilling_linear_bw.pdf", width = 15, height = 4)
ggplot(ests_lin, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Police Killings)") +
  ggtitle("Linear Specification")
dev.off()

ests_qua$bw <- 1:60
pdf("polkilling_quad_bw.pdf", width = 15, height = 4)
ggplot(ests_qua, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Police Killings)") +
  ggtitle("Quadratic Specification")
dev.off()

ests_cub$bw <- 1:60
pdf("polkilling_cubic_bw.pdf", width = 15, height = 4)
ggplot(ests_cub, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Police Killings)") +
  ggtitle("Cubic Specification")
dev.off()

#### FIGURE B2 ####
## Loop to calculate bandwidths 
bws <- 1:60
ests_lin <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_lin) <- c("est", "lb", "ub")
ests_qua <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_qua) <- c("est", "lb", "ub")
ests_cub <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_cub) <- c("est", "lb", "ub")

for(i in 1:length(bws)) {
    bw <- bws[i]
    # Subset data frame 
    df_short <- subset(fogo_dp, abs(margin) <= bw)
    # Run models
    lin_fc <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + factor(dp) + factor(weekday), df_short)
    qua_fc <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(dp) + factor(weekday), df_short)
    cub_fc <- lm(sharepolice ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(dp) + factor(weekday), df_short)
    lin_fc_se <- coeftest(lin_fc, vcov=vcovCL, type = "HC1", cluster = ~dp)
    qua_fc_se <- coeftest(qua_fc, vcov=vcovCL, type = "HC1", cluster = ~dp)
    cub_fc_se <- coeftest(cub_fc, vcov=vcovCL, type = "HC1", cluster = ~dp)
    # Store Ban and confint coefficients
    ests_lin[i,1] <- lin_fc_se[3,1]
    ests_qua[i,1] <- qua_fc_se[3,1]
    ests_cub[i,1] <- cub_fc_se[3,1]
    ests_lin[i,2] <- lin_fc_se[3,1] - lin_fc_se[3,2]
    ests_qua[i,2] <- qua_fc_se[3,1] - qua_fc_se[3,2]
    ests_cub[i,2] <- cub_fc_se[3,1] - cub_fc_se[3,2]
    ests_lin[i,3] <- lin_fc_se[3,1] + lin_fc_se[3,2]
    ests_qua[i,3] <- qua_fc_se[3,1] + qua_fc_se[3,2]
    ests_cub[i,3] <- cub_fc_se[3,1] + cub_fc_se[3,2]
  }
  
## Plots for various bws for linear, quadratic, and cubic functional forms 
ests_lin$bw <- 1:60
pdf("sharepolice_linear_bw.pdf", width = 15, height = 4)
ggplot(ests_lin, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Share of Shootings with Police Presence)") +
  ggtitle("Linear Specification")
dev.off()

ests_qua$bw <- 1:60
pdf("sharepolice_quad_bw.pdf", width = 15, height = 4)
ggplot(ests_qua, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Share of Shootings with Police Presence)") +
  ggtitle("Quadratic Specification")
dev.off()

ests_cub$bw <- 1:60
pdf("sharepolice_cubic_bw.pdf", width = 15, height = 4)
ggplot(ests_cub, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Share of Shootings with Police Presence)") +
  ggtitle("Cubic Specification")
dev.off()

#### FIGURE B3 ####
## Calculate bandwidths 
bws <- 1:60
ests_lin <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_lin) <- c("est", "lb", "ub")
ests_qua <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_qua) <- c("est", "lb", "ub")
ests_cub <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_cub) <- c("est", "lb", "ub")

for(i in 1:length(bws)) {
  bw <- bws[i]
  # Subset data frame 
  df_short <- subset(isp_group_totals, abs(margin) <= bw)
  # Run models
  lin_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday), df_short)
  qua_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(cisp) + factor(weekday), df_short)
  cub_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(cisp) + factor(weekday), df_short)
  lin_pc_se <- coeftest(lin_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  qua_pc_se <- coeftest(qua_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  cub_pc_se <- coeftest(cub_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  # Store Ban and confint coefficients
  ests_lin[i,1] <- lin_pc_se[3,1]
  ests_qua[i,1] <- qua_pc_se[3,1]
  ests_cub[i,1] <- cub_pc_se[3,1]
  ests_lin[i,2] <- lin_pc_se[3,1] - lin_pc_se[3,2]
  ests_qua[i,2] <- qua_pc_se[3,1] - qua_pc_se[3,2]
  ests_cub[i,2] <- cub_pc_se[3,1] - cub_pc_se[3,2]
  ests_lin[i,3] <- lin_pc_se[3,1] + lin_pc_se[3,2]
  ests_qua[i,3] <- qua_pc_se[3,1] + qua_pc_se[3,2]
  ests_cub[i,3] <- cub_pc_se[3,1] + cub_pc_se[3,2]
}

ests_lin$bw <- 1:60
pdf("hom_linear_bw.pdf", width = 15, height = 4)
ggplot(ests_lin, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Linear Specification")
dev.off()

ests_qua$bw <- 1:60
pdf("hom_quad_bw.pdf", width = 15, height = 4)
ggplot(ests_qua, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Quadratic Specification")
dev.off()

ests_cub$bw <- 1:60
pdf("hom_cubic_bw.pdf", width = 15, height = 4)
ggplot(ests_cub, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.2, 0.2)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Cubic Specification")
dev.off()


#### FIGURE B4 ####
## Reload data
load("isp_daily.RData")
isp_group_totals$weekday <- weekdays(as.Date((isp_group_totals$data_fato)))
isp_group_totals$month <- as.factor(format(as.Date(isp_group_totals$data_fato), "%m"))
colnames(isp_group_totals)[17] <- "hom"
## Define group with highest amount of lethal violence
homs19 <- isp19_group_totals %>%
  group_by(cisp) %>%
  summarise(cvli = sum(cvli))
mostviolent <- homs19$cisp[which(homs19$cvli >= quantile(homs19$cvli)[4])] # top 75%
isp_high <- subset(isp_group_totals, cisp %in% mostviolent)
isp30 <- subset(isp_high, abs(margin) <= 30)

## Loop to calculate bandwidths 
bws <- 1:60
ests_lin <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_lin) <- c("est", "lb", "ub")
ests_qua <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_qua) <- c("est", "lb", "ub")
ests_cub <- data.frame(matrix(nrow = 60, ncol = 3))
colnames(ests_cub) <- c("est", "lb", "ub")

## Homicides in Top Quartile
for(i in 1:length(bws)) {
  bw <- bws[i]
  # Subset data frame 
  df_short <- subset(isp_high, abs(margin) <= bw)
  # Run models
  lin_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + factor(cisp) + factor(weekday), df_short)
  qua_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) +factor(cisp) + factor(weekday), df_short)
  cub_pc <- lm(hom ~ margin+FederalBan + margin*FederalBan + I(margin^2) + FederalBan*I(margin^2) + I(margin^3) + FederalBan*I(margin^3)+factor(cisp) + factor(weekday), df_short)
  lin_pc_se <- coeftest(lin_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  qua_pc_se <- coeftest(qua_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  cub_pc_se <- coeftest(cub_pc, vcov=vcovCL, type = "HC1", cluster = ~cisp)
  # Store Ban and confint coefficients
  ests_lin[i,1] <- lin_pc_se[3,1]
  ests_qua[i,1] <- qua_pc_se[3,1]
  ests_cub[i,1] <- cub_pc_se[3,1]
  ests_lin[i,2] <- lin_pc_se[3,1] - lin_pc_se[3,2]
  ests_qua[i,2] <- qua_pc_se[3,1] - qua_pc_se[3,2]
  ests_cub[i,2] <- cub_pc_se[3,1] - cub_pc_se[3,2]
  ests_lin[i,3] <- lin_pc_se[3,1] + lin_pc_se[3,2]
  ests_qua[i,3] <- qua_pc_se[3,1] + qua_pc_se[3,2]
  ests_cub[i,3] <- cub_pc_se[3,1] + cub_pc_se[3,2]
}

ests_lin$bw <- 1:60
pdf("homtop_linear_bw.pdf", width = 15, height = 4)
ggplot(ests_lin, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) + coord_cartesian(ylim=c(-0.5, 0.5)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Linear Specification")
dev.off()

ests_qua$bw <- 1:60
pdf("homtop_quad_bw.pdf", width = 15, height = 4)
ggplot(ests_qua, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) +coord_cartesian(ylim=c(-0.5, 0.5)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Quadratic Specification")
dev.off()

ests_cub$bw <- 1:60
pdf("homtop_cubic_bw.pdf", width = 15, height = 4)
ggplot(ests_cub, aes(x = bw, y = est)) + geom_hline(yintercept = 0, color = "red") + scale_x_continuous(breaks = 1:60) +coord_cartesian(ylim=c(-0.5, 0.5)) +
  geom_pointrange(aes(ymin = lb, ymax = ub)) + theme_classic() + xlab("Bandwidth (days)") + ylab("Effect Size (Homicides)") +
  ggtitle("Cubic Specification")
dev.off()

