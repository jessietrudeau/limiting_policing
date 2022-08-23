## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces the histogram in Figure 3 showing the frequency of shootings 
## corresponding to different types of police action
## 
library(tidyverse)
library(gridExtra)
load("fogo_RJ_PE_rawdata.RData")

## Tag for different kinds of police action
## 0 is no police presence at a shooting, 1 is police presence but no operation, 
fogocruzado_rj$action <- 0
fogocruzado_rj$action[which(fogocruzado_rj$motivo_principal=="Ataque a agentes de segurança" | 
                              fogocruzado_rj$motivo_complementar =="Ataque a agentes de segurança")]  <- 2
fogocruzado_rj$action[which(fogocruzado_rj$motivo_principal=="Ação policial" | fogocruzado_rj$motivo_complementar =="Ação policial")] <- 1
fogocruzado_rj$margin <-as.numeric(as.Date(fogocruzado_rj$data_ocorrencia)) - as.numeric(as.Date("2020-05-19"))
fogo <- subset(fogocruzado_rj, margin <= 90 & data_ocorrencia >= "2020-01-01" & presen_agen_segur_ocorrencia == 1)

## Make histogram
pdf("policingtypes.pdf", width = 10, height = 7)
ggplot(fogo, aes(x = margin, fill = factor(action))) + geom_histogram(binwidth = 7) + theme_bw() + 
  theme(legend.position = "bottom", text = element_text(size = 20)) + 
  scale_fill_discrete(name = "Policing Type", labels = c("Raid", "Ordinary Policing", "Attack Against Police"))+
  xlab("Days") +ylab("Shootings Involving Police") + geom_vline(xintercept = 0, linetype = "dashed")
dev.off()


