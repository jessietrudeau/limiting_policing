## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Reproduces Figure A1 showing google trends
## 
trends <- read_csv("googletrends.csv")
colnames(trends) <- c("Date", "João Pedro", "Witzel", "Crivella", "Covid")
trends$`João Pedro`[9] <- 0
trends$Crivella[2] <- 0
trends$`João Pedro` <- as.numeric(trends$`João Pedro`)
trends$Crivella <- as.numeric(trends$Crivella)

trends2 <- pivot_longer(trends, cols = c(`João Pedro`, Crivella, Witzel, Covid),
                        names_to = "Search Term", 
                        values_to = "Prevalence") 


pdf("googletrends.pdf", width = 10, height = 6)
ggplot(trends2, aes(x = Date, y = Prevalence, col = `Search Term`)) + geom_line(size=2) + theme_bw() + theme(legend.position = "bottom")
dev.off()
