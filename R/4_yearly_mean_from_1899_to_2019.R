# Making yearly mean plot from 1899 to 2019. 
# --------------------------------------
# -- Using Nordli's composite series: --
# --------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
data<-read.table("Data/Nordli_series_from_1898_to_2019.txt", skip = 29, dec=".",
                 sep=";", colClasses = c("integer","character",rep("numeric",1)), 
                 header=TRUE)
data$Date <- as.Date(paste("01.",data$Month,sep=""), "%d.%m.%Y")
data$year <- year(data$Date)
data<-data[data$year<=2019 & data$year>=1899,]

mean.year <- data %>%
  group_by(year) %>%
  dplyr::summarize(Mean = mean(TAM, na.rm=TRUE))
mean.year <- as.data.frame(mean.year)
mean.year$year.k <- 0:(nrow(mean.year)-1)
lm.full <- lm(Mean ~ year.k, data=mean.year)
lm.nordli <- lm(Mean ~ year.k, data=mean.year[mean.year$year<=2012,])
lm.post76 <- lm(Mean ~ year.k, data=mean.year[mean.year$year>=1976,])
mean.year$lm.full <- fitted(lm.full)
mean.year$lm.post76 <- c(rep(NA, length(which(mean.year$year <1976))),fitted(lm.post76))
mean.year$lm.nordli <- c(fitted(lm.nordli),rep(NA,length(which(mean.year$year>2012))))

(q<-ggplot(data=mean.year, aes(year, Mean))+geom_line(col = ifelse(mean.year$year<1976,"springgreen4","blue"))+
    geom_line(aes(year,lm.full),col = "red",lty=1)+
    geom_line(aes(year,lm.post76),col = "red",lty=2)+
    scale_y_continuous(breaks = seq(-14,0,2), name = "Temperature (°C)")+
    scale_x_continuous(expand = c(0.02,0.7), breaks = seq(1900,2020,20),name = "")+
    theme_minimal()+
    theme(panel.grid = element_blank(), axis.line = element_line(),axis.ticks = element_line())
)
ggsave("Figures/trends_composite_temperature_svalbard20.pdf",width=8,height=2.5)
ggsave("Figures/trends_composite_temperature_svalbard20_high.pdf", width=5,height=1.9)
