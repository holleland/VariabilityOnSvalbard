# Figures
# Packages: 
library(ggplot2)
library(mapdata)
library(plyr)
library(dplyr)
library(lubridate)
library(ggpubr)

# ---------------------------------------------------------------------
# -- Change in distribution illustration: -----------------------------
# ---------------------------------------------------------------------
meantemp <- c(-13.999619, -8.784662)
dens.df <- data.frame(
  mean = meantemp+c(1,0), 
  sd = c(4.308112, 1.3),
  year = c(1976, 2019)
)
yend <- dnorm(0,sd = min(dens.df$sd))
q1 <- qnorm(.95, mean = dens.df$mean[1], sd = dens.df$sd[1])
q2 <- qnorm(.95, mean = dens.df$mean[2], sd = dens.df$sd[2])
q12 <- qnorm(.95, mean = dens.df$mean[2], sd = dens.df$sd[1])
q1a <- qnorm(.05, mean = dens.df$mean[1], sd = dens.df$sd[1])
q2a <- qnorm(.05, mean = dens.df$mean[2], sd = dens.df$sd[2])
q12a <- qnorm(.05, mean = dens.df$mean[2], sd = dens.df$sd[1])


xl <- -25
xu <- 4
options(warn = -1)
p1<-ggplot(dens.df)+stat_function(fun = dnorm, xlim = c(xl,xu), args=list(mean = dens.df$mean[1],
                                                                          sd = dens.df$sd[1]), col = "blue")+
  xlim(xl,xu)+mytheme+
  geom_area(stat = "function", fun = dnorm, xlim = c(q1,xu), 
            args=list(mean = dens.df$mean[1],
                      sd = dens.df$sd[1]), fill = "orchid2", alpha = .7, col = NA, lwd= 0)+
  geom_area(stat = "function", fun = dnorm, xlim = c(xl,q1a), 
            args=list(mean = dens.df$mean[1],
                      sd = dens.df$sd[1]), fill = "orchid2", alpha = .7, col = NA, lwd= 0)+
  #geom_segment(aes(x = q1, xend = q1, y = 0, yend = dnorm(q1,dens.df$mean[1],dens.df$sd[1])),
  #             col = "blue", size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  #geom_segment(aes(x = q1a, xend = q1a, y = 0, yend = dnorm(q1a,dens.df$mean[1],dens.df$sd[1])),
  #             col = "blue", size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  xlab("")+ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0,yend))+
  theme(axis.line.y = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  geom_vline(aes(xintercept = dens.df$mean[1]), col = "brown1", lty = 2)+
  geom_vline(aes(xintercept = q1), lty = 2, col = "darkblue")+
  geom_vline(aes(xintercept = q1a), lty = 2, col = "darkblue")

p1
p2<-ggplot(dens.df)+
  stat_function(fun = dnorm, xlim = c(xl,xu), args=list(mean = dens.df$mean[2],
                                                        sd = dens.df$sd[1]), col = "blue")+
  #stat_function(fun = dnorm, xlim = c(q12,xu), args=list(mean = dens.df$mean[2],
  #                                                      sd = dens.df$sd[1]), col = "blue",lty=2)+
  #  stat_function(fun = dnorm, xlim = c(xl,xu), args=list(mean = dens.df$mean[2],
  #                                                        sd = dens.df$sd[1]), col = "green")+
  xlim(xl,xu)+mytheme+
  geom_area(stat = "function", fun = dnorm, xlim = c(q12,xu), 
            args=list(mean = dens.df$mean[2],
                      sd = dens.df$sd[1]), fill = "orchid2", alpha = .7, col = NA, lwd= 0)+
  geom_area(stat = "function", fun = dnorm, xlim = c(xl,q12a), 
            args=list(mean = dens.df$mean[2],
                      sd = dens.df$sd[1]), fill = "orchid2", alpha = .7, col = NA, lwd= 0)+
  #geom_segment(aes(x = q12, xend = q12, y = 0, yend = dnorm(q12,dens.df$mean[2],dens.df$sd[1])),
  #             col = "blue", lty = 1, size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  #geom_segment(aes(x = q12a, xend = q12a, y = 0, yend = dnorm(q12a,dens.df$mean[2],dens.df$sd[1])),
  #             col = "blue", lty = 1, size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  
  xlab("")+ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0,yend))+
  theme(axis.line.y = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  geom_vline(aes(xintercept = dens.df$mean[1]), col = "brown1", lty = 2)+
  geom_vline(aes(xintercept = dens.df$mean[2]), col = "brown1", lty = 2)+
  geom_segment(aes(x = dens.df$mean[1], xend = dens.df$mean[2],
                   y = yend*.65, yend = yend*.65), arrow = arrow(ends = "last", length = unit(.1, "inches")),
               col = "red", lty = 1, size = .3)+
  geom_label(aes(x = dens.df$mean[1]-diff(dens.df$mean)/2-1, y = yend*.65, label = "Change\nin mean"), 
             fill = NA, col = "red", label.size = 0, size = 3)+
  geom_vline(aes(xintercept = q1), lty = 2, col = "darkblue")+
  geom_vline(aes(xintercept = q1a), lty = 2, col = "darkblue")
p2
p3<-ggplot(dens.df)+
  stat_function(fun = dnorm, xlim = c(xl,xu), args=list(mean = dens.df$mean[2],
                                                        sd = dens.df$sd[2]), col = "blue")+

  
  xlim(xl,xu)+mytheme+
  geom_area(stat = "function", fun = dnorm, xlim = c(q2,xu), 
            args=list(mean = dens.df$mean[2],
                      sd = dens.df$sd[2]), fill = "orchid2", alpha = .7,lwd= 0)+
  geom_area(stat = "function", fun = dnorm, xlim = c(xl,q2a), 
             args=list(mean = dens.df$mean[2],
                       sd = dens.df$sd[2]), fill = "orchid2", alpha = .7,lwd= 0)+
  xlab("")+ylab("")+
  #geom_segment(aes(x = q2, xend = q2, y = 0, yend = dnorm(q2,dens.df$mean[2],dens.df$sd[2])),
  #             col = "blue", size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  #geom_segment(aes(x = q2a, xend = q2a, y = 0, yend = dnorm(q2a,dens.df$mean[2],dens.df$sd[2])),
  #             col = "blue", size = .6,
  #             arrow = arrow(ends = "first", length = unit(.1, "inches")))+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,yend))+
  theme(axis.line.y = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  geom_vline(aes(xintercept = dens.df$mean[1]), col = "brown1", lty = 2)+
  geom_vline(aes(xintercept = dens.df$mean[2]), col = "brown1", lty = 2)+
  geom_segment(aes(x = dens.df$mean[1], xend = dens.df$mean[2],
                   y = yend*.65, yend = yend*.65), arrow = arrow(ends = "last", length = unit(.1, "inches")),
               col = "red", lty = 1, size = .3)+
  geom_label(aes(x = dens.df$mean[1]-diff(dens.df$mean)/2-1, y = yend*.65, label = "Change\nin mean"), 
             fill = NA, col = "red", label.size = 0, size = 3)+
  geom_vline(aes(xintercept = q1), lty = 2, col = "darkblue")+
  geom_vline(aes(xintercept = q1a), lty = 2, col = "darkblue")
p3
figure <- ggarrange(p1, p2, p3, 
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3, hjust = -3,
                    font.label = list(size = 12))

figure <- annotate_figure(figure, 
                          bottom = text_grob("Temperature (°C)", color = "black",
                                             size = 11, vjust = -1.5)
)
figure
ggsave(filename = "Figures/1_distribution_change.pdf", 
       width = 4, height = 4)
ggsave(filename = "../../Submission/Thesis_version/1_distribution_change_wide.pdf", 
       width = 6, height = 4)

# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------




# ---------------------
# -- Map of Svalbard --
# ---------------------
w2hr <- map_data("world2Hires")
norway<-w2hr[which(w2hr$region=="Norway"),]
svalbard.airport <- data.frame(lat = 78.2453, lon=15.5015)
svalbardmap <- ggplot() + geom_polygon(data = norway, aes(x=long, y = lat, group = group),
                                       fill="lightblue",col="black") +
  ylim(76,81)+xlim(10,31)+theme_classic()+
  geom_point(data=svalbard.airport, aes(x=lon, y = lat), pch = 19, col = 2, cex = 2)+
  xlab("")+ylab("")+theme(axis.line=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks=element_blank())

print(svalbardmap)
ggsave("Figures/2_svalbard_map.png", width=5,height=5)
ggsave("Figures/2_svalbard_map.pdf", width=5,height=5)

# ------------------------------------
# -- Nordli's series from 1899-2019 --
# ------------------------------------
nordli<-read.table("Data/2_Nordli_series_from_1898_to_2019.txt", skip = 29, dec=".",
                   sep=";", colClasses = c("integer","character",rep("numeric",1)), 
                   header=TRUE)
nordli$Date <- as.Date(paste("01.",nordli$Month,sep=""), "%d.%m.%Y")
nordli$year <- year(nordli$Date)
nordli<-nordli[nordli$year<=2019 & nordli$year>=1899,]

mean.year <- nordli %>%
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

ggplot(data=mean.year, aes(year, Mean))+geom_line(col = ifelse(mean.year$year<1976,"springgreen4","blue"))+
    geom_line(aes(year,lm.full),col = "red",lty=1)+
    geom_line(aes(year,lm.post76),col = "red",lty=2)+
    scale_y_continuous(breaks = seq(-14,0,2), name = "Temperature (°C)")+
    scale_x_continuous(expand = c(0.02,0.7), breaks = seq(1900,2020,20),name = "")+
    theme_minimal()+
    theme(panel.grid = element_blank(), axis.line = element_line(),axis.ticks = element_line())

#ggsave("Figures/trends_composite_temperature_svalbard20.pdf",width=8,height=2.5)
ggsave("Figures/3_trends_composite_temperature_svalbard20_high.pdf", width=5,height=1.9)
 #----------------------------------------------------------------------------



options(warn=-1) # To ignore warnings of plotting missing values: 
missdates <- seq(as.Date("1970-01-01"),as.Date("1975-12-31"),1)
missdates <- missdates[-which(mday(missdates)==29 & month(missdates)==2)]
df <- data.frame(Date = c(missdates, data$Date), 
                 temp = c(rep(NA,length(missdates)),data$TAM),
                 rollsd = c(rep(NA,length(missdates)),data$rollsd),
                 sig = c(rep(NA,length(missdates)), data$sig),
                 EWMA = c(rep(NA,length(missdates)),data$EWMA),
                 sigTMB = c(rep(NA,length(missdates)),data$sigTMB),
                 x = c(rep(NA,length(missdates)), data$x),
                 year = year(c(missdates,data$Date)))
df$decade = factor(ifelse(df$year < 1980, "1970s", ifelse(df$year<1990, "1980s", ifelse(df$year<2000, "1990s", 
                                                                                        ifelse(df$year <2010, "2000s","2010s")))))
df2 <- data.frame(Date = rep(df$Date,times = 4),
                  sig = c(df$sig, df$sigTMB, df$rollsd, df$EWMA),
                  type = factor(rep(c("Nonstochastic", "GARCH","MVAR", "EWMA"), each = nrow(df))),
                  decade = rep(df$decade,4))
# ----------------------------
# -- Volatilities by decade --
# ----------------------------
ggplot(transform(df2,#[df$decade != "1970s",], 
                 decade = factor(decade, levels = c("1970s","1980s","1990s","2000s","2010s")),
                 type = factor(type, levels = c("EWMA", "MVAR", "GARCH", "Nonstochastic"))),
       aes(x=Date, y= sig^2))+geom_line(aes(col = type), lwd = .5)+ 
  scale_color_manual(values = c("green","red","blue", "magenta"), name = "")+
  guides(color = guide_legend(override.aes = list(size = 1.7)))+
  #geom_line(aes(y = sigTMB), col = "magenta")+
  #geom_line(aes(y = sig), col = "red")+
  facet_wrap(~decade, scales = "free_x", ncol = 1, strip.position = "right")+mytheme+
  theme(#strip.placement = "inside",
    legend.position = "top",
    plot.margin=unit(x=c(0,0,0,0),units="mm"),
    legend.box.background = element_rect(fill = "skyblue", colour = "skyblue"),
    legend.text = element_text(size = 12, colour = "white"),
    legend.key.width = unit(1,"cm"),
    legend.margin=margin(-1,0,0,0,"pt"),
    #legend.box.margin=margin(0,0,0,0),
    strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
    strip.text = element_text(colour = "white", size = 12))+
  xlab("")+ylab("")
ggsave("Figures/9_volatility_by_decade_kappat.pdf",
       width = 8, height = 8.4)

ggsave("../../Submission/Thesis_version/9_volatility_by_decade_kappat.pdf",
       width = 8, height = 10)


# ----------------------
# -- Series by decade --
# ----------------------
ggplot(transform(df, 
                 decade = factor(decade, levels = c("1970s","1980s","1990s","2000s","2010s"))), 
       aes(x=Date, y = temp))+geom_line(col = "blue")+
  facet_wrap(~decade, scales = "free_x", ncol = 1, strip.position = "right")+mytheme+
  theme(
    strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
    strip.text = element_text(colour = "white", size = 12))+
  xlab("")+ylab("")
ggsave("Figures/4_Series_by_decade_kappat.pdf",
       width = 7, height = 7.5)

# ------------------------------------
# -- Mean model residuals by decade --
# ------------------------------------

ggplot(transform(df,#[df$decade != "1970s",], 
                 decade = factor(decade, levels = c("1970s","1980s","1990s","2000s","2010s"))), 
       aes(x=Date, y = x))+geom_line(col = 4)+
  #scale_x_discrete(expand = c(0.01,0))+scale_y_continuous(expand = c(0,0))+
  facet_wrap(~decade, scales = "free_x", ncol = 1, strip.position = "right")+mytheme+
  theme(strip.placement = "bottom",
        strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(colour = "white", size = 12))+
  xlab("")+ylab("")
ggsave("Figures/5_Mean_model_residual_series_by_decade_kappat.pdf",
       width = 7, height = 7.5)




# ------------------------------
# -- Residuals analysis plots --
# ------------------------------
#
# 1. QQ-normality
#
df.Z <- data.frame(
  Z = rep(data$x,4)/c(data$sig,data$sigTMB, data$rollsd, data$EWMA),
  type = factor(rep(c("Nonstochastic", "GARCH", "MVAR", "EWMA"), each = length(data$x)))
  )
MVAR.df <- MASS::fitdistr(df.Z$Z[df.Z$type=="MVAR"], fGarch::dstd, start = list(nu = 10), sd = 1, mean = 0,lower = 2.1)$estimate
EWMA.df <- MASS::fitdistr(df.Z$Z[df.Z$type=="EWMA"], fGarch::dstd, start = list(nu = 10), sd = 1, mean = 0,lower = 2.1)$estimate
df.Z$df = rep(c(fit$par[1], fitG$par[1], MVAR.df, EWMA.df), each = length(x))
df.Z <- transform(df.Z, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))


z1a<-ggplot(df.Z[df.Z$type == "Nonstochastic", ], aes(sample = Z))+
  geom_qq(col = "skyblue", cex = .5, distribution = fGarch::qstd, dparams =list(nu=fit$par[1]))+
  geom_qq_line(col = "red", lwd = .6, lty = 2, distribution = fGarch::qstd, dparams =list(nu=fit$par[1]))+
  mytheme + xlab(bquote(nu == .(round(fit$par[1],2))))+ylab("Sample")+
  facet_wrap( ~type)+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 14, colour = "white"), 
        plot.margin = margin(l = .8, unit = "cm"))
z1b<-ggplot(df.Z[df.Z$type == "GARCH", ], aes(sample = Z))+
  geom_qq(col = "skyblue", cex = .5, distribution = fGarch::qstd, dparams =list(nu=fitG$par[1]))+
  geom_qq_line(col = "red", lwd = .6, lty = 2, distribution = fGarch::qstd, dparams =list(nu=fitG$par[1]))+
  mytheme +xlab(bquote(nu == .(round(fitG$par[1],2))))+ylab("")+
  facet_wrap( ~type)+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 14, colour = "white"),
        axis.title.y = element_blank(),
        plot.margin = margin())

z1c<-ggplot(df.Z[df.Z$type == "MVAR", ], aes(sample = Z))+
  geom_qq(col = "skyblue", cex = .5, distribution = fGarch::qstd, dparams =list(nu=MVAR.df))+
  geom_qq_line(col = "red", lwd = .6, lty = 2, distribution = fGarch::qstd, dparams =list(nu=MVAR.df))+
  mytheme +xlab(bquote(nu == .(round(MVAR.df,2))))+ylab("")+
  facet_wrap( ~type)+
  scale_x_continuous(breaks = seq(-6,6,3))+
  scale_y_continuous(breaks = seq(-6,6,3))+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 14, colour = "white"),
        axis.title.y = element_blank(),
        plot.margin = margin())

z1d<-ggplot(df.Z[df.Z$type == "EWMA", ], aes(sample = Z))+
  geom_qq(col = "skyblue", cex = .5, distribution = fGarch::qstd, dparams =list(nu=EWMA.df))+
  geom_qq_line(col = "red", lwd = .6, lty = 2, distribution = fGarch::qstd, dparams =list(nu=EWMA.df))+
  mytheme + xlab(bquote(nu == .(round(EWMA.df,2))))+ylab("")+
  scale_x_continuous(breaks = seq(-6,6,3))+
  scale_y_continuous(breaks = seq(-6,6,3))+
  #xlab(expression(paste(plain(sin) * phi, "  and  ",
  #                      plain(cos) * phi)))
  facet_wrap( ~type)+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 14, colour = "white"),axis.title.y = element_blank(),
        plot.margin = margin())


(z1<-ggarrange(z1a,z1b,z1c,z1d, ncol = 4, widths = c(1.3, 1,1,1)))
#ggsave("Figures/10a_Z_qqnorm_by_method.pdf",   width = 8, height = 2.6)


#
# 2. ACF of Z
#
acf.df <- data.frame(ACF = c(acf(df.Z$Z[df.Z$type == "Nonstochastic"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "GARCH"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "MVAR"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "EWMA"], lag.max = 400, plot= FALSE)$acf),
                     lag = rep(0:400, 4),
                     type = rep(c("Nonstochastic","GARCH","MVAR","EWMA"),each = 401))
acf.df <- transform(acf.df, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))
(z2<-ggplot(acf.df[acf.df$lag !=0,], aes(y = ACF, x = lag))+geom_line(col = "skyblue")+#geom_segment(aes(xend = lag, yend = 0))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept = qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept = qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  mytheme+xlab("Lag")+ylab(expression(ACF~(Z)))+
  theme(strip.background = element_blank(),#element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_blank())+#element_text(size = 12, colour = "white"))+
  facet_wrap( ~type, ncol = 4))
#ggsave("Figures/10b_Z_acf_by_method.pdf",       width = 8, height = 2.4)

#
# 3. ACF of Z squared
#
acf.df <- data.frame(ACF = c(acf(df.Z$Z[df.Z$type == "Nonstochastic"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "GARCH"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "MVAR"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "EWMA"]^2, lag.max = 400, plot= FALSE)$acf),
                     lag = rep(0:400, 4),
                     type = rep(c("Nonstochastic","GARCH","MVAR","EWMA"),each = 401))
acf.df <- transform(acf.df, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))

(z3<-ggplot(acf.df[acf.df$lag !=0,], aes(y = ACF, x = lag))+
    geom_line(col = "skyblue")+
  #geom_segment(aes(xend = lag, yend = 0))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept = qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept = qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
    scale_y_continuous(breaks = seq(-0.06,0.06,0.02))+
  mytheme+xlab("Lag")+ylab(expression(ACF~(Z^2)))+
  theme(strip.background = element_blank(),#element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_blank(),
        strip.placement = "outside")+
        #axis.title.x = element_text(vjust = 8))+#element_text(size = 12, colour = "white"))+
  facet_wrap( ~type, ncol = 4, strip.position = "bottom"))
#ggsave("Figures/10c_Z2_acf_by_method.pdf",   width = 8, height = 2.4)
z1<-ggarrange(z1a,z1b,z1c,z1d, ncol = 4, widths = c(1.3, 1,1,1))
ggarrange(z1,z2,z3,ncol = 1,
                  align = "v", labels = c("A","B","C"))

ggsave("Figures/10_Z_acf_by_method_kappat.pdf",
       width = 8, height = 3*2.4)
ggsave("../../Submission/Thesis_version/10_Z_acf_by_method_kappat.pdf",
       width = 8, height = 10)

#
# 4. Coverage table
#
L <- cbind(aggregate(df.Z$Z, list(df.Z$type), function(k)mean(abs(k)<=qnorm(p=1-0.05/2))),
           aggregate(df.Z$Z, list(df.Z$type), function(k)mean(dnorm(x=k, log = TRUE)))$x)

Lt <- 100*
  c(length(which(abs(df.Z$Z[df.Z$type == "Nonstochastic"])<fGarch::qstd(.975, nu = fit$par[1])))/length(data$x),
        length(which(abs(df.Z$Z[df.Z$type == "GARCH"])<fGarch::qstd(.975, nu = fitG$par[1])))/length(data$x),
        length(which(abs(df.Z$Z[df.Z$type == "MVAR"])<fGarch::qstd(.975, nu = MVAR.df)))/length(data$x),
        length(which(abs(df.Z$Z[df.Z$type == "EWMA"])<fGarch::qstd(.975, nu = EWMA.df)))/length(data$x))
L <- data.frame(
  Cover = Lt
)
rownames(L) <- levels(df.Z$type)

print(xtable(t(L), digits = 2),
      file = "Tex/10d_Coverage_table_kappat.tex",
      sanitize.text.function = function(k)k,
      include.colnames = FALSE)


# ------------------------------------------------------ 
# -- Empirical variance by year and month : --
# ------------------------------------------------------
Sys.setlocale("LC_TIME", "english") 
sd.df <- data.frame(
  x=data$x,
  mth = month(data$Date, abbr = FALSE, label = TRUE),
  year = year(data$Date)
  )
year.and.month <- ddply(sd.df, .(mth, year), summarize, Var=var(x))
summary(lm(Var~year, year.and.month[year.and.month$mth == "May",]))
summary(lm(Var~year, year.and.month[year.and.month$mth == "June",]))
summary(lm(Var~year, year.and.month[year.and.month$mth == "July",]))
summary(lm(Var~year, year.and.month[year.and.month$mth == "August",]))
summary(lm(Var~year, year.and.month[year.and.month$mth == "September",]))

ggplot(year.and.month, aes(x = year, y = Var))+geom_line(col = "skyblue")+
  facet_wrap(~mth, ncol = 3)+geom_smooth(method='lm', formula= y~x, se=FALSE, col = "red", lwd = .4)+
  mytheme+theme(strip.background = element_rect(fill = "skyblue", color = "skyblue"),
                strip.text = element_text(color = "white", size = 12))+
  xlab("Year")+ylab("Variance")

ggsave("Figures/6_variance_by_year_and_month.pdf", width = 8, height = 6)

# ---------------------------------
# -- Empirical Variance by year: --
# ---------------------------------
only.year <- ddply(sd.df, .(year), summarize, Var=var(x))
lm.year<-lm(formula = Var~year, data = only.year)$coef
ggplot(only.year, aes(x = year, y = Var))+geom_point(col = "skyblue")+
  geom_abline(aes(intercept= lm.year[1], slope = lm.year[2]), lwd =.5, col = 2)+
  mytheme+theme(axis.text= element_text(size = 12))+
  xlab("Time")+ylab("Variance")

ggsave("Figures/7_variance_by_year.pdf", width = 6, height = 2.5)

options(warn = 0)


# ------------------------ #
# -- Figure of kappa(t) -- #
# ------------------------ #
Sys.setlocale("LC_TIME", "english") 
NS.kappa_t = (1+cos(2*pi*((1:365)-h)/365))/2*fit$par[3]
G.kappa_t = (1+cos(2*pi*((1:365)-h)/365))/2*fitG$par[5]
kappa.df <- data.frame(
  date = as.Date(rep(seq(as.Date("1979-01-01"), as.Date("1979-12-31"), 1), 2)),
  kappa = c(NS.kappa_t, G.kappa_t),#, lincoefs), 
  type = rep(c("Nonstochastic", "GARCH"#, "movingtrend"
               ), each = length(NS.kappa_t)) 
)

ggplot(kappa.df, aes(x = date, y = kappa))+
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey80")+
    geom_line(aes(col = type))+
  scale_color_manual(values = c("brown1", "blue", "grey50"))+mytheme+
  scale_x_date(breaks = "months",date_labels = "%b", name ="")+
  ylab(expression(kappa[t]))+
  theme(legend.position = "top", 
        legend.title = element_blank())+
  geom_vline(aes(xintercept = as.Date("1979-08-24")), lty =2)+
  geom_vline(aes(xintercept = as.Date("1979-02-22")), lty =2)
  
ggsave("Figures/7_kappa_of_t.pdf", width = 6, height = 3)


#-- COVERAGE FIGURES
df2$degfree <- ifelse(df2$type == "Nonstochastic", fit$par[1], 
                      ifelse(df2$type == "GARCH", fitG$par[1], 
                      ifelse(df2$type=="MVAR",MVAR.df,EWMA.df)))
df2$bands1 <- fGarch::qstd(.975, mean = 0, sd = df2$sig, nu = df2$degfree)
df2$bands2 <- fGarch::qstd(1-.975, mean = 0, sd = df2$sig, nu = df2$degfree)


ggplot(transform(df2,#[df$decade != "1970s",], 
                 decade = factor(decade, levels = c("1970s","1980s","1990s","2000s","2010s")),
                 type = factor(type, levels = c("EWMA", "MVAR", "GARCH", "Nonstochastic"))),
       aes(x=Date))+
  geom_point(data = df, aes(x=Date, y = abs(x)),col = "skyblue", size = .4)+
  geom_line(aes(y = bands1, col = type), lwd = .5)+ 
  #geom_line(aes(y = bands2, col = type), lwd = .5)+ 
  scale_color_manual(values = c("green","red","blue", "magenta"), name = "")+
  guides(color = guide_legend(override.aes = list(size = 1.7)))+

  #geom_line(aes(y = sigTMB), col = "magenta")+
  #geom_line(aes(y = sig), col = "red")+
  facet_wrap(~decade, scales = "free_x", ncol = 1, strip.position = "right")+mytheme+
  theme(#strip.placement = "inside",
    legend.position = "top",
    plot.margin=unit(x=c(0,0,0,0),units="mm"),
    legend.box.background = element_rect(fill = "skyblue", colour = "skyblue"),
    legend.text = element_text(size = 12, colour = "white"),
    legend.key.width = unit(1,"cm"),
    legend.margin=margin(-1,0,0,0,"pt"),
    #legend.box.margin=margin(0,0,0,0),
    strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
    strip.text = element_text(colour = "white", size = 12))+
  xlab("")+ylab("")
ggsave("Figures/9_t_bands_for_X.pdf", width = 8, height = 8)
ggsave("../../Submission/Thesis_version/9_t_bands_for_X.pdf",
       width = 8, height = 10)
