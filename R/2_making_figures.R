# Figures
options(warn=-1) # To ignore warnings of plotting missing values: 
missdates <- seq(as.Date("1970-01-01"),as.Date("1975-12-31"),1)
missdates <- missdates[-which(mday(missdates)==29 & month(missdates)==2)]
df <- data.frame(Date = c(missdates, data$Date), 
                 diff = c(rep(NA,length(missdates)), data$diff),
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
ggplot(transform(df2,#[df$decade != "1970s",], 
                 decade = factor(decade, levels = c("1970s","1980s","1990s","2000s","2010s")),
                 type = factor(type, levels = c("EWMA", "MVAR", "GARCH", "Nonstochastic"))),
       aes(x=Date, y= sig))+geom_line(aes(col = type), lwd = .5)+ 
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
ggsave("Figures/volatility_by_decade.pdf",
       width = 8, height = 8.4)



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
ggsave("Figures/Series_by_decade.pdf",
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
ggsave("C:/Users/sho115/Dropbox/UiB/phd/Articles/AOAS/CD/Figures/Mean_model_residual_series_by_decade.pdf",
       width = 7, height = 7.5)



# --------------
# -- Barplots --
# --------------
Sys.setlocale("LC_TIME", "english") 
library(plyr)
datesx4 <- as.Date(c("1979-02-20","1979-06-05","2019-02-20","2019-06-05"))
df5<-data.frame(Date = datesx4,
                sig = NA, sigTMB = NA, EWMA =NA, rollsd = NA) 
for(i in 1:4){
  df5[i,2] <- df$sig[df$Date == df5$Date[i]]
  df5[i,3] <- df$sigTMB[df$Date == df5$Date[i]]
  df5[i,5] <- df$rollsd[df$Date == df5$Date[i]]
  df5[i,4] <- df$EWMA[df$Date == df5$Date[i]]
}
df3 <- melt(df5, id = "Date")
names(df3)<-c("Date","type","sig")
df3$type = mapvalues(df3$type, from = levels(df3$type), to = c("Nonstochastic", "GARCH","EWMA", "MVAR"))
df3$sig <- df3$sig^2
df3$Date <- factor(format(df3$Date, format = "%Y, %b %d"))
df3$Date <- factor(df3$Date, levels = levels(df3$Date)[c(1,3,2,4)])
ggplot(df3, aes(x=type, y = sig))+
  geom_col(aes(group = Date, fill = factor(Date)),position = "dodge")+
  scale_fill_manual(values = c("skyblue", "blue", "darkorchid1","darkorchid4"),name = "" )+mytheme+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 38))+ylab("Variance")+
  xlab("")+
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.ticks.x = element_blank())+
  geom_text(aes(group = Date, col = factor(Date), label = round(sig,2), y = sig),vjust = -.25,position =position_dodge(width = .9),
            size = 4,
            fontface = "bold")+
  scale_colour_manual(values = c("skyblue", "blue", "darkorchid1","darkorchid4"),name = "" )
ggsave("Figures/Barplots_of_variances.pdf", width = 8, height = 4)

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
df.Z <- transform(df.Z, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))
ggplot(df.Z, aes(sample = Z))+
  geom_qq(col = "skyblue", cex = .5)+
  geom_qq_line(col = "red", lwd = .6, lty = 2)+
  facet_wrap(~type, ncol = 4)+mytheme+xlab("")+ylab("Sample")+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 14, colour = "white"))#+
#xlim(-6,6)
#scale_x_continuous(expand = c(0,0))+
#scale_y_continuous(expand = c(0,0))
ggsave("Figures/Z_qqnorm_by_method.pdf",
       width = 8, height = 2.6)

#
# 2. Density
#
ggplot(df.Z, aes(x = Z))+stat_density(fill = "skyblue", colour = "blue", lwd = .6, alpha= .5)+
  facet_wrap(~factor(type), ncol = 4)+mytheme+xlab("")+ylab("Density")+
  theme(strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_text(size = 12, colour = "white"))+
  stat_function(fun = dnorm, colour = "red", lwd = .6, lty = 1)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,.5))
ggsave("Figures/Z_density_by_method.pdf",
       width = 8, height = 2)

#
# 3. ACF of Z
#
acf.df <- data.frame(ACF = c(acf(df.Z$Z[df.Z$type == "Nonstochastic"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "GARCH"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "MVAR"], lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "EWMA"], lag.max = 400, plot= FALSE)$acf),
                     lag = rep(0:400, 4),
                     type = rep(c("Nonstochastic","GARCH","MVAR","EWMA"),each = 401))
acf.df <- transform(acf.df, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))
ggplot(acf.df[acf.df$lag !=0,], aes(y = ACF, x = lag))+geom_line(col = "skyblue")+#geom_segment(aes(xend = lag, yend = 0))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept = qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept = qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  mytheme+xlab("Lag")+ylab(expression(ACF~(Z)))+
  theme(strip.background = element_blank(),#element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_blank())+#element_text(size = 12, colour = "white"))+
  facet_wrap( ~type, ncol = 4)
ggsave("Figures/Z_acf_by_method.pdf",
       width = 8, height = 2.4)

#
# 4. ACF of Z squared
#
acf.df <- data.frame(ACF = c(acf(df.Z$Z[df.Z$type == "Nonstochastic"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "GARCH"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "MVAR"]^2, lag.max = 400, plot= FALSE)$acf,
                             acf(df.Z$Z[df.Z$type == "EWMA"]^2, lag.max = 400, plot= FALSE)$acf),
                     lag = rep(0:400, 4),
                     type = rep(c("Nonstochastic","GARCH","MVAR","EWMA"),each = 401))
acf.df <- transform(acf.df, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))

ggplot(acf.df[acf.df$lag !=0,], aes(y = ACF, x = lag))+geom_line(col = "skyblue")+
  #geom_segment(aes(xend = lag, yend = 0))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept = qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(.975)/sqrt(length(data$x))), col = "blue", lty = 2)+
  geom_hline(aes(yintercept = qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  geom_hline(aes(yintercept =-qnorm(1-.025/400)/sqrt(length(data$x))), col = "green", lty = 2)+
  mytheme+xlab("Lag")+ylab(expression(ACF~(Z^2)))+
  theme(strip.background = element_blank(),#element_rect(fill = "skyblue", colour = "skyblue"),
        strip.text = element_blank())+#element_text(size = 12, colour = "white"))+
  facet_wrap( ~type, ncol = 4)
ggsave("Figures/Z2_acf_by_method.pdf",
       width = 8, height = 2.4)

#
# 5. Coverage table
#
L <- cbind(aggregate(df.Z$Z, list(df.Z$type), function(k)mean(abs(k)<=qnorm(1-0.05/2))),
           aggregate(df.Z$Z, list(df.Z$type), function(k)mean(dnorm(k, log = TRUE)))$x)
names(L)<- c("Model", "Cover", "L/n")
L$Cover <- paste(round(L$Cover*100, 1),"\\%",sep="")
L$"L/n" <- round(L$"L/n", 3)
print(xtable(t(L), digits = 2),
      file = "Tex/Coverage_table.tex",
      sanitize.text.function = function(k)k,
      include.colnames = FALSE)


options(warn = 0)
