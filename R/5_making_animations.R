# ---------------------------------
# -- Pictures for the ANIMATIONS --
# ---------------------------------

# 1. Day by day
t <- seq(-15,15,.1)
dates <- c(seq(as.Date("1979-01-01"), as.Date("1979-12-31"),1),seq(as.Date("2019-01-01"), as.Date("2019-12-31"),1))
dens.df <- data.frame(
  t = rep(t, 2*length(dates)),
  date = rep(rep(dates,2), each = length(t)),
  sd = rep(c(sig[which(data$Date %in% dates)],data$MCsd[data$Date %in% dates]),each=length(t)),
  nu = rep(rep(c(fit$par[1], fitG$par[1]), each = length(dates)),each = length(t)),
  model = rep(rep(c("Nonstochastic", "GARCH"), each = length(dates)), each = length(t))
)
dens.df$year = year(dens.df$date)
dens.df$shortdate <- paste(month(dens.df$date, abbr = FALSE, label = TRUE),day(dens.df$date)) 
dens.df$y <- fGarch::dstd(x = dens.df$t, mean =0,#ifelse(dens.df$year == 2019,40*lm1$coef["trend"]*365,0),# dens.df$mu, 
                          sd = dens.df$sd, nu = dens.df$nu)
dens.df<-transform(dens.df, model = factor(model, levels = c("Nonstochastic", "GARCH")))

for(j in 1:365){
  Sys.setlocale("LC_TIME", "english") 
  ggplot(dens.df[yday(dens.df$date)==j,], aes(x = t, y = y, col = factor(year)))+
    geom_vline(aes(xintercept = 0), lty = 2, col = "skyblue")+
    geom_line()+facet_grid(cols = vars(model), rows = vars(shortdate), scales = "free")+
    mytheme+
    scale_color_manual(values = c("blue", "brown1","darkorchid4"),name = "")+
    #scale_fill_manual(values = c("skyblue", "red"),name = "")+
    scale_y_continuous(limits = c(0,.53), breaks = seq(0,.5,.1))+
    xlab("")+ylab("Density")+
    
    theme(legend.position = "bottom",
          strip.placement = "outside",
          strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
          strip.text = element_text(size = 14, colour = "white"),
          axis.title.x = element_blank()
    )
  
  ggsave(paste("Animations/uncond_daybyday/", j, ".png",sep=""),  width = 6, height = 4)
  
}

## 2. Year by year
dates <- sort(c(seq(as.Date("1976-02-22"), as.Date("2019-02-22"),"year"),
                seq(as.Date("1976-06-05"), as.Date("2019-06-05"),"year")))
dens.df <- data.frame(
  t = rep(t, 2*length(dates)),
  date = rep(rep(dates, 2), each = length(t)),
  sd = rep(c(data$sig[which(data$Date %in% dates)],data$MCsd[data$Date %in% dates]),each=length(t)),
  nu = rep(rep(c(fit$par[1], fitG$par[1]), each = length(dates)),each = length(t)),
  model = rep(rep(c("Nonstochastic", "GARCH"), each = length(dates)), each = length(t))
)
dens.df$year = year(dens.df$date)
dens.df$shortdate <- paste(month(dens.df$date, abbr = FALSE, label = TRUE),day(dens.df$date)) 
dens.df$y <- fGarch::dstd(x = dens.df$t, mean =0,#ifelse(dens.df$year == 2019,40*lm1$coef["trend"]*365,0),# dens.df$mu, 
                          sd = dens.df$sd, nu = dens.df$nu)
dens.df<-transform(dens.df, model = factor(model, levels = c("Nonstochastic", "GARCH")))

for(j in 1:44){
  ggplot(dens.df[dens.df$year==(1975+j),], aes(x = t, y = y, col = factor(shortdate)))+
    geom_vline(aes(xintercept = 0), lty = 2, col = "skyblue")+
    geom_line()+facet_grid(cols = vars(model),rows = vars(year), scales = "fixed")+
    mytheme+
    scale_color_manual(values = c( "blue","brown1","darkorchid4"),name = "")+
    scale_y_continuous(limits = c(0,.55), breaks = seq(0,.6,0.1))+
    xlab("")+ylab("Density")+
    
    theme(legend.position = "bottom",
          strip.placement = "outside",
          strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
          strip.text = element_text(size = 14, colour = "white")
    )
  
  ggsave(paste("Animations/uncond_yearbyyear/", j, ".png",sep=""),  width = 6, height = 4)
  
}
