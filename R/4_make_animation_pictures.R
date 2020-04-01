Sys.setlocale("LC_TIME", "english") 
library(plyr)
library(ggplot2)
i80 <- which(data$Date == "1979-01-01")
i15 <- which(data$Date == "2019-01-01")


# Every day of the year: 
for(j in 0:364){
  df5<-data.frame(Date =  format(as.Date(c("1979-01-01","2019-01-01"))+j, format = "%Y, %b %d"), 
                  data[c(i80+j,i15+j), c("sig","sigTMB","EWMA","rollsd")])
  df3 <- melt(df5, id = "Date")
  names(df3)<-c("Date","type","sig")
  df3$type = mapvalues(df3$type, from = levels(df3$type), to = c("Nonstochastic", "GARCH","EWMA", "MVAR"))
  df3$sig <- df3$sig^2
  df3 <- transform(df3, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))
  ggplot(df3, aes(x=type, y = sig))+
    geom_col(aes(group = Date, fill = factor(Date)),position = "dodge")+
    scale_fill_manual(values = c("skyblue", "blue"),name = "" )+mytheme+
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,60))+ylab("Variance")+
    xlab("")+
    theme(legend.position = "top",
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8))+
    geom_text(aes(group = Date, col = factor(Date), label = round(sig,2), y = sig),vjust = -.25,position =position_dodge(width = .9),
              size = 2)+
    scale_colour_manual(values = c("skyblue", "blue"),name = "" )
  ggsave(paste("Animations/Var1980-2015/",j+1,".png",sep=""),
         width = 4, height = 4)
  
}


# Every year of the day
for(j in 0:43){
  dtemp <- as.Date(c(paste(1976+j,"-02-20",sep = ""),
               paste(1976+j,"-06-05",sep = "")))
  df5<-data.frame(Date = format(as.Date(dtemp), format = "%Y, %b %d"),
                  data[which(data$Date %in% dtemp), c("sig","sigTMB","EWMA","rollsd")])
  
  df3 <- melt(df5, id = "Date")
  names(df3)<-c("Date","type","sig")
  df3$sig = df3$sig^2
  df3$type = mapvalues(df3$type, from = levels(df3$type), 
                       to = c("Nonstochastic", "GARCH","EWMA", "MVAR"))
  df3$Date = factor(df3$Date,levels = df3$Date[1:2])
  df3 <- transform(df3, type = factor(type, levels = c("Nonstochastic", "GARCH", "MVAR", "EWMA")))
    ggplot(data = df3,
         aes(x = type, y = sig))+
    geom_col(aes(group = Date, fill = Date),position = "dodge")+
    scale_fill_manual(values = c("skyblue", "blue"),name = "" )+mytheme+
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,70))+ylab("Variance")+
    xlab("")+
    geom_text(aes(group = Date, col = Date, label = round(sig,2), y = sig),
              vjust = -.25,
              position = position_dodge(width = .9),
              size = 2)+
    scale_colour_manual(values = c("skyblue", "blue"),name = "" )+
    theme(legend.position = "top",
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8))
  ggsave(paste("Animations/Var_by_year/",j+1,".png",sep=""),
         width = 4, height = 4)
  
}


