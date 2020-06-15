library(doParallel)
library(plyr)
library(ggplot2)

if(RUN_MC){
  set.seed(1)
cores <- detectCores()-1
  # -----------------
  # -- GARCH MODEL --
  # -----------------
  par <- fitG$par
  init = data$rollsd[1]
  run.MC.garch <- function(k, par,Mboot, init){
    n <- nrow(Mboot)
    xboot <-sig <- numeric(n)
    sig[1] <- init^2
    for(j in 1:n){
      if(j>=2){
        sig[j] <- (c(1,Mboot[j,]) %*% par[-c(1,3:4)] )*(1-sum(par[3:4]))+
          par[3] * xboot[j-1]^2 + par[4] * sig[j-1]
      }
      xboot[j] <- fGarch::rstd(1, sd = sqrt(sig[j]), nu = par[1])
    }
    sig
    
}
  # -- test-run
  #run.MC.garch(1, par = par, Mboot = Mtmb, init=init)
  t1 <- Sys.time()
  cl <- makeCluster(cores)
  clusterEvalQ(cl, 
               {library(TMB)
                library(lubridate)
                dyn.load(dynlib("Cpp/t_GARCH_with_covariates"))
               }
  )
  MC <- parSapply(cl=cl,X = 1:10000, FUN = run.MC.garch, par = par, Mboot = Mtmb, init=init)
  stopCluster(cl)
  t2 <- Sys.time()
  (MC.time.garch <- t2-t1)
  MCvar <-apply(MC, 1, mean)
  save(MCvar, file = "Bootstrap_and_MC_results/MC_1000_conditional_variances_from_GARCH_model_kappat.RData")
  }
  
  load("Bootstrap_and_MC_results/MC_1000_conditional_variances_from_GARCH_model_kappat.RData")
  
  data$MCsd <- sqrt(MCvar)

  
  
  # ---------------
  # -- Figure 11 --
  # ---------------
  Sys.setlocale("LC_TIME", "english") 
  df11 <- data.frame(
    t = rep(seq(as.Date("1979-01-01"), as.Date("1979-12-31"),1),4),
    sigma = c(data$sig[year(data$Date) %in% c(1979,2019)]^2, 
              MCvar[year(data$Date) %in% c(1979,2019)]),
    type = rep(c("Nonstochastic", "GARCH"), each = 365*2),
    year = rep(c(1979,2019,1979,2019), each = 365)
  )
  df11<-transform(df11, type = factor(type, levels = c("Nonstochastic", "GARCH")))
  ggplot(df11, aes(x = t, y = sigma))+
    geom_line(aes(col = factor(year)), size =.5)+facet_wrap(~type)+mytheme+
    scale_x_date(date_breaks = "months", date_labels = "%b", name = "")+
    scale_y_continuous(name = "Variance")+
    scale_color_manual(values = c( "brown1","blue","darkorchid4"),name = "")+
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.placement = "outside",
          strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
          strip.text = element_text(size = 14, colour = "white")
    )
  ggsave("Figures/11_Unconditional_Variance_plots_kappa.pdf", width = 8, height = 4)
  