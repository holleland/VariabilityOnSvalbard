library(doParallel)
library(plyr)
library(ggplot2)


if(RUN_BOOT){
  cores <- detectCores()-1
  # -------------------
  # -- Nonstochastic --
  # -------------------
  par <- fit$par
  par[3] <- 0
  
  
  run.boot.det <- function(k, par,Mboot, Date,init ){
    xboot <- fGarch::rstd(nrow(Mboot), sd =sqrt(Mboot%*%par[-1]), nu = par[1])
    parameters <- list(
      v = 10,
      omega = var(xboot),
      alpha = 0,
      beta = 0,
      theta =  c(-1e-4, 10,5,-.4, 2.2, -1.9, -.5))#rep(0, ncol(Mboot)-1))
    f = MakeADFun(data=list(x=xboot, M=Mboot[,-1],
                            init = init),#sd(xboot[which(mday(Date)==1 & month(Date)==1)])),
                  parameters=parameters, silent =TRUE, map = list(alpha = factor(NA), 
                                                                  beta = factor(NA)))
    fit <- nlminb(f$par,f$fn,f$gr, f$he, 
                  lower = c(2.5,#rep(1e-8, length(which(names(f$par) %in% c("omega","alpha", "beta")))), 
                    c(1e-8,rep(-5, ncol(Mboot)-1))),
    )
    return(c(fit$par, sqrt(diag(solve(f$he(fit$par))))))
  }
  #testrun 
  run.boot.det(1,par=par,Mboot = cbind(1,M),data$Date, init =rollsd[1])
  t1 <- Sys.time()
  cl <- makeCluster(cores)
  clusterEvalQ(cl, 
               { library(TMB)
                 library(lubridate)
                 dyn.load(dynlib("Cpp/t_GARCH_with_covariates"))
               }
  )
  RESULTS <- parSapply(cl=cl,X = 1:10000, FUN = run.boot.det, par = par, Mboot = cbind(1,M), Date = data$Date,
                       init = rollsd[1])
  stopCluster(cl)
  t2 <- Sys.time()
  
  boot.time.nonstochastic <- t2-t1
  tobs <- RESULTS[2,]/(RESULTS[nrow(RESULTS)/2+2,2])
  plot(density(tobs), xlim = c(-8,4))
  abline(v=quantile(tobs, prob = c(0.025,0.975)), col =2)
  abline(v=resmat[2,1]/resmat[2,2], col = 4)
  save(RESULTS, 
       file = "Bootstrap_and_MC_results/Bootstrap_estimation_results_from_10000_repeated_samples_from_nonstochastic_model_with_kappa_t.RData")
  
  
  
  # -----------------
  # -- GARCH MODEL --
  # -----------------
  par <- fitG$par
  par[5]<-0
  run.boot.garch <- function(k, par,Mboot, Date, init){
    n <- nrow(Mboot)
    xboot <-sig <- numeric(n)
    #xboot[1]<-rnorm(1, sd = sqrt(par[1]/(1-sum(par[2:3]))))
    for(j in 1:n){
      if(j>=2)
        sig[j] <- (c(1,Mboot[j,]) %*% par[-c(1,3:4)] )*(1-sum(par[3:4]))+
          par[3] * xboot[j-1]^2 + par[4] * sig[j-1]
      xboot[j] <- fGarch::rstd(1, sd = sqrt(sig[j]), nu = par[1])
    }
    parameters <- list(
      v=10,
      omega = var(xboot),
      alpha = 5e-2,
      beta  = 0.86,
      theta = c(-1e-4, 10,3.6, 1.9, -2.3)#rep(0, ncol(Mtmb))
    )
    f = MakeADFun(data=list(x=xboot, M=Mboot,
                            init = init),# sd(xboot[which(mday(Date)==1 & month(Date)==1)])),
                  parameters=parameters, silent =TRUE)
    fit_G <- nlminb(f$par,f$fn,f$gr, f$he, 
                   lower = c(2.5,rep(1e-8, length(which(names(f$par) %in% c("omega","alpha", "beta")))), 
                             rep(-100, length(which(names(f$par) %in% c("theta")))))
    )
    return(c(fit_G$par, sqrt(diag(solve(f$he(fit_G$par))))))
  }
  #test-run
  #options(warn=-1)
  run.boot.garch(1, par = par, Mboot = Mtmb, Date = data$Date, init = rollsd[1])
  #options(warn=0)
  t1 <- Sys.time()
  cl <- makeCluster(cores)
  clusterEvalQ(cl, 
               {library(TMB)
                library(lubridate)
                dyn.load(dynlib("Cpp/t_GARCH_with_covariates"))
               }
  )
  RESULTS.garch <- parSapply(cl=cl,X = 1:10000, FUN = run.boot.garch, par = par,
                             Mboot = Mtmb, Date = data$Date, init = rollsd[1])
  stopCluster(cl)
  t2 <- Sys.time()
  boot.time.garch <- t2-t1
  
  tobs <- RESULTS.garch[5,]/(RESULTS.garch[nrow(RESULTS.garch)/2+5,])
  plot(density(tobs), xlim = c(-4,4))
  abline(v=quantile(tobs, prob = c(0.025,0.975)), col =2)
  abline(v=resTMB[4,1]/resTMB[4,2], col = 4)
  save(RESULTS.garch, 
       file = "Bootstrap_and_MC_results/Bootstrap_estimation_results_from_10000_repeated_samples_from_GARCH_model_with_kappa_t.RData")
}
# -----------------------
# -- Plotting results: --
# -----------------------
load(file = "Bootstrap_and_MC_results/Bootstrap_estimation_results_from_10000_repeated_samples_from_nonstochastic_model_with_kappa_t.RData")
load(file = "Bootstrap_and_MC_results/Bootstrap_estimation_results_from_10000_repeated_samples_from_GARCH_model_with_kappa_t.RData")
Tobs <- data.frame(Tobs = c(resmat[3,1]/resmat[3,2], resTMB[5,1]/resTMB[5,2]),
                   type = c("Nonstochastic", "GARCH"),
                   lab = c("frac(widehat(kappa)[A],sde(widehat(kappa)[A]))",
                           "frac(widehat(kappa)[A],sde(widehat(kappa)[A]))"))
Tobs$x1 <- qnorm(.975) + Tobs$Tobs  
Tobs$x2 <- qnorm(.025) + Tobs$Tobs  

bootres<-data.frame(tobs = c(RESULTS[3,]/RESULTS[nrow(RESULTS)/2+3,],
                             RESULTS.garch[5,]/RESULTS.garch[nrow(RESULTS.garch)/2+5,]),
                    type = c(rep("Nonstochastic",ncol(RESULTS)),
                             rep("GARCH", ncol(RESULTS.garch))))

quants <- ddply(bootres, .(type), summarize, q25 = quantile(tobs, .025), q75 = quantile(tobs,.975))

bootres <- transform(bootres, type = factor(type, levels = c("Nonstochastic", "GARCH")))
ggplot(bootres, aes(x = tobs))+geom_density(fill = "skyblue", alpha = .5)+
  geom_vline(data = quants,
             aes(xintercept =q25), col = "red", lwd=.8,lty = 2)+
  geom_vline(data = quants,
             aes(xintercept =q75), col = "red", lwd=.8,lty = 2)+
  geom_text(data = quants,
            aes(x=q25+(q75-q25)/2, y = .52, label = "Bootstrap\n 95% CI"), vjust =.9,
            colour = "red", #fontface = "bold",
            size = 4.5)+
  geom_segment(data = Tobs, aes(x = Tobs, xend =Tobs, 
                                y = .2, yend = 0), 
               col = 4, lwd = .6, lty=1,
               arrow = arrow(ends = "last", length = unit(.1, "inches")))+
  geom_text(data = Tobs, 
            aes(x = Tobs, y = .2,
                label = lab), 
            vjust = -.25, hjust = 0.5, parse = TRUE,size = 5,
            col = "blue")+
  geom_text(aes(x = 0, y = .02, label = "H[0]"), parse=TRUE,color = "white", size =4)+
  geom_segment(data=quants,
               aes(x=q25, y = .44,xend = q75,yend =.44),size = .6,
               arrow = arrow(ends = "both", length = unit(0.1,"inches")), 
               col = "red",
               lty=1)+
  scale_x_continuous(expand = c(0,1), labels = seq(-10,5,2.5),breaks =   seq(-10,5,2.5),
                     name = expression(widehat(kappa)[A]/sde(widehat(kappa)[A])))+
  scale_y_continuous(expand = c(0,0), limits = c(0,.54),
                     name = "Density")+
  geom_vline(data= Tobs, aes(xintercept=x1), col = "blue",lty = 2, lwd = 0.6)+
  geom_vline(data= Tobs, aes(xintercept=x2), col = "blue",lty = 2, lwd = 0.6)+
  #geom_vline(data= Tobs, aes(xintercept=x1-Tobs), col = "blue",lty = 2, lwd = 0.6)+
  #geom_vline(data= Tobs, aes(xintercept=x2-Tobs), col = "blue",lty = 2, lwd = 0.6)+
  geom_text(data = Tobs,
            aes(x= Tobs, y = .38, label = "Gaussian\n 95% CI"), vjust =0,
            colour = "blue", #fontface = "bold",
            size = 4.5)+
  geom_segment(data=Tobs,
               aes(x=x1, y = .35,xend = x2,yend =.35),size = .6,
               arrow = arrow(ends = "both", length = unit(0.1,"inches")), 
               col = "blue",
               lty=1)+
  
  mytheme+theme(strip.placement = "outside",
                strip.background = element_rect(fill = "skyblue", colour = "skyblue"),
                strip.text = element_text(colour = "white", size = 14),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))+
  facet_wrap(~type, ncol = 2, scales = "fixed", strip.position = "top")
ggsave("Figures/8_Bootstrap_kappa_denisty_kappat.pdf", width = 8, height = 4)

if(RUN_BOOT){
cat("The nonstochastic bootstrap took ", boot.time.nonstochastic,"\n")
cat("The GARCH bootstrap took ", boot.time.garch,"\n")
}
