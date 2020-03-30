# main analysis:
library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(TMB)
library(forecast)
library(xtable)

mytheme <- theme_minimal() + theme(panel.grid = element_blank(),
                                   axis.line = element_line(),
                                   axis.title = element_text(size = 12),  
                                   axis.ticks = element_line())
#dyn.unload(dynlib("Cpp/AR_GARCH_with_covariates.cpp"))
compile("Cpp/AR_GARCH_with_covariates.cpp")
dyn.load(dynlib("Cpp/AR_GARCH_with_covariates"))

# Mean model
data <- read.table("Data/Svalbard_1975-2019.txt", skip = 40, sep = ";",
                   stringsAsFactors = FALSE, header =TRUE)
data$Date <- as.Date(data$Date, "%d.%m.%Y")
data <- data[-which(month(data$Date)==2 & mday(data$Date)==29),]

length(which(year(data$Date)>= 1976))
k <- seq(16060-nrow(data)+1, 16060)
Mmean <- as.matrix(data.frame(
  trend  = k,
  "cos2"  = cos(2*pi*k/365),
  "sin2"  = sin(2*pi*k/365),
  "cos4"  = cos(4*pi*k/365),
  "sin4"  = sin(4*pi*k/365)
))


lm1 <- arima(x=data$TAM, order = c(3, 0, 0), xreg = Mmean)
lm1
data$x <- as.numeric(lm1$residuals)


x <- data$x 
data$diff = x

MEAN.RES <- data.frame(cbind(lm1$coef, sqrt(diag(lm1$var.coef))))
names(MEAN.RES)<-c("Est", "SD")
rownames(MEAN.RES)<-c("$\\phi_1$", "$\\phi_2$","$\\phi_3$",
                      "$a_0$","$10^4b_0$","$a_1$","$b_1$","$a_2$","$b_2$")
MEAN.RES[5,]<-10^4*MEAN.RES[5,]
MEAN.RES$Est <- paste("$", ifelse(MEAN.RES$Est>0, "\\phantom{-}",""), round(MEAN.RES$Est,3), "$", sep="")
print(xtable(MEAN.RES, digits = 3), sanitize.text.function = function(k)k, file = "Tex/mean_model.tex")

# ---------------------------------------------
# -- Exponentially weighted moving average:  --
# ---------------------------------------------
EWMA.fun <- function(h, y, t){
  gam <- exp(-1/h)
  if(t!=1){
    return((1-gam)/(1-gam^(t-1))*sum(gam^(0:(t-2))*y[seq(t-1,1)]))
  }else
    return(y[1])
}

y <- data$x^2

# Optimizing
if(FALSE){
  objfun <- function(h, y){
    mean((y/sapply(1:length(y), EWMA.fun, y = y, h = h)-1)^2)
  }
  opt <- nlminb(start = 5, objective = objfun, y = x^2, lower = 0.01)
  xi <- opt$par
}else{
  xi <- 20.91554
}
EWMA<- sqrt(sapply(1:length(y), EWMA.fun, y = y, h = xi))
# ---------------------
# -- Moving Variance --
# ---------------------
rollwin <- 30
rollsd<-c(rep(NA,(rollwin)),rollapply(data$x, width = rollwin, FUN = function(x)sqrt(sum(x^2)/(rollwin-1))))
rollsd <- rollsd[-length(rollsd)]
data$rollsd <- rollsd
data$EWMA <- EWMA

# Removing initial values (prior to 1976):
data <- data[year(data$Date) >= 1976,]
rollsd <- data$rollsd
EWMA <- data$EWMA
x<-data$x

# --------------------------
# -- Non-stochastic model --
# --------------------------
n <- length(data$x)
M <- data.frame(intercept = rep(1,n),
                trend = 1:n,
                cos(2*pi*(1:n)/365),
                sin(2*pi*(1:n)/365),
                cos(4*pi*(1:n)/365),
                sin(4*pi*(1:n)/365),
                cos(6*pi*(1:n)/365),
                sin(6*pi*(1:n)/365)
)

M <- as.matrix(M)
parameters <- list(
  omega = var(data$x),
  alpha = 0,
  beta = 0,
  theta = c(-1e-5, 9,4.8,-.5, 2.6, -2, -.5)
)
f = MakeADFun(data=list(x=data$x, M=M[,-1],
                        init = sd(x[which(mday(data$Date)==1 & month(data$Date)==1)])),
              parameters=parameters, silent =TRUE, map = list(alpha = factor(NA), 
                                                              beta = factor(NA)))
fit <- nlminb(f$par,f$fn,f$gr, f$he, 
              lower = c(1e-8,rep(-5, ncol(M)-1))
)
sig <- f$report(fit$par)$sigma
data$sig <- sig
resmat<-cbind(fit$par, 
              sqrt(diag(solve(f$he(fit$par)))))
resmat <- cbind(resmat, resmat[,1]/resmat[,2], 2*pnorm(abs(resmat[,1]/resmat[,2]), lower.tail = FALSE))
rownames(resmat)<-c("Intercept", "trend", "cos2","sin2","cos4","sin4",
                    "cos6","sin6")
colnames(resmat)<-c("Est","SD","Z-score","p-value")
round(resmat,6)
cat("Nonstocastic model\noptimum: \t",fit$objective,"\nMessage: \t", fit$message, "\nAIC: \t\t",(aic <- 2*fit$objective + 2*length(fit$par)),"\n")
cat("-----------------------------------------------------\n")

# -----------------
# -- GARCH model --
# -----------------
Mtmb<-M[,-c(1,ncol(M)-c(0,3))]
parameters <- list(
  omega = var(data$x),
  alpha = c(5e-2),
  beta = 0.86,
  theta = c(-1e-5, 10, 2.5, 1.8, -2.3)#rep(0, ncol(Mtmb))
)
f = MakeADFun(data=list(x=data$x, M=Mtmb,
                        init = sd(x[which(mday(data$Date)==1 & month(data$Date)==1)])),
              parameters=parameters, silent =TRUE)
fitG <- nlminb(f$par,f$fn,f$gr, f$he, 
               lower = c(rep(1e-8, length(which(names(f$par) %in% c("omega","alpha", "beta")))), 
                         rep(-5, length(which(names(f$par) %in% c("theta")))))
               
)

resTMB <- cbind(fitG$par,
                sqrt(diag(solve(f$he(fitG$par)))))
resTMB <-cbind(resTMB,resTMB[,1]/resTMB[,2])
resTMB <- cbind(resTMB,round(2*pnorm(abs(resTMB[,3]), lower.tail = FALSE),5))
rownames(resTMB)<-c("omega", "alpha","beta","trend", "cos2","sin2","sin4","cos6")
sigTMB <- f$report()$sigma
data$sigTMB <- sigTMB
resTMB
cat("GARCH model\noptimum: \t",fitG$objective,"\nMessage: \t", fitG$message, "\nAIC: \t\t",(aic <- 2*fitG$objective + 2*length(fitG$par)),"\n")
cat("-----------------------------------------------------\n")
# --------------------------------
# -- Parameter estaimates table --
# --------------------------------

tab <- data.frame(par = rownames(resmat), 
                  Estimate = resmat[,1],
                  SD = resmat[,2],
                  GARCHpar = rownames(resTMB),
                  EstimateG = resTMB[,1],
                  SDG = resTMB[,2])
tab$Estimate[2]<-tab$Estimate[2]*10^5
tab$SD[2]<-tab$SD[2]*10^5
tab$EstimateG[4] <- tab$EstimateG[4]*10^5
tab$SDG[4] <- tab$SDG[4]*10^5
tab$par <- paste("$", c("\\omega","10^5\\,\\kappa", "a_1","b_1","a_2","b_2","a_3", "b_3"), "$", sep = "")
tab$GARCHpar <- paste("$", c("\\omega","\\alpha","\\beta","10^5\\,\\kappa", "a_1","b_1","b_2","a_3"), "$", sep = "")
tab$Estimate <- paste("$", ifelse(tab$Estimate>0, "\\phantom{-}",""), round(tab$Estimate,3), "$", sep="")
tab$EstimateG <- paste("$", ifelse(tab$EstimateG>0, "\\phantom{-}",""), round(tab$EstimateG,3), "$", sep="")
print(xtable(tab, digits = 3), include.rownames=FALSE, sanitize.text.function = function(x)x,
      file = "Tex/Estimation_results.tex")

MSE <- function(x) mean(((data$x^2/x^2)[!is.na(x)]-1)^2) 
cat("Method         \t  MSE\n")
cat("Nonstochastic: \t ", round(MSE(sig),4),"\n")
cat("GARCH:         \t ",round(MSE(sigTMB),4),"\n")
cat("MVAR:    \t ", round(MSE(rollsd),4),"\n")
cat("EWMA:          \t ", round(MSE(data$EWMA),4),"\n")
