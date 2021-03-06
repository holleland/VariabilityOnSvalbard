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
compile("Cpp/t_GARCH_with_covariates.cpp")
dyn.load(dynlib("Cpp/t_GARCH_with_covariates"))


# Mean model
data <- read.table("Data/1_Svalbard_1975-2019.txt", skip = 40, sep = ";",
                   stringsAsFactors = FALSE, header =TRUE)
data$Date <- as.Date(data$Date, "%d.%m.%Y")
data <- data[-which(month(data$Date)==2 & mday(data$Date)==29),]

length(which(year(data$Date)>= 1976))
k <- seq(16060-nrow(data)+1, 16060)
aic <- numeric(16)
Mmean <- as.matrix(data.frame(
  trend  = k/365,
  "cos2"  = cos(2*pi*k/365),
  "sin2"  = sin(2*pi*k/365),
  "cos4"  = cos(4*pi*k/365),
  "sin4"  = sin(4*pi*k/365)
))
lm1 <- arima(x=data$TAM, order = c(3, 0, 3), xreg = Mmean)
lm1

# -- Adding columns to data: 
data$x <- as.numeric(lm1$residuals)
data$mu <- cbind(1,Mmean) %*% lm1$coef[-(1:6)]
data$meanpred <- fitted(lm1)
x <- data$x 

# -- Summarizing results in table --
MEAN.RES <- data.frame(cbind(lm1$coef, sqrt(diag(lm1$var.coef)),
                             lm1$coef/sqrt(diag(lm1$var.coef)),
                             2*pnorm(abs(lm1$coef/sqrt(diag(lm1$var.coef))), lower.tail =FALSE)))
names(MEAN.RES)<-c("Est", "SD", "T-score", "P-value")
MEAN.RES$Psymbol <- c(gtools::stars.pval(MEAN.RES[,4]))
rownames(MEAN.RES)<-c("$\\phi_1$", "$\\phi_2$","$\\phi_3$","$\\theta_1$","$\\theta_2$","$\\theta_3$",
                      "$\\mu$","$\\gamma$","$a_1$","$b_1$","$a_2$","$b_2$")
#MEAN.RES[8,1:2]<-10^4*MEAN.RES[8,1:2]
#MEAN.RES$Est <- paste("$", ifelse(MEAN.RES$Est>0, "\\phantom{-}",""), round(MEAN.RES$Est,3), "$", sep="")

print(xtable(MEAN.RES[,-(3:4)], digits = 3), sanitize.text.function = function(k)k, file = "Tex/mean_model_review.tex")

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

# -- Optimizing
if(FALSE){ # Set to TRUE if you want to rerun
  objfun <- function(h, y){
    mean((y/sapply(1:length(y), EWMA.fun, y = y, h = h)-1)^2)
  }
  opt <- nlminb(start = 5, objective = objfun, y = x^2, lower = 0.01)
  xi <- opt$par
}else{
  xi <- 21.00866 # Result from running the optimazation
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

ss<-as.data.frame(t(matrix(data$rollsd^2, nrow = 365)))
names(ss) <- 1:365
h <- which.min(lincoefs<-sapply((1:365), function(x){k = 1:44; lm(ss[,x]~k)$coef[2]}))

fun <- function(par, lincoefs){ sum((lincoefs - par[1]*(1+cos(2*pi*((1:365)-par[2])/365)/2))^2)}

par <- optim(par = c(-.2, 50), fun, lincoefs = lincoefs)$par
plot(data$Date[1:365],lincoefs, type = "l", xlab = "", ylab = "Variance", 
     main = "Selection of 22nd February based on MVAR")    
h <- par[2]
lines(data$Date[1:365], par[1]/2*(1+cos(2*pi*((1:365)-par[2])/365)), col = 2)
abline(v=data$Date[round(h)], col = 2, lty = 2)
abline(v=data$Date[round(h)+365/2], col = 2, lty = 2)
n <- length(data$x)
# Covariates: 
M <- data.frame(#intercept = rep(1,n),
                #trendA = (1:n)/365 * .5 * (1-cos(2*pi*((1:n)-h)/365)),
                trendB <- (1:n)/365 * (1+cos(2*pi*((1:n)-h)/365))/2,
                #trend = 1:n,
                cos2 = cos(2*pi*(1:n)/365),
                sin2 = sin(2*pi*(1:n)/365),
                cos4 = cos(4*pi*(1:n)/365),
                sin4 = sin(4*pi*(1:n)/365),
                cos6 = cos(6*pi*(1:n)/365),
                sin6 = sin(6*pi*(1:n)/365)
)
M <- as.matrix(M)

# List of inital parameters: 
parameters <- list(
  v=9,
  omega = var(data$x),
  alpha = 0, # fixed
  beta = 0,  # fixed
  theta = c(-1e-2, 10,5.3,-.4,2.2, -1.9,-.6)
)

# -- Setting up likelihood and finding derivatives: --
f = MakeADFun(data=list(x=data$x, M=M,
                        init = rollsd[1]),#sd(x[which(mday(data$Date)==1 & month(data$Date)==1)])),
              parameters=parameters, silent =TRUE, map = list(alpha = factor(NA), 
                                                              beta = factor(NA)))
# -- Finding MLE: --
fit <- nlminb(f$par,f$fn,f$gr, f$he, 
              lower = c(2.5,1e-8,rep(-10, ncol(M)))
)

# -- Exporting fitted volatilty --
sig <- f$report(fit$par)$sigma  
data$sig <- sig

# -- Summarizing results from estimation --
resmat<-cbind(fit$par, 
              sqrt(diag(solve(f$he(fit$par)))))
resmat <- cbind(resmat, resmat[,1]/resmat[,2], 2*pnorm(abs(resmat[,1]/resmat[,2]), lower.tail = FALSE))
rownames(resmat)<-c("v","Intercept", "kappa", "cos2","sin2","sin4","cos4",
                    "cos6","sin6")
colnames(resmat)<-c("Est","SD","Z-score","p-value")
round(resmat,6)
cat("AIC: ", TMBhelper::TMBAIC(fit), "\tBIC: ",TMBhelper::TMBAIC(fit, p = log(n)),"\n") # BIC
fit
cat("Nonstocastic model\noptimum: \t",fit$objective,"\nMessage: \t", fit$message, "\nAIC: \t\t",TMBhelper::TMBAIC(fit),"\n")
cat("-----------------------------------------------------\n")

# -----------------
# -- GARCH model --
# -----------------

# -- Covariates and initial parameters: --
Mtmb <- data.frame(#intercept = rep(1,n),
 # trendA = (1:n)/365 * .5 * (1-cos(2*pi*((1:n)-h)/365)),
  trendB <- (1:n)/365 * (1+cos(2*pi*((1:n)-h)/365))/2,
  #trend = 1:n,
  cos2 = cos(2*pi*(1:n)/365),
  sin2 = sin(2*pi*(1:n)/365),
 # cos4 = cos(4*pi*(1:n)/365),
  sin4 = sin(4*pi*(1:n)/365),
  cos6 = cos(6*pi*(1:n)/365)
  #sin6 = sin(6*pi*(1:n)/365)
)
Mtmb<- as.matrix(Mtmb)

parameters <- list(
  v =10.4,
  omega = var(data$x),
  alpha = 6e-2,
  beta = 0.84,
  theta = c(-8e-2, 10.47, 3.62, 1.9, -2.27)
)
# -- Setting up likelihood and finding derivatives: --
f = MakeADFun(data=list(x=data$x, M=Mtmb,
                        init = rollsd[1]#sd(x[which(mday(data$Date)==1 & month(data$Date)==1)])
                        ),
              parameters=parameters, silent =TRUE)
# -- Finding MLE: --
fitG <- nlminb(f$par,f$fn,f$gr, f$he, 
               lower = c(2.5,rep(1e-8, length(which(names(f$par) %in% c("omega","alpha", "beta")))), 
                         rep(-5, length(which(names(f$par) %in% c("theta")))))
               
)

# -- Exporting fitted volatilty --
sigTMB <- f$report()$sigma
data$sigTMB <- sigTMB

# -- Summarizing results from estimation --
resTMB <- cbind(fitG$par,
                sqrt(diag(solve(f$he(fitG$par)))))
resTMB <-cbind(resTMB,resTMB[,1]/resTMB[,2])
resTMB <- cbind(resTMB,round(2*pnorm(abs(resTMB[,3]), lower.tail = FALSE),5))
rownames(resTMB)<-c("v","omega", "alpha","beta","trend", "cos2","sin2","sin4","cos6")
resTMB
cat("AIC: ", TMBhelper::TMBAIC(fitG)-75e3, "\tBIC: ",TMBhelper::TMBAIC(fitG, p = log(n))-75e3,"\n") # BIC
fitG$message

cat("GARCH model\noptimum: \t",fitG$objective,"\nMessage: \t", fitG$message, "\nAIC: \t\t",(aic <- 2*fitG$objective + 2*length(fitG$par)),"\n")
cat("-----------------------------------------------------\n")
resTMB[3:5,]<-resTMB[c(5,3:4),]
rownames(resTMB)<-c("v","omega","trend", "alpha","beta", "cos2","sin2","sin4","cos6")
# --------------------------------
# -- Parameter estimates table 1--
# --------------------------------
nrow(resmat)
nrow(resTMB)
tab <- data.frame(par = rownames(resmat), 
                  Estimate = resmat[,1],
                  SD = resmat[,2],
                  psym = c(gtools::stars.pval(resmat[,4])),
                  GARCHpar = rownames(resTMB),
                  EstimateG = resTMB[,1],
                  SDG = resTMB[,2],
                  psymG  = c(gtools::stars.pval(resTMB[,4])))

#tab$Estimate[3]<-tab$Estimate[3]*10^-2
#tab$SD[3]<-tab$SD[3]*10
#tab$EstimateG[5] <- tab$EstimateG[5]*10^-2
#tab$SDG[5] <- tab$SDG[5]*10^-2
tab$par <- paste("$", c("\\nu","\\omega","\\kappa_A", "c_1","d_1","c_2","d_2","c_3", "d_3"), "$", sep = "")
tab$GARCHpar <- paste("$", c("\\nu","\\omega","\\kappa_A","\\alpha","\\beta", "c_1","d_1","d_2","c_3"), "$", sep = "")
#tab$Estimate <- paste("$", ifelse(tab$Estimate>0, "\\phantom{-}",""), round(tab$Estimate,3), "$", sep="")
#tab$EstimateG <- paste("$", ifelse(tab$EstimateG>0, "\\phantom{-}",""), round(tab$EstimateG,3), "$", sep="")
print(xtable(tab, digits = 3), include.rownames=FALSE, sanitize.text.function = function(x)x,
      file = "Tex/Estimation_results_kappa.tex")

MSE <- function(x) mean(((data$x^2/x^2)[!is.na(x)]-1)^2) 
cat("Method         \t  MSE\n")
cat("Nonstochastic: \t ", round(MSE(sig),4),"\n")
cat("GARCH:         \t ",round(MSE(sigTMB),4),"\n")
cat("MVAR:    \t ", round(MSE(rollsd),4),"\n")
cat("EWMA:          \t ", round(MSE(data$EWMA),4),"\n")
MEAN.RES

# -- full table --
FULL <- data.frame(
  Est = MEAN.RES$Est,
  muSD = MEAN.RES$SD,
  muP = as.character(MEAN.RES$Psymbol),
  blank1 = "",
  NSpar = c(tab$par, rep("",3)),
  NSEst = c(tab$Estimate, rep(NA_real_,3)),
  NSSD = c(tab$SD, rep(NA_real_,3)),
  NSP = c(as.character(tab$psym), rep("",3)),
  blank2 = "",
  Gpar = c(tab$GARCHpar, rep("",3)),
  GEst = c(tab$EstimateG, rep(NA_real_,3)),
  GSD = c(tab$SDG, rep(NA_real_,3)), 
  GP = c(as.character(tab$psymG), rep("", 3))
)
rownames(FULL) <- rownames(MEAN.RES)
FULL$muP <- as.character(FULL$muP)
FULL$NSP <- as.character(FULL$NSP)
FULL$GP <- as.character(FULL$GP)
FULL$muP <- ifelse(FULL$muP == "***", "", FULL$muP)
FULL$NSP <- ifelse(FULL$NSP == "***", "", FULL$NSP)
FULL$GP <- ifelse(FULL$GP == "***", "", FULL$GP)
print(xtable(FULL, digits = 3), include.rownames=TRUE, sanitize.text.function = function(x)x,
      file = "Tex/FULL_Estimation_results_review.tex")
resTMB[3:5,]<-resTMB[c(4:5,3),]
rownames(resTMB)<-c("v","omega", "alpha","beta","trend", "cos2","sin2","sin4","cos6")

dyn.unload(dynlib("Cpp/t_GARCH_with_covariates"))
