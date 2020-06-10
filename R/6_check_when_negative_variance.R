n <- 1e6
M <- data.frame("intercept" = rep(1,n),
  "trend" = 1:n,
  "cos2" = cos(2*pi*(1:n)/365),
  "sin2" = sin(2*pi*(1:n)/365),
  "cos4" = cos(4*pi*(1:n)/365),
  "sin4" = sin(4*pi*(1:n)/365),
  "cos6" = cos(6*pi*(1:n)/365),
  "sin6" = sin(6*pi*(1:n)/365)
  #                cos8 = cos(8*pi*(1:n)/365),
  #sin8 = sin(8*pi*(1:n)/365)
)
M <- as.matrix(M)
print(as.Date("1976-01-01") + which(M %*%  as.numeric(fit$par[-1])<0)[1])
