# Plot the autocorrelation ρ(k) up to nlag = 30 for the coefficient values above ϕ1 = −0.7 and ϕ2 = −0.2
#Given the AR parameters
phi1 <- -0.7  
phi2 <- -0.2  

# Define the maximum lag (30 lags)
n_lags <- 30

rho <- numeric(n_lags)
# FRom two first Yule-Walker equations
#a) ρ(1) + φ1 + φ2ρ(1) = 0
#b) ρ(2) + φ1ρ(1) + φ2  = 0.
rho[1] <- -phi1 / (1 + phi2)
rho[2] <- phi2 - (phi1^2) / (1 + phi2)

# Use the recursive formula to calculate the rest of the autocorrelations
for (k in 3:n_lags) {
  rho[k] <- -phi1 * rho[k-1] - phi2 * rho[k-2]
}
print(rho)
# Plot the autocorrelation function (ACF) 
plot(1:n_lags, rho, type = "h", lwd = 2, col = "blue", 
     main = "Autocorrelation Plot for AR(2) Process", 
     xlab = "Lag", ylab = "Autocorrelation")  


#---------------------------------------
n <- 300
y <- numeric(n)  # Initialize Y_t with zeros
epsilon <- rnorm(n)  # White noise

# Function to plot time series, ACF, and PACF
plotit <- function(y,title = " ") 
{
  layout(rbind(1,2:3))
  plot(y, ylab="Y",xlab="Time",main=title,type="l")
  acf(y, lag.max = 50, lwd = 2, main = "ACF")  
  pacf(y, lag.max = 50, lwd = 2, main = "PACF")  
}
#----------------------------------------
# 2.1. A (1, 0, 0) × (0, 0, 0)12 model with the parameter ϕ1 = 0.6. = (1-0.6B)Y_t=epsilon_t
for (t in 2:n) 
{  
  y[t] <- epsilon[t] + 0.6 * y[t-1] 
}
plotit(y,title="2.1.AR(1) with out seasoanlity")


## ---------------------------------------------------------------------------------------
#2.2. A (0, 0, 0) × (1, 0, 0)12 model with the parameter Φ1 = −0.9
y[1:12] <- epsilon[1:12]
for (t in 13:n) 
{  
  y[t] <- epsilon[t]-0.9 * y[t-12] 
}
plotit(y,title="2.2.Seasonal AR(1) with lag 12")

## ---------------------------------------------------------------------------------------
#2.3. A (1, 0, 0) × (0, 0, 1)12 model with the parameters ϕ1 = 0.9 and Θ1 = −0.7.
for (t in 13:n) 
{  
  y[t] <- epsilon[t] -0.7 * epsilon[t-12]+ 0.9 * y[t-1] 
}
plotit(y,title= "2.3.SARIMA(1,0,0) x (0,0,1)_12")

#---------------------------------------------------------------------------
# 2.4.A (1, 0, 0) × (1, 0, 0)12 model with the parameters ϕ1 = −0.6 and Φ1 = -0.8.
y[1:13] <- epsilon[1:13]
for (t in 14:n) 
{  
  y[t] <- epsilon[t] - 0.6 * y[t-1] - 0.8 * y[t-12] - 0.48 * y[t-13]
}
plotit(y,title="2.4.SARIMA (1, 0, 0) x (1, 0, 0)_12 ")

#-------------------------------------
#2.5. A (0, 0, 1) × (0, 0, 1)12 model with the parameters θ1 = 0.4 and Θ1 = −0.8.
for (t in 14:n) 
{  
  y[t] <- epsilon[t] +0.4 * epsilon[t-1] - 0.8 * epsilon[t-12] - 0.32 * epsilon[t-13]
}
plotit(y,title ="2.5.SARIMA (0, 0, 1) x (0, 0, 1)_12")

#----------------------------------------------------
#2.6. A (0, 0, 1) × (1, 0, 0)12 model with the parameters θ1 = −0.4 and Φ1 = 0.7.
y[1:13] <- epsilon[1:13]
for (t in 13:n) 
{  
  y[t] <- epsilon[t] -0.4 * epsilon[t-1] + 0.7 * y[t-12] 
}
plotit(y,title ="2.6.SARIMA A (0, 0, 1) x (1, 0, 0)12")









