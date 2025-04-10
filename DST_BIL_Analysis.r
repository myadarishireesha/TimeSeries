# Read training data
D <- read.csv("C:\\Users\\Shireesha myadari\\Desktop\\TimeSeries\\assignment1\\DST_BIL54.csv")
str(D)
summary(D)
cat("Sum of Null values",colSums(is.na(D)),"sum of duplicates",sum(duplicated(D)))

boxplot(D$total, main="Boxplot of Total Vehicles", ylab="Number of Vehicles")

# Preprocess the time variable and convert it to the year in decimal format
D$time <- as.POSIXct(paste0(D$time,"-01"), "%Y-%m-%d", tz="UTC")
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12
D$total <- as.numeric(D$total) / 1E6  # scale the output

# Split into training and test sets
teststart <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]

#----------------------------------------------------------------------------------
#1.1. Make a time variable, x, such that 2018-Jan has x0 = 2018, 2018-Feb has x1 = 2018 + 1/12,2018-Mar has x2 = 2018 + 2/12 etc. and plot the training data versus x.
# Create the sequence from 2018 to 2024 with steps of 1/12
x <- seq(from = 2018, to = 2023+11/12, by = 1/12)
plot(x, Dtrain$total, col = 'blue', pch = 19, main = 'Train Data vs x', xlab = 'Time (x)', ylab = 'Total')

#-------------------------------------------------------------------------------------------------------------
 #2.1.Write up the model on matrix form for the first 3 time points: First on matrix form (as vectors and matrices), then insert the elements in the matrices and vectors and finally, insert the actual
#values of the output vector y and the design matrix X (keep max 3 digits)
# Fit the Linear Trend Model to the first 3 time points
x_3tp <- x[1:3]
y_3tp <- Dtrain$total[1:3]

cat("First 3 time points are =", x_3tp, "\n","First 3 Y values are:",y_3tp)

X <- cbind(1, x_3tp)  # Design matrix
Y <- matrix(y_3tp, ncol = 1)
theta <- solve(t(X) %*% X) %*% t(X) %*% Y  

theta1 <- theta[1]
theta2 <- theta[2]
cat("Manualy calculated Estimated θ1 (Intercept) =", theta1, "\n")
cat("Manually Calculated Estimated θ2 (Slope) =", theta2, "\n")


# ---------3. Linear Trend Model----------------------------------------------------------------

x <- seq(from = 2018, by = 1/12, length.out = nrow(Dtrain))
y <- Dtrain$total
print(y)

# Fit the linear model for all data
model <- lm(y ~ x)
summary(model)

# Extract theta1 and theta2 (intercept and slope)
theta1 <- coef(model)[1]
theta2 <- coef(model)[2]
se_theta1 <- summary(model)$coefficients[1, 2]
se_theta2 <- summary(model)$coefficients[2, 2]

cat("Ordinary Least Squares Estimated Parameters:\n")
cat("θ̂1 =", theta1, "with SE =", se_theta1, "\n")
cat("θ̂2 =", theta2, "with SE =", se_theta2, "\n")


# Create the sequence for the next 12 months (forecast period)
next_months <- seq(from = 2024, by = 1/12, length.out = 12)
print(next_months)

# Predict values for the next 12 months with 90% prediction intervals
forecasted_values <- predict(model, newdata = data.frame(x = next_months), interval = "prediction", level = 0.90)

# Combine forecasted values and intervals into a data frame
forecast_table <- data.frame(
  Month = next_months,
  Forecast = forecasted_values[, 1],
  Lower_90_Interval = forecasted_values[, 2],
  Upper_90_Interval = forecasted_values[, 3]
)

# Print the forecast table
print(forecast_table)

# 3.4 Plot the historical data and forecasted values
plot(x, y, col = 'black', pch = 19, main = "Linear Trend Model with Forecast", 
     xlab = "Time (Year)", ylab = "Total", 
     xlim = c(min(c(x, next_months)), max(c(x, next_months))),
     ylim = c(min(y, forecasted_values[, 2]), max(y, forecasted_values[, 3])))

# Add the fitted regression line
abline(model, col = 'red', lwd = 2)

# Add the forecasted values as a blue line
lines(next_months, forecasted_values[, 1], col = "blue", lwd = 2)

# Add prediction intervals (90% confidence interval) as dashed lines
lines(next_months, forecasted_values[, 2], col = "green", lty = 2, lwd = 2)  # Lower bound
lines(next_months, forecasted_values[, 3], col = "green", lty = 2, lwd = 2)  # Upper bound

legend("topleft", legend = c("Observations", "Fitted Line", "Forecast", "90% Prediction Interval"),
       col = c("black", "red", "blue", "green"), 
       pch = c(19, NA, NA, NA), 
       lty = c(NA, 1, 1, 2), 
       lwd = c(NA, 2, 2, 2))


# 3.6.Investigating Residuals

plot(fitted(model), residuals(model), main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# From the plot model fits before 2021, but after it started deviating from linear trend.
# The prediction interval (green dashed line) is narrow, indicating high confidence interval
#3.6 General Linear model assumptions are not filled.it is skewed
#-------------------------------------------------------------------------------------------
#           4.WLS Model
#--------------------------------------------------------------------------------------------------
-
lambda <- 0.9  
N <- length(Y)

# Compute weights (fixed)
weights <- lambda^((N-1):0)  
Sigma_inv <- diag(weights)  

# Compute WLS estimates
theta_WLS <- solve(t(X) %*% Sigma_inv %*% X) %*% (t(X) %*% Sigma_inv %*% Y)
print(theta_WLS)  # Display estimated parameters

# ---------------------- Forecasting ----------------------
# Forecasting period (Next 12 months)
future_x <- seq(2024, by = 1 / 12, length.out = 12)
X_future <- cbind(1, future_x)

# Predictions using WLS
pred_WLS <- X_future %*% theta_WLS

# ---------------------- OLS Model ----------------------
model_OLS <- lm(y ~ x)
pred_OLS <- predict(model_OLS, newdata = data.frame(x = future_x))

# ---------------------- Compute Variances & Confidence Intervals ----------------------
# Compute residual variance for OLS
residuals_OLS <- Y - X %*% coef(model_OLS)
sigma2_OLS <- sum(residuals_OLS^2) / (length(Y) - ncol(X))

# Compute residual variance for WLS (fixed formula)
residuals_WLS <- Y - X %*% theta_WLS
sigma2_WLS <- sum((residuals_WLS)^2 * weights) / (length(Y) - ncol(X))

# Compute standard errors
se_OLS <- sqrt(diag(sigma2_OLS * solve(t(X) %*% X)))
se_WLS <- sqrt(diag(sigma2_WLS * solve(t(X) %*% Sigma_inv %*% X)))

# Compute 90% confidence intervals
alpha_90 <- qt(0.95, df=N-1)  # 90% confidence level
pred_WLS_upper_90 <- pred_WLS + alpha_90 * se_WLS[2]
pred_WLS_lower_90 <- pred_WLS - alpha_90 * se_WLS[2]

# ---------------------- Plot Results ----------------------

plot(x, y, col = 'black', pch = 19, 
     main = 'OLS vs WLS Forecast with 90% CI (λ = 0.9)',
     xlab = 'Year', ylab = 'Total',
     xlim = range(c(x, future_x)),
     ylim = range(c(y, pred_OLS_lower_90, pred_OLS_upper_90, pred_WLS_lower_90, pred_WLS_upper_90))) # Ensure limits include CI bounds

# Add OLS forecast (red)
lines(future_x, pred_OLS, col = 'red', type = 'o', pch = 19)

# Add WLS forecast (green)
lines(future_x, pred_WLS, col = 'green', type = 'o', pch = 19)

# Add shaded area for WLS 90% CI
polygon(c(future_x, rev(future_x)), c(pred_WLS_lower_90, rev(pred_WLS_upper_90)), 
        col = rgb(0, 1, 0, 0.3), border = NA)

# Add legend
legend("topleft", legend = c("Training Data", "OLS Forecast", "WLS Forecast"),
       col = c("black", "red", "green"), pch = 19, cex = 0.8, inset = 0.02, bg = "white")

#-------------------------------------------------------
# lambda values for WLS
#-------------------------------------------------
lambda_values <- c(0.99, 0.9, 0.8, 0.7, 0.6)  
colors <- c("blue", "green", "orange", "purple", "brown")  

# ---------------------- OLS Model ----------------------
# Plot setup
plot(x, y, col = 'black', pch = 19, 
     main = 'OLS vs WLS Forecast (Multiple λ values)',
     xlab = 'Year', ylab = 'Total',
     xlim = range(c(x, future_x)), 
     ylim = range(c(y, pred_OLS)))  

# Add OLS forecast (red)
lines(future_x, pred_OLS, col = 'red', type = 'o', pch = 19, lwd = 2)

# Loop over different lambda values
for (i in seq_along(lambda_values)) {
  lambda <- lambda_values[i]
  N <- length(Y)

  # Compute weights
  weights <- lambda^((N-1):0)
  Sigma_inv <- diag(weights)

  # Compute WLS estimates
  theta_WLS <- solve(t(X) %*% Sigma_inv %*% X) %*% (t(X) %*% Sigma_inv %*% Y)

  # Forecasting
  X_future <- cbind(1, future_x)
  pred_WLS <- X_future %*% theta_WLS

  # Plot WLS predictions
  lines(future_x, pred_WLS, col = colors[i], type = 'o', pch = 19, lwd = 2)
}

# Add legend
legend("topleft", legend = c("OLS Forecast", paste("WLS λ=", lambda_values)),
       col = c("red", colors), pch = 19, lwd = 2, cex = 0.8, inset = 0.02, bg = "white")

#------------------------------------------------

### 5.1 Update equations
# initialization
n <- length(A[,1])
p <- 2
R0 <- diag(0.1, 2)  
Theta0 <- c(0,0)
x1 <- A[1,]
print(x1)
x2 <- A[2,]
y <- cbind(Dtrain$total)

R1 <- R0 + x1%*%t(x1)
print(R1)
R2 <- R1 + x2%*%t(x2)
print(R2)

theta1 <- Theta0 + solve(R1)%*%x1%*%(y[1]-t(x1)%*%Theta0)
print(theta1)

theta2 <- theta1 + solve(R2)%*%x2%*%(y[2]-t(x2)%*%theta1)
print(theta2)


### 5.2 Computation of estimates
lambda <- 1
R0 <- diag(0.1, 2)  

# we will use the entire dataset (not only the training data from before):
X <- A
y <- cbind(Dtrain$total)

n <- length(A[,1])

# initialise containers for parameter estimates (Theta) and one step predictions:
Theta <- matrix(NA, nrow=n, ncol=p)
OneStepPred <- matrix(NA, nrow=n)

# 1 # very first step:
x1 <- X[1,]

R_1 <- R0 + x1%*%t(x1) # R is a pxp matrix
h_1 <- x1*y[1]    # h is a px1 vector (but R prints it in a row..)

# to estimate theta we need to invert R:
solve(R_1)

Theta[1,] <- solve(R_1) %*% h_1

# 2 # second step - first time to estimate parameters and make prediction
x2 <- X[2,]
R_2 <- lambda*R_1 + x2 %*% t(x2)
h_2 <- lambda*h_1 + x2 * y[2]

solve(R_2)
# R is now invertible (we can estimate p parameters from p observations)

# we estimate theta (for the first time - so not yet using "update" formula):
Theta[2,] <- solve(R_2) %*% h_2

# we predict one step ahead:
OneStepPred[2+1] <- X[2+1,]%*%Theta[2,]

# 3 # third step - first time to use update formula
x3 <- X[3,]
R_3 <- lambda*R_2 + x3 %*% t(x3)
Theta[3,] <- Theta[2,] + solve(R_3) %*% x3 %*% (y[3] - t(x3) %*% Theta[2,])

# we predict one step ahead:
OneStepPred[3+1] <- X[3+1,]%*%Theta[3,]

# next many steps # - update and predict

R <- R_3

for(i in 4:n){
  x <- X[i, ]
  # Update
  R <- lambda*R + x %*% t(x)
  Theta[i, ] <- Theta[i-1, ] + solve(R) %*% x %*% (y[i] - t(x) %*% Theta[i-1, ])
}

# predict
for(i in 4:n-1){
  OneStepPred[i+1] <- X[i+1, ]%*%Theta[i, ]
}

print(Theta[n,])
