################################################################################
#                                                                              #
#       W4451 - Applied Time Series Project With R, summer semester 2023       #
#                                                                              #
#               Problem 1: AR Simulation and Order Selection                   #
#                                                                              #
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Automatic adjustment of the working directory to the the directory of this script file

# Information: Group 09

# Use this file to save your code for solving Problem 1 of the applied project.
# Remove '1234567' in line 19 and instead, replace it with the matriculation 
# number of one of your group members.

# Start at line 24 with your own code.

MatrNr <- 6917548        # Change here '1234567' to the matriculation number of one of your team members

set.seed(MatrNr)           # The seed is set to the given matriculation number

# <--------------- Begin in the next line with your own code  ---------------> #

# Required library
library(forecast)

# Function to simulate AR(2) process and plot the time series
simulate_ar2 <- function(n) {
  # AR coefficients
  ar.coefs <- c(0.6, 0.3)
  mean <- 13.1
  
  # Simulate model
  x <- arima.sim(n = n, list(ar = ar.coefs)) + mean
  
  # Plot the time series
  plot(x, type = "l", main = paste("Simulated AR(2) Time Series (n =", n, ")"), xlab = "Time", ylab = "Value")
  
  return(x)
}

# Function to fit ARMA models and select the best one based on BIC
select_best_arma <- function(x) {
  best.model <- NULL
  best.bic <- Inf
  best.p <- 0
  best.q <- 0
  
  for (p in 0:3) {
    for (q in 0:3) {
      model <- arima(x, order = c(p, 0, q))
      bic <- BIC(model)
      if (bic < best.bic) {
        best.model <- model
        best.bic <- bic
        best.p <- p
        best.q <- q
      }
    }
  }
  
  cat("Best ARMA model (n =", length(x), "): ARMA(", best.p, ",", best.q, ")\n")
  
  return(best.model)
}


# Define n1 and n2
n1 <- 100
n2 <- 4 * n1

# Simulate AR(2) process and select best ARMA model for n1 observations
x1 <- simulate_ar2(n1)
best.model.n1 <- select_best_arma(x1)

# Simulate AR(2) process and select best ARMA model for n2 observations
x2 <- simulate_ar2(n2)
best.model.n2 <- select_best_arma(x2)

# Print the best models' details
best.model.n1
best.model.n2

