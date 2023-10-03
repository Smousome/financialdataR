################################################################################
#                                                                              #
#           W4451 - Applied project using R, summer semester 2023              #
#                                                                              #
#               Problem 2: Semi-ARMA Fitting and Forecasting                   #
#                                                                              #
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Automatic adjustment of the working directory to the the directory of this script file

# Information: Group 09

# Use this file to save your code for solving Problem 2 of the applied project.

# <--------------- Begin in the next line with your own code  ---------------> #

##loading libraries
library(zoo)
library(ggplot2)
library(smoots)
library(forecast)
library(tseries)
library(dplyr)

#loading in data
data <- read.table("IMPGS.csv", sep = ",", header = TRUE)
#checking for missing values
sum(is.na(data))
#checking to see if data meets minimum requirement
str(data)
summary(data)

#####Defining necessary functions to be utilised later
####mae & rmse function
mae <- function(obs, forec) {
  mean(abs(obs - forec), na.rm = TRUE)
}

rmse <- function(obs, forec) {
  sqrt(mean((obs - forec)^2, na.rm = TRUE))
}
####  k-step rolling forecast function for ARMA models
kstep_arma <- function(obj, ts_train, ts_test, k = 1) {
  n_out <- length(ts_test)
  n_in <- length(ts_train)
  fc <- rep(NA, n_out)
  xt <- c(ts_train, ts_test)
  
  for (i in k:n_out) {
    arma <- forecast::Arima(head(xt, n_in + i - k), model = obj)
    fc[[i]] <- forecast::forecast(arma, h = k)$mean[[k]]
  }
  fc
}

### k-step out-of-sample forecasts according to the naive method
kstep_nai_in <- function(ts_train, k = 1) {
  n <- length(ts_train)
  fc <- rep(NA, n)
  for (i in (k + 1):n) {
    fc[[i]] <- ts_train[[i - k]]
  }
  fc
}
kstep_nai <- function(ts_train, ts_test, k = 1) {
  
  n <- length(ts_test)
  fc <- rep(NA, n)
  x <- c(tail(ts_train, 1), ts_test)
  
  for (i in k:n) {
    fc[[i]] <- x[[i - k + 1]]
  }
  fc
  
}

### function for automated ARMA order selection according to BIC
BIC.sel <- function(xt, max_p = 3, max_q = max_p, include.mean = TRUE) {
  bic_mat <- matrix(1000000, ncol = max_q + 1, nrow = max_p + 1)
  n <- length(xt)
  k <- log(n)
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      tryCatch(
        {
          arma <- arima(xt, order = c(p, 0, q), include.mean = include.mean)
          bic_mat[p + 1, q + 1] <- AIC(arma, k = k)
        }, 
        error = function(e1) {
          NULL
        }
      )
    }
  }
  which(bic_mat == min(bic_mat), arr.ind = TRUE) - 1
}



#plotting the initial series
timestamps <- as.yearmon(as.Date(data$DATE))
ts_zoo <- zoo(data$IMPGS, order.by = timestamps)
rate <- as.ts(ts_zoo)

plot_series <- autoplot.zoo(rate) +
  xlab("Year") +
  ylab("Import of Goods & Services (in billions of dollars)") +
  ggtitle(paste0(
    "Quarterly Importation, Jan 1947 to Dec 2019"
  ))
plot_series

#### making comparison
yt <- log(ts_zoo)
zt <- cbind(
  "labour Participation rate" = ts_zoo,
  "Log-rate" = yt
)

plot_series.2 <- autoplot.zoo(zt) +
  facet_free() +
  ylab("Import of Goods & Services (in billions of dollars
       and log of Importation") +
  xlab("Year") +
  ggtitle("Quarterly Importation, Jan 1947 to Dec 2019")
plot_series.2

##SPLITTING DATA

xt <- data$IMPGS
n <- length(xt)    # Number of observations
# Number of test observations
n_te <- trunc(0.05 * n)
# Number of training observations
n_tr <- n - n_te 

# Training data
xt_tr <- head(xt, n_tr)
# Test data
xt_te <- tail(xt, n_te)
# Log-transformed training data
yt_tr <- log(xt_tr)
# Log-transformed test data
yt_te <- log(xt_te)


#### finding the bandwidth
est <- msmooth(yt_tr)
bwidth <- est$b0
bwidth

length(yt_tr) # length of the trainning data


# Convert train_data to a data frame with a "date" column
yt_tr_df <- data.frame(date = seq_along(yt_tr), IMPGS = yt_tr)
yt_tr_df <- yt_tr_df%>% mutate(date = as.Date(date))
ts_zoo_tr <- zoo(yt_tr_df$IMPGS, order.by = yt_tr_df$date)
rate_tr <- as.ts(ts_zoo_tr)

### plotting the time series for the training dataset
plot_tr<- autoplot.zoo(rate_tr) +
  xlab("Year") +
  ylab("Import of Goods & Services (in billions of dollars)") +
  ggtitle(paste0(
    "Quarterly Importation, Jan 1947 to Dec 2019"
  ))
plot_tr

### plotting the the estimated trend and series togeher on training data
trend <- fitted(est)
df <- data.frame(
  t = time(rate_tr),
  trend = trend
)

plot_tr_trend <- plot_tr +
  geom_line(data = df, aes(x = t, y = trend), color = "red", linewidth = 0.8) +
  ggtitle(paste0("The observed series (black) together with the ", 
                 "estimated local linear trend (red) on train data"))

plot_tr_trend


####### detrendend values and residuals
res <- resid(est)
plotB3.2d <- autoplot.zoo(res) +
  xlab("Year") +
  ylab("Residual value") +
  ggtitle("The residual series") +
  geom_hline(yintercept = 0, color = "blue")
plotB3.2d
### detrended series
acfB3.2d <- ggAcf(as.numeric(res)) +
  ggtitle("Correlogram of the detrended series")
acfB3.2d

#### finding the BIC of detrended values
p_max <- q_max <- 3
p <- 0:p_max
q <- 0:q_max
bic <- matrix(NA, nrow = p_max + 1, ncol = q_max + 1)
rownames(bic) <- paste0("p=", p)
colnames(bic) <- paste0("q=", q)
n <- length(res)

for (p0 in p) {
  for (q0 in q) {
    arma <- arima(res, order = c(p0, 0, q0), include.mean = FALSE)
    bic[(p0 + 1), (q0 + 1)] <- AIC(arma, k = log(n))
  }
}

bic #min BIC value

#calling the best model
pq_opt <- unname(which(bic == min(bic), arr.ind = TRUE) - 1)
p_opt <- pq_opt[[1]]
q_opt <- pq_opt[[2]]
p_opt
q_opt
#fitting the min BIC on residual value
arma_opt <- arima(res, order = c(p_opt, 0, q_opt), include.mean = FALSE)
arma_opt

#####fitting the ARMA(1,1) on training data
arma11 <- arima(yt_tr, order = c(1, 0, 1))
arma11


# Retransformed one-step forecasts according to ARMA
fc_arma <- exp(kstep_arma(
  obj = arma11,
  ts_train = yt_tr,
  ts_test = yt_te,
  k = 1
))
# Retransformed one-step forecasts according to Semi-ARMA
trend_fc <- tail(est$ye, 1) + (1:n_te) * diff(tail(est$ye, 2))
fc_semiarma <- exp(kstep_arma(
  obj = arma_opt,
  ts_train = res,
  ts_test = yt_te - trend_fc,
  k = 1
) + trend_fc)
# Naive out-of-sample forecasts
fc_nai <- kstep_nai(
  ts_train = xt_tr,
  ts_test = xt_te,
  k = 1
)
# Naive in-sample one-step forecasts
fc_nai_in <- kstep_nai_in(
  ts_train = xt_tr,
  k = 1
)

# MAE and RMSE values
mae_arma <- mae(xt_te, fc_arma)
mae_semiarma <- mae(xt_te, fc_semiarma)
rmse_arma <- rmse(xt_te, fc_arma)
rmse_semiarma <- rmse(xt_te, fc_semiarma)
mae_nai <- mae(xt_te, fc_nai)
rmse_nai <- rmse(xt_te, fc_nai)
mae_nai_in <- mae(xt_tr, fc_nai_in)
rmse_nai_in <- rmse(xt_tr, fc_nai_in)

c(
  "MASE Naive" = mae_nai / mae_nai_in,
  "RMSSE Naive" = rmse_nai / rmse_nai_in,
  "MASE ARMA" = mae_arma / mae_nai_in,
  "RMSSE ARMA" = rmse_arma / rmse_nai_in,
  "MASE Semi-ARMA" = mae_semiarma / mae_nai_in,
  "RMSSE Semi-ARMA" = rmse_semiarma / rmse_nai_in
)


#### plot fo the one-step forcast test period
mts <- cbind(xt_te, fc_nai, fc_arma, fc_semiarma)
fc_plot1 <- autoplot.zoo(mts, facets = NULL) +
  aes(linetype = Series) +
  xlab("Time") +
  ylab("Billions of pounds") +
  ggtitle("The obtained one-step forecasts for the test period") +
  scale_color_manual(
    name = "Series",
    values = c("black", "red", "deepskyblue", "purple"),
    labels = c(
      "Test observations",
      "Naive forecasts",
      "ARMA forecasts",
      "Semi-ARMA forecasts"
    )
  ) + 
  scale_linetype_manual(
    name = "Series",
    values = 1:4,
    labels = c(
      "Test observations",
      "Naive forecasts",
      "ARMA forecasts",
      "Semi-ARMA forecasts"
    )
  )
fc_plot1

####
invisible(confBounds(est, x = c(time(xt_tr)),
                     xlab = "Year"))
####
# Absolute bandwidth
b_abs <- trunc(est$b0 * n_tr + 0.5)
# Readjusted relative bandwidth for complete data
b_n <- b_abs / n
###
res <- est$res
pq_opt2 <- BIC.sel(res, include.mean = FALSE)
p_o2 <- pq_opt2[[1]]
q_o2 <- pq_opt2[[2]]

arma_opt2 <- arima(res, order = c(p_o2, 0, q_o2), include.mean = FALSE)
arma_opt2

est_n <- gsmooth(yt, b = b_n)
arma_n <- Arima(est_n$res, order = c(p_o2, 0, q_o2), include.mean = FALSE)
arma_n

arma_res <- arma_n$residuals


jarque.bera.test(arma_res)


# for reproducibility
set.seed(123)
# Forecasts for the parametric part
fc_para <- forecast(arma_n, h = 5, bootstrap = TRUE, level = 95)
# Forecasts for the nonparametric part
fc_trend <- tail(est_n$ye, 1) + (1:5) * diff(tail(est_n$ye, 2))

# Forecasts according to the complete model
fc_point <- exp(fc_para$mean + fc_trend)
fc_low <- exp(fc_para$lower + fc_trend)
fc_up <- exp(fc_para$upper + fc_trend)

# Data frame with the results
df <- data.frame(
  Year = c(time(fc_point)),
  fc_point = c(fc_point),
  fc_low = c(fc_low),
  fc_up = c(fc_up)
)
df

# Plot of the original series
# with point and interval forecasts
# at the end
autoplot.zoo(rate) +
  geom_ribbon(
    data = df, 
    aes(x = Year, ymin = fc_low, ymax = fc_up), 
    fill = alpha("red", 0.2)
  ) +
  geom_line(data = df, aes(x = Year, y = fc_point), color = "red") +
  ggtitle("Forecasts for the import series") +
  xlab("Year") +
  ylab("Billions of pounds")

#####
set.seed(123)
# Forecasts for the whole Semi-ARMA model
fc_mc <- exp(modelCast(est_n, p = p_o2, q = q_o2, h = 5,
                       method = "boot"))
fc_mc

