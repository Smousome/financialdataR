################################################################################
#                                                                              #
#           W4451 - Applied project using R, summer semester 2023              #
#                                                                              #
#                 Problem 3: GARCH Models and Backtesting                      #
#                                                                              #
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Automatic adjustment of the working directory to the the directory of this script file

# Information: Group 09

# Use this file to save your code for solving Problem 3 of the applied project.

# <--------------- Begin in the next line with your own code  ---------------> #
#loading libraries
library(zoo)
library(ggplot2)
library(ggpubr)
library(forecast)
library(fGarch)
library(ufRisk)

#loading in data
data <- read.table("HistoricalData_1687372755914.csv", sep = ",", header = TRUE)
head(data)

n <- length(data[, 1])
data <- data[n:1, ]
head(data)
str(data)
summary(data)

####converting date to default format
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
head(data)

###setting the parameters
close <- zoo(data$Close.Last, order.by = data$Date)
log_close <- log(close)
diff_close <- diff(close)
ret <- diff(log(close))
t <- time(close)
min_t <- min(t)
max_t <- max(t)

####plotting the closing price
plot.closing <- ggplot(data.frame(t = t, xt = close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("APPLE closing price") +
  ggtitle(paste0("Daily closing price of APPLE Inc., ", 
                 "Jan 2013 to Dec 2019"))
plot.closing

######### plotting the log returns
plot.returns <- ggplot(data.frame(t = t[-1], xt = ret), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Log-return") +
  ggtitle("Log-returns of APPLE closing price")
plot.returns

###summary of all relevent graphs
plot.log <- ggplot(data.frame(t = t, xt = log_close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Log of APPLE Inc. closing price") +
  ggtitle("Logarithm of the APPLE price")
plot.diff <- ggplot(data.frame(t = t[-1], xt = diff_close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Differences of APPLE Inc. closing price") +
  ggtitle("First difference of the APPLE price")


plot.all <- ggarrange(plot.closing, plot.log,
                      plot.returns , plot.diff, ncol = 1, nrow = 4, align = "v")
plot.all


###### plotting the ACFs
acf1 <- ggAcf(as.numeric(ret)) +
  ggtitle("Correlogram of the log-returns")
acf2 <- ggAcf(as.numeric(ret)^2) +
  ggtitle("Correlogram of the squared log-returns")
plot.acf <- ggarrange(acf1, acf2, ncol = 2, nrow = 1)
plot.acf

#### splitting data into training and test data
n_test <- 250
n_ret <- length(ret)
n_train <- n_ret - n_test

Ret_train <- head(ret, n_train)

##modelling of series
aparch11 <- garchFit(~ aparch(1, 1), Ret_train, trace = FALSE, 
                     cond.dist = "std")

aparch11
aparch11@fit$ics[["BIC"]]

garch11 = garchFit(~ garch(1, 1), Ret_train, trace = FALSE,cond.dist = "std")
garch11
garch11@fit$ics[2]

######

fcast <- varcast(as.numeric(close), model = "apARCH", distr = "std", 
                 garchOrder = c(1, 1)) 



Date_test <- tail(data$Date, n_test)
VaR99 <- zoo(fcast$VaR.v, order.by = Date_test)
VaR975 <- zoo(fcast$VaR.e, order.by = Date_test)
ES975 <- zoo(fcast$ES, order.by = Date_test)
Loss_test <- tail(-ret, 250)

violations <- Loss_test > VaR975
Date_violations <- Date_test[violations]
VaR_violations <- VaR975[violations]

df <- data.frame(Date = Date_test, Var975 = VaR975, ES975 = ES975, 
                 Loss = Loss_test)
df_vio <- data.frame(Date = Date_violations, VaR = VaR_violations)

colors <- c("Loss_test" = "grey64", "VaR975" = "red", "ES975" = "green")
labels <- c("Loss_test" = "Losses", "VaR975" = "97.5%-VaR", "ES975" = "97.5%-ES")

plotB5.4d <- ggplot(df, aes(x = Date)) +
  geom_segment(aes(y = Loss, xend = Date, yend = 0, color = "Loss_test")) +  
  geom_line(aes(y = VaR975, color = "VaR975")) +
  geom_line(aes(y = ES975, color = "ES975")) +
  geom_point(data = df_vio, aes(x = Date, y = VaR), color = "blue", size = 3,
             pch = 13) +
  scale_color_manual(values = colors, 
                     labels = labels,
                     name = "Series") +
  xlab("Month and year") +
  ylab("Loss, 97.5%-VaR and 97.5%-ES") +
  ggtitle("The test losses together with 97.5% risk measures")
plotB5.4d

########
covtest(fcast)

########
trafftest(fcast)
