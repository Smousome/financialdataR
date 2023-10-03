################################################################################
#                                                                              #
#           W4451 - Applied project using R, summer semester 2023              #
#                                                                              #
#                Problem 4: The EGARCH and Other GARCH Models                  #
#                                                                              #
################################################################################

# Information: Group 09

# Use this file to save your code for solving Problem 4 of the applied project.

# <--------------- Begin in the next line with your own code  ---------------> #
##loading libraries
library(rugarch)           
library(ggplot2)          
library(zoo)               

#loading in data
data <- read.table("HistoricalData_1686755858557.csv", sep = ",", header = TRUE)
head(data)

n <- length(data[, 1])
data <- data[n:1, ]
head(data)
str(data)

####converting date to default format
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
head(data)

###setting the parameters
close <- zoo(data$Close.Last, order.by = data$Date)
log_close <- log(close)
diff_close <- diff(close)
returns <- diff(log(close))
t <- time(close)
min_t <- min(t)
max_t <- max(t)

####plotting the closing price
plot.closing <- ggplot(data.frame(t = t, xt = close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Netflix closing price") +
  ggtitle(paste0("Daily closing price of Netflix, ", 
                 "Jan 2013 to Dec 2019"))
plot.closing

######### plotting the log returns
plot.returns <- ggplot(data.frame(t = t[-1], xt = returns), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Log-return") +
  ggtitle("Log-returns of Netflix closing price")
plot.returns

#####Creating GARCH specification objects
#### Simple GARCH(1,1) with conditional normal distribution
spec_sGARCH <- ugarchspec(
  variance.model = list(
    model = "sGARCH",           # Simple GARCH
    garchOrder = c(1, 1)        # Orders 1 and 1
  ),
  mean.model = list(
    armaOrder = c(0, 0),        # No ARMA model for the conditional mean
    include.mean = TRUE         # Include a constant mean term
  ),
  distribution.model = "norm"   # Conditional normal distribution
)

#### APARCH(1,1) with conditional t-distribution
spec_apARCH <- ugarchspec(
  variance.model = list(
    model = "apARCH",           # APARCH
    garchOrder = c(1, 1)        # Orders 1 and 1
  ),
  mean.model = list(
    armaOrder = c(0, 0),        # No ARMA model for the conditional mean
    include.mean = TRUE         # Include a constant mean term
  ),
  distribution.model = "norm"   # Conditional normal distribution
)

#### EGARCH(1,1) with conditional normal distribution
spec_eGARCH <- ugarchspec(
  variance.model = list(
    model = "eGARCH",           # APARCH
    garchOrder = c(1, 1)        # Orders 1 and 1
  ),
  mean.model = list(
    armaOrder = c(0, 0),        # No ARMA model for the conditional mean
    include.mean = TRUE         # Include a constant mean term
  ),
  distribution.model = "norm"   # Conditional normal distribution
)

###### fitting the various GARCH-tyoe models
#### Simple GARCH(1,1) with conditional normal distribution 

garch <- ugarchfit(
  spec = spec_sGARCH,         # specification object with information on what model to fit to the data
  data = returns                  # the return series
  
)

#### APARCH(1,1) with Conditional normal distribution
aparch <- ugarchfit(
  spec = spec_apARCH,         # specification object with information on what model to fit to the data
  data = returns                 # the return series
  
)

#### EGARCH(1,1) with conditional normal distribution

egarch <- ugarchfit(
  spec = spec_eGARCH,         # specification object with information on what model to fit to the data
  data = returns                  # the return series
  
)

### results

garch

aparch

egarch

###  obtaining the BIC values

infocriteria(garch)["Bayes", ]        
infocriteria(aparch)["Bayes", ]   
infocriteria(egarch)["Bayes", ]  

### Plotting the estimated conditional standard deviations of the selected model

c_sdev <- aparch@fit$sigma                      # The fitted conditional standard deviations
c_sdev <- zoo(c_sdev, order.by = data$Date[-1])    # Transform into time series object

#### Multivariate time series object with different columns for different
#### univariate time series (Returns and conditonal standard deviations)
TS <- cbind("Log-Return" = returns, "Conditional standard deviation" = c_sdev)

#### Create plot the standard deviation
autoplot.zoo(TS) +
  ggtitle("Log-returns and corresponding fitted conditional standard deviations following the APARCH(1,1)") +
  xlab("Year") +
  facet_free()        # to allow both y-axes to be scaled independently

####
# Put fitted conditional volatilities for the data 
Volatility = aparch@fit$sigma      
# ... and transform that vector once again into a time series object
Volatility <- zoo(Volatility, order.by = data$Date[-1])

plotB5.2c <- autoplot.zoo(merge(returns, Volatility), facet = NULL) +
  ylab("Return and calculated conditional volatility") +
  xlab("Year") +
  ggtitle(paste0(
    "Netflix closing price returns together with the fitted and forecasted ",
    "conditional volatilities"
  )) +
  scale_color_manual(values = c("black", "red")) +
  aes(size = Series) +  
  scale_size_manual(values = c(0.5, 1)) +
  geom_vline(xintercept = as.Date("2007-01-01"), color = "blue")
plotB5.2c

