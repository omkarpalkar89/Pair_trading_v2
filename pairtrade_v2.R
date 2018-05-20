rm(list=ls())
library(stringr)
library(TTR)
library(curl)
library("quantmod")
library("urca")

working_directory = "E:/Work/pair trading"
setwd(working_directory)


# Use getSymbols to import the data
HDFC <- na.omit(getSymbols('HDFC.NS', src="yahoo", from = '2017-01-01',
                           to = '2018-05-02', adjust = T, auto.assign = FALSE))

HDFCBANK <- na.omit(getSymbols('HDFCBANK.NS', src="yahoo", from = '2017-01-01',
                           to = '2018-05-02', adjust = T, auto.assign = FALSE))

#Sys.setenv(TZ = "UTC")

HDFC_df <- data.frame(date=index(HDFC), coredata(HDFC))
HDFCBANK_df <- data.frame(date=index(HDFCBANK), coredata(HDFCBANK))

jotest=ca.jo(data.frame(HDFC_df$HDFC.NS.Adjusted,HDFCBANK_df$HDFCBANK.NS.Adjusted), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)




# Rolling window of trading days
window_length <- 10
# Time range
start_date <- "2017-01-01"
end_date <- "2018-01-31"
range <- paste(start_date, "::",end_date, sep = "")


# Our stock pair
x <- HDFC[range, 6]
y <- HDFCBANK[range, 6]


dF <- cbind(x, y)

names(dF) <- c("x", "y")

# Function that we will use to calculate betas
run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data = as.data.frame(dF))))
}
rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression,
            by.column = FALSE, align = "right")
}


betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x


# returns <- diff(dF) / dF
# return_beta <- rolling_beta(returns, 10)
# 
# data$spreadR <- diff(data$y) / data$y -
#   return_beta * diff(data$x) / data$x


thresholdup <- mean(data$spread,na.rm = TRUE)+1.5*sd(data$spread, na.rm = TRUE)


thresholddown <- mean(data$spread,na.rm = TRUE)-1.5*sd(data$spread, na.rm = TRUE)

plot(data$spread, main = "HDFC vs. HDFCBANK In-Sample",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = thresholdup, lty = 2)
abline(h = thresholddown, lty = 2)




# Construct the out of sample spread
# Keep the same 10 day rolling window
window_length <- 10
# Time range
start_date <- "2018-02-01"
end_date <- "2018-05-02"
range <- paste(start_date, "::",
               end_date, sep = "")
# Our stock pair
x <- HDFC[range, 6]
y <- HDFCBANK[range, 6]
# Bind these together into a matrix
dF <- cbind(x, y)
colnames(dF) <- c("x", "y")
#returns <- diff(dF) / dF

# Calculate the out of sample rolling beta
beta_out_of_sample <- rolling_beta(diff(dF), 10)
#beta_out_of_sample <- rolling_beta(returns, 10)


# Buy and sell threshold
data_out <- merge(beta_out_of_sample, dF)
colnames(data_out) <- c("beta_out_of_sample","x", "y")
data_out$spread <- data_out$y -
  lag(beta_out_of_sample, 1) * data_out$x

# data_out$spreadR <- (diff(data_out$y) / data_out$y) -
#   (lag(beta_out_of_sample, 1) * diff(data_out$x) / data_out$x)


# Plot the spread with out-sample bands
plot(data_out$spread, main = "HDFC vs. HDFCBANK out of sample",cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = thresholdup, lwd = 2)
abline(h = thresholddown, lwd = 2)



# Generate sell and buy signals
sells <- ifelse(data_out$spread > thresholdup, -1, 0)
buys <- ifelse(data_out$spread < thresholddown, 1, 0)
data_out$signal <- buys + sells


plot(data_out$spread, main = "HDFC vs. HDFCBANK out of sample",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = thresholdup, lty = 2)
abline(h = thresholddown, lty = 2)

# data_out_df <- data.frame(date=index(data_out), coredata(data_out))
# write.csv(data_out_df,'dataout2.csv')

point_type <- rep(NA, nrow(data_out))
buy_index <- which(data_out$signal == -1)
sell_index <- which(data_out$signal == 1)


point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)


num_of_buy_signals <- sum(abs(buys), na.rm = TRUE)
num_of_sell_signals <- sum(abs(sells), na.rm = TRUE)



prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)
qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for(i in 1:length(signal)) {
  if(signal[i] == 1 && position == 0 && spread[i-2]<spread[i-1] &&
     spread[i-1]<spread[i]) {
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1

  }
  if(signal[i] == -1 && position == 0 && spread[i-2]>spread[i-1] &&
     spread[i-1]>spread[i]) {
    # sell the spread initially
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }
  # if(signal[i] == 1 && position == -1) {
  #   # we are short the spread and need to buy
  #   qty_x[i] <- -(round(beta[i] * trade_size) +
  #                   prev_x_qty)
  #   prev_x_qty <- round(beta[i] * trade_size)
  #   qty_y[i] <- 2 * trade_size
  #   position <- 1
  # }
  # if(signal[i] == -1 && position == 1) {
  #   # we are long the spread and need to sell
  #   qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
  #   prev_x_qty <- round(beta[i] * trade_size)
  #   qty_y[i] <- -2 * trade_size
  #   position <- -1
  # }

  }


qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)


data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

# function for computing the equity curve
# compute_equity_curve <- function(qty, price) {
#   cash_buy <- ifelse(sign(qty) == 1,
#                      qty * price, 0)
#   cash_sell <- ifelse(sign(qty) == -1,
#                       -qty * price, 0)
#   position <- cumsum(qty)
#   cumulative_buy <- cumsum(cash_buy)
#   cumulative_sell <- cumsum(cash_sell)
#   equity <- cumulative_sell - cumulative_buy +
#     position * price
#   return(equity)
# }
# 
# 
# # Add the equity curve columns to the data_out table
# data_out$equity_curve_x <- compute_equity_curve(
#   data_out$qty_x, data_out$x)
# data_out$equity_curve_y <- compute_equity_curve(
#   data_out$qty_y, data_out$y)
# 
# 
# plot(data_out$equity_curve_x +
#        data_out$equity_curve_y, type = 'l',
#      main = "HDFC / HDFCBANK spread", ylab = "P&L",
#      cex.main = 0.8,
#      cex.axis = 0.8,
#      cex.lab = 0.8)

