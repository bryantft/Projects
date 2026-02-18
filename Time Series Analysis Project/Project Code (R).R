#################
# PREPROCESSING #
#################

library(readr)
library(dplyr)
library(lubridate)

library(zoo)

df <- read_csv("C:/Users/Z E P H Y R U S/Downloads/nilai_tukar.csv")

df["Terakhir"] <- df["Terakhir"] * 1000

df["Pembukaan"] <- df["Pembukaan"] * 1000

df["Tertinggi"] <- df["Tertinggi"] * 1000

df["Terendah"] <- df["Terendah"] * 1000

df["Vol."] <- NULL

df$`Perubahan%` <- gsub("%", "", df$`Perubahan%`)
df$`Perubahan%` <- gsub(",", ".", df$`Perubahan%`)
df$`Perubahan%` <- as.numeric(df$`Perubahan%`)

df$Tanggal <- as.Date(df$Tanggal, format = "%d/%m/%Y")

df <- df[order(df$Tanggal), ]

df$Time_Index <- seq_len(nrow(df))

df <- df[, c("Time_Index", setdiff(names(df), "Time_Index"))]

df_series <- data.frame(index = df$Time_Index, closing = df$Terakhir, row.names = NULL)

write.csv(df_series, "series_nilai_tukar.csv", row.names = FALSE)

library(ggplot2)

ggplot(df, aes(x = Time_Index, y = Terakhir)) +
  geom_line() +
  labs(x = "Waktu", y = "Closing Price", 
       title = "Plot Runtun Waktu Closing Price") +
  theme_minimal()

ggplot(df, aes(x = Time_Index, y = Pembukaan)) +
  geom_line() +
  labs(x = "Waktu", y = "Opening Price", 
       title = "Plot Runtun Waktu Opening Price") +
  theme_minimal()

ggplot(df, aes(x = Time_Index, y = Tertinggi)) +
  geom_line() +
  labs(x = "Waktu", y = "Highest Price", 
       title = "Plot Runtun Waktu Highest Price") +
  theme_minimal()

ggplot(df, aes(x = Time_Index, y = Terendah)) +
  geom_line() +
  labs(x = "Waktu", y = "Lowest Price", 
       title = "Plot Runtun Waktu Lowest Price") +
  theme_minimal()

ggplot(df, aes(x = Time_Index, y = `Perubahan%`)) +
  geom_line() +
  labs(x = "Waktu", y = "Persentase Perubahan", 
       title = "Plot Persentase Perubahan Closing Price Terhadap Waktu") +
  theme_minimal()

prices <- df$Terakhir

########################
# ARIMA-GARCH MODELING #
########################

library(quantmod)

returns <- diff(log(prices))
returns <- na.omit(returns)

library(tseries)

plot(returns, type = "l", main = "Plot Return Terhadap Waktu", xlab = "Indeks", ylab = "Return", col = "blue")

adf.test(returns)

library(TSA)
acf(returns)
pacf(returns)
eacf(returns)

library(forecast)

arima_fit <- auto.arima(returns)
summary(arima_fit)

library(rugarch)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 2), include.mean = FALSE),
  distribution.model = "ged",
)

garch_fit <- ugarchfit(spec, data = returns)
show(garch_fit)

###############
# FORECASTING #
###############

n_forecast <- 30
garch_forecast <- ugarchforecast(garch_fit, n.ahead = n_forecast)

mean_forecast <- fitted(garch_forecast)
sigma_forecast <- sigma(garch_forecast)
last_price <- tail(prices, 1)

cum_log_return <- cumsum(mean_forecast)

forecasted_prices <- last_price * exp(cum_log_return)

library(ggplot2)

prev_returns <- tail(returns, 50)
plot_returns <- c(prev_returns, as.numeric(mean_forecast))
time_returns <- (length(returns) - 49):(length(returns) + n_forecast)

df_mean <- data.frame(
  Time = time_returns,
  Return = plot_returns,
  Type = c(rep("Historis", 50), rep("Forecast", n_forecast))
)

library(ggplot2)

ggplot(df_mean, aes(x = Time, y = Return, color = Type)) +
  geom_line(size = 1) +
  labs(title = paste0("Forecasting Return Untuk ", n_forecast, " Step Ke Depan"),
       x = "Waktu", y = "Return") +
  theme_minimal()

fitted_sigma <- as.numeric(sigma(garch_fit))
prev_sigma <- tail(fitted_sigma, 50)
plot_sigma <- c(prev_sigma, as.numeric(sigma_forecast))

time_sigma <- (length(fitted_sigma) - 49):(length(fitted_sigma) + n_forecast)

df_sigma <- data.frame(
  Time = time_sigma,
  Volatility = plot_sigma,
  Type = c(rep("Historis", 50), rep("Forecast", n_forecast))
)

ggplot(df_sigma, aes(x = Time, y = Volatility, color = Type)) +
  geom_line(size = 1) +
  labs(title = paste0("Forecasting Volatilitas (σ) Untuk ", n_forecast, " Step Ke Depan"),
       x = "Waktu", y = "Volatilitas (σ)") +
  theme_minimal()

prev_returns <- tail(returns, 50)
df_plot_returns <- data.frame(
  Time = c((length(returns)-49):length(returns), (length(returns)+1):(length(returns)+n_forecast)),
  Return = c(prev_returns, mean_forecast),
  Type = c(rep("Historis", 50), rep("Forecast", n_forecast))
)

ggplot(df_plot_returns, aes(x = Time, y = Return, color = Type)) +
  geom_line() +
  geom_ribbon(data = df_plot_returns[51:80,], 
              aes(ymin = Return - 1.96*sigma_forecast,
                  ymax = Return + 1.96*sigma_forecast), 
              alpha = 0.3, fill = "red") +
  labs(title = "Forecasting Return untuk 30 Step Ke Depan Beserta Interval Kepercayaan",
       x = "Waktu", y = "Return") +
  theme_minimal()

prev_prices <- tail(prices, 50)
all_prices <- c(prev_prices, forecasted_prices)
price_index <- (length(prices)-49):(length(prices)+n_forecast)

df_plot_price <- data.frame(
  Time = price_index,
  Price = all_prices,
  Type = c(rep("Historis", 50), rep("Forecast", n_forecast))
)

ggplot(df_plot_price, aes(x = Time, y = Price, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Forecast Untuk Closing Price Hasil Transformasi Balik dari Return",
       x = "Waktu", y = "Closing Price") +
  theme_minimal()

####################
# ANALISIS RESIDUAL#
####################

library(rugarch)
library(ggplot2)
library(tseries)
library(FinTS)

std_resid <- residuals(garch_fit, standardize = TRUE)
std_resid <- as.numeric(std_resid)
resid_df <- data.frame(Time = 1:length(std_resid), Residual = std_resid)

ggplot(resid_df, aes(x = Time, y = Residual)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Plot Residual Terstandarisasi Terhadap Waktu", x = "Waktu", y = "Residual Terstandarisasi") +
  theme_minimal()

acf_data <- acf(std_resid, plot = FALSE, lag.max = 20)
acf_df <- data.frame(
  Lag = 0:(length(acf_data$acf) - 1),
  ACF = as.numeric(acf_data$acf)
)

ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_hline(yintercept = c(-1.96 / sqrt(length(std_resid)), 1.96 / sqrt(length(std_resid))),
             linetype = "dashed", color = "red") +
  labs(title = "ACF dari Residual Terstandarisasi", x = "Lag", y = "ACF") +
  theme_minimal()

acf_sq <- acf(std_resid^2, plot = FALSE, lag.max = 20)
acf_sq_df <- data.frame(
  Lag = 0:(length(acf_sq$acf) - 1),
  ACF = as.numeric(acf_sq$acf)
)

ggplot(acf_sq_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_hline(yintercept = c(-1.96 / sqrt(length(std_resid)), 1.96 / sqrt(length(std_resid))),
             linetype = "dashed", color = "red") +
  labs(title = "ACF dari Kuadrat dari Residual Terstandarisasi", x = "Lag", y = "ACF") +
  theme_minimal()

hist(std_resid, breaks = 30, probability = TRUE, 
     main = "Histogram Residual Terstandarisasi dengan Fit Distribusi GED", 
     xlab = "Residual", col = "lightgray", border = "white",
     ylim = c(0,0.7))

x_vals <- seq(min(std_resid), max(std_resid), length.out = 500)

nu <- 1.076327
ged_density <- dged(x_vals, mean = 0, sd = 1, nu = nu)

lines(x_vals, ged_density, col = "blue", lwd = 2)
legend("topright", legend = paste("GED( ν =", nu, ")"), col = "blue", lwd = 2)

library(fGarch)
library(ggplot2)

ged_nu <- coef(garch_fit)["shape"]

empirical_q <- sort(std_resid)
n <- length(empirical_q)

theoretical_q <- qged(ppoints(n), mean = 0, sd = 1, nu = ged_nu)

qq_df <- data.frame(Theoretical = theoretical_q, Empirical = empirical_q)

ggplot(qq_df, aes(x = Theoretical, y = Empirical)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "QQ Plot: Residual Terstandarisasi Terhadap Distribusi GED",
       x = "Kuantil Teoritis (GED)", y = "Kuantil Empiris") +
  theme_minimal()

