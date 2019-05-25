# Martin McParland - L00143723
# Msc Big Data Analytics Class A
# Data Science - CA4 Predictive Modeling - Health in Ireland
# Submitted 25th May 2019

# The dataset to be used includes unemployment rates and death rates by month 
# for the period 2005 to 2016. 
# The CSV file is read into a data frame.

unemployment_and_deaths <- read.csv("Unemployment and Death rates by Month 2005 - 2016.csv", 
                                    header = TRUE, stringsAsFactors = FALSE)

# View the head and structure of unemployment_and_deaths
head(unemployment_and_deaths)
str(unemployment_and_deaths)

# Rename first column header to Period and convert Period to a Factor
names(unemployment_and_deaths)[1] <- "Period"
unemployment_and_deaths$Period <- factor(unemployment_and_deaths$Period)
str(unemployment_and_deaths)

# The variables unemployment_rate and Deaths is visualised by year 
library(gplots)
par(mfrow = c(1,2)) # divide graph area into 2 cols 
par(mar=c(5,5,2,2)) # set the border sizes for the graph area

plotmeans(unemployment_rate ~ Year, data = unemployment_and_deaths,
          xlab = "Year",
          ylab = "Unemployment Rate",
          main = "Mean Plot with 95% CI")

plotmeans(Deaths ~ Year, data = unemployment_and_deaths,
          xlab = "Year",
          ylab = "Deaths",
          main = "Mean Plot with 95% CI")

# From CA3 - Data Analysis it was determined that the results of the Spearman's test 
# produces a correlation coefficient of -0.14
# This indicates that there is only a weak correlation between unemployment rate
# and death rate. Therefore time series will be used for forecasting Death rates.

# To test the results of the prediction, the last 3 values are removed from the dataset
# These values are: Oct 2016 = 2397, Nov 2016 = 2512, Dec 2016 = 3010 
Deaths <- unemployment_and_deaths$Deaths
test_Deaths <- Deaths[(142:144)]
test_Deaths
Deaths <- Deaths[-(142:144)]
Deaths

# Convert Deaths to a time series object
time_series_Deaths <- ts(Deaths, start = c(2005, 1), frequency = 12 )
time_series_Deaths

# Test the time series object to confirm correct start end and frequency
par(mfrow = c(1,1)) # set graph area to 1 column 
plot(time_series_Deaths)
start(time_series_Deaths)
end(time_series_Deaths)
frequency(time_series_Deaths)

# Plot the data first and smooth it to remove significant error components
# through centered moving averages

install.packages("forecast")
library(forecast)
default_settings <- par(no.readonly = TRUE)
par(mar=c(5,5,2,2)) # set the border sizes for the graph area
par(mfrow = c(2,2)) # divide graph area into 2 cols and 2 rows
y_boundary <- c(min(time_series_Deaths), max(time_series_Deaths))

plot(time_series_Deaths, main = "Raw time series",
     ylim = y_boundary)
# ma() function is used to smooth the Deaths time series
plot(ma(time_series_Deaths, 3), main = "Simple moving averages (k=3)",
     ylim = y_boundary)
plot(ma(time_series_Deaths, 7), main = "Simple moving averages (k=7)",
     ylim = y_boundary)
plot(ma(time_series_Deaths, 15), main = "Simple moving averages (k=15)",
     ylim = y_boundary)
par(default_settings)



# Seasonal decomposition
seasonal_decomposition <- stl(time_series_Deaths, s.window = "period")
plot(seasonal_decomposition)

# components of each observation - in logged time series
seasonal_decomposition$time.series

# convert back to the original metric first
converted_Deaths <- exp(seasonal_decomposition$time.series)
converted_Deaths

# Seasonally adjusted Deaths
seasonally_adjusted_Deaths <- seasadj(seasonal_decomposition)
default_settings <- par(no.readonly = TRUE)
par(mar=c(5,5,2,2)) # set the border sizes for the graph area
par(mfrow = c(1,2)) # divide graph area into 2 cols
plot(time_series_Deaths, main = "Time Series Deaths")
plot(seasonally_adjusted_Deaths, main = "Seasonally Adjusted Deaths")
par(default_settings)

# Examine that the seasonal frequency has been removed
default_settings <- par(no.readonly = TRUE)
par(mar=c(5,5,2,2)) # set the border sizes for the graph area
par(mfrow = c(1,2)) # divide graph area into 2 cols
seasonplot(time_series_Deaths, 12, col = rainbow(12), year.labels = TRUE,
           main = "Seasonal Plot of Deaths")

seasonplot(seasonally_adjusted_Deaths, 12, col = rainbow(12), year.labels = TRUE,
           main = "Seasonal element removed for Deaths")
par(default_settings)

# Assess the presence of a trend in the data
ndiffs(seasonally_adjusted_Deaths)

# Since there is a trend the series is differenced once
# (lag = 1)
diff_seasonal_adjusted_Deaths <- diff(seasonally_adjusted_Deaths, lag = 1)
ndiffs(diff_seasonal_adjusted_Deaths)

# Applying the ADF test to the differenced series suggets that its now stationary
# so we can proceed to the next step
# Null hypothesis of adf test = data needs to be differenced to make it stationary

adf.test(diff_seasonal_adjusted_Deaths)

# ACF and PACF Plots to determine parameters for the ARIMA model
default_settings <- par(no.readonly = TRUE)
par(mar=c(5,5,5,2)) # set the border sizes for the graph area
par(mfrow = c(1,2)) # divide graph area into 2 cols
Acf(diff_seasonal_adjusted_Deaths,
    main = "Autocorrelation plot for differenced Deaths time series")
Pacf(diff_seasonal_adjusted_Deaths,
     main = "Partial Autocorrelation plot for differenced Deaths time series")
par(default_settings)

# Validation of ARIMA model
# The ARIMA(p,d,q) model used consists of 
# d=1 (differenced value), p=1 (ACF plot) and q=3 (PACF plot)
library(forecast)
adj_arima_model <- Arima(time_series_Deaths, order = c(1,1,3))
adj_arima_model

# Accuracy measures through the mean absolute percentage error (MAPE)
# is a measurement of the prediction accuracy
accuracy(adj_arima_model)

# Evaluate the model fit
# qqnorm produces a normal qq plot from the values of y
# qqline adds a theoretical qq plot which passes through the 
# probability quantiles by default the 1st and 3rd quantiles
par(mar=c(5,5,5,2)) # set the border sizes for the graph area
qqnorm(adj_arima_model$residuals, main = "Normal Q-Q Plot - Adjusted ARIMA Model")
qqline(adj_arima_model$residuals)

# Box test function provides a test that correlations are all zero (H0)
# H0 = the autocorrelations are all equal. In this example p>0.05 so fail to reject the null hypothesis
# The Arima model appears to fit the data well
Box.test(adj_arima_model$residuals, type = "Ljung-Box")


# Auto ARIMA model
auto_arima_model <- auto.arima(time_series_Deaths)
auto_arima_model
accuracy(auto_arima_model)
qqnorm(auto_arima_model$residuals, main = "Normal Q-Q Plot - Auto ARIMA Model")
qqline(auto_arima_model$residuals)
Box.test(auto_arima_model$residuals, type = "Ljung-Box")

# Forecast 3 years ahead for the Deaths time series
forecast(adj_arima_model, 3)
plot(forecast(adj_arima_model, 3), xlab = "Year",
     ylab = "Deaths")

forecast(auto_arima_model, 3)
plot(forecast(auto_arima_model, 3), xlab = "Year",
     ylab = "Deaths (thousands)")

