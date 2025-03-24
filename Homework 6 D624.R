## Homework 6

Do the exercises 9.1, 9.2, 9.3, 9.5, 9.6, 9.7, 9.8 in Hyndman.  Please submit both the Rpubs link as well as your .rmd file.

# 9.1) 

Figure 9.32 shows the ACFs for 36 random numbers, 360 random numbers and 1,000 random numbers.

```{r, echo=FALSE}
knitr::include_graphics("C:/Users/Dell/Downloads/Fig932.png")
```

a) Explain the differences among these figures. Do they all indicate that the data are white noise?
  
  The differences between ACF plots for random numbers with sample sizes of 36, 360, and 1,000 are The variability is greater in smaller samples, resulting in larger spikes in autocorrelation values and less stable estimates. As the sample size increases, the ACF values converge closer to zero, aligning with the characteristics of white noise. Despite these differences, all plots indicate white noise as there are no systematic patterns across the lags, and the values fall within the critical bounds.

b) Why are the critical values at different distances from the mean of zero? Why are the autocorrelations different in each figure when they each refer to white noise?
  
  The critical values are wider for smaller samples due to greater variability, while larger samples lead to narrower bounds and more precise results. Autocorrelation estimates vary because smaller samples produce less stable ACF values, while larger samples reduce randomness and align more closely with the expectations of white noise. In essence, the observed differences stem from the statistical properties of varying sample sizes.

# 9.2) 

A classic example of a non-stationary series are stock prices. Plot the daily closing prices for Amazon stock (contained in gafa_stock), along with the ACF and PACF. Explain how each plot shows that the series is non-stationary and should be differenced.

```{r 92}
library(fpp3)
library(ggplot2)

# Plot the daily closing prices for Amazon stock
ggplot(gafa_stock, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Daily Closing Prices of Amazon Stock", x = "Date", y = "Closing Price (USD)") +
  theme_minimal()

# Plot the ACF of the daily closing prices
gafa_stock %>% 
  ACF(Close) %>%
  autoplot() +
  labs(title = "ACF of Amazon Stock Closing Prices")

# Plot the PACF of the daily closing prices
gafa_stock %>%
  PACF(Close) %>%
  autoplot() +
  labs(title = "PACF of Amazon Stock Closing Prices")
```
1. Daily Closing Prices Plot

This plot shows Amazon’s stock prices over time. The clear upward trend and volatility indicate non-stationarity, as the mean and variance are not constant. The presence of a trend suggests that differencing is required to make the data stationary.                                                                                   
2. ACF Plot

The slow decay and high autocorrelation values signify that the series is dominated by trends or patterns rather than random fluctuations. Differencing the series to remove the trend would stabilize its mean and variance, making it suitable for time series modeling and forecasting.      

3. The PACF plot strongly supports the conclusion that Amazon's stock prices are non-stationary and need differencing. First-order differencing will remove the trend, making the series stationary and suitable for time series modeling.

# 9.3 )

For the following series, find an appropriate Box-Cox transformation and order of differencing in order to obtain stationary data.

a) Turkish GDP from global_economy.
b) Accommodation takings in the state of Tasmania from aus_accommodation.
c) Monthly sales from souvenirs.

```{r 93}
# Load necessary libraries
library(tidyverse)
library(tsibble)
library(fpp3)
library(forecast)

# Reusable function for processing time series
process_series <- function(data, value_col, time_col, lag) {
  # Calculate optimal Box-Cox lambda
  lambda <- data |> 
    features({{ value_col }}, features = guerrero) |> 
    pull(lambda_guerrero)
  
  # Determine number of seasonal differences
  nsdiffs <- data |> 
    features(box_cox({{ value_col }}, lambda), features = unitroot_nsdiffs) |> 
    pull(nsdiffs)
  
  # If nsdiffs is less than or equal to 0, set it to 1 to avoid errors
  nsdiffs <- max(nsdiffs, 1)
  
  # Determine number of first differences
  ndiffs <- data |> 
    features(difference(box_cox({{ value_col }}, lambda), lag = lag, differences = nsdiffs), 
             features = unitroot_ndiffs) |> 
    pull(ndiffs)
  
  # If ndiffs is less than or equal to 0, set it to 1 to avoid errors
  ndiffs <- max(ndiffs, 1)
  
  # Print results
  cat("The optimal Box-Cox lambda value is", lambda,
      "\nThe optimal number of seasonal differences is", nsdiffs,
      "\nThe optimal number of first differences is", ndiffs, "\n")
  
  # Generate transformed data
  return(data |> 
           transmute(
             {{ time_col }},
             Original = {{ value_col }},
             BoxCox_Transformed = box_cox({{ value_col }}, lambda),
             Final_Transformed = difference(box_cox({{ value_col }}, lambda), lag = lag, differences = nsdiffs + ndiffs)
           ))
}

# Turkish GDP
turkey_gdp <- global_economy |> 
  filter(Country == "Turkey")

turkey_gdp_transformed <- process_series(turkey_gdp, GDP, Year, lag = 4)

turkey_gdp_transformed |> 
  pivot_longer(-Year, names_to = "Transformation", values_to = "GDP") |> 
  ggplot(aes(x = Year, y = GDP)) +
  geom_line() +
  facet_grid(vars(Transformation), scales = "free_y") +
  labs(title = "Turkish GDP Transformations", y = "GDP")

# Tasmanian Accommodation
tas_accommodation <- aus_accommodation |> 
  filter(State == "Tasmania")

tas_accommodation_transformed <- process_series(tas_accommodation, Takings, Date, lag = 4)

tas_accommodation_transformed |> 
  pivot_longer(-Date, names_to = "Transformation", values_to = "Takings") |> 
  ggplot(aes(x = Date, y = Takings)) +
  geom_line() +
  facet_grid(vars(Transformation), scales = "free_y") +
  labs(title = "Tasmanian Accommodation Transformations", y = "Takings")

# Souvenir Sales
souvenir_sales_transformed <- process_series(souvenirs, Sales, Month, lag = 12)

souvenir_sales_transformed |> 
  pivot_longer(-Month, names_to = "Transformation", values_to = "Sales") |> 
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Transformation), scales = "free_y") +
  labs(title = "Souvenir Sales Transformations", y = "Sales")

```
Each dataset – Turkish GDP, Tasmanian Accommodation Takings, and Souvenir Sales – initially presents challenges with trends and/or seasonality, rendering them unsuitable for direct modeling. The application of the Box-Cox transformation addresses variance instability across all series, but the persistence of trends and seasonal patterns necessitates further intervention.

Differencing techniques are then employed to eliminate these remaining non-stationary components. For the Turkish GDP, a straightforward differencing removes the upward trend, creating a stable series. In the case of Tasmanian Accommodation Takings and Souvenir Sales, seasonal differencing is applied to account for the recurring quarterly and annual fluctuations, respectively. The combination of Box-Cox transformation and differencing successfully stabilizes both mean and variance, producing datasets that exhibit stationarity.

The transformation from non-stationary to stationary data is essential for reliable time series analysis. By removing trends and seasonal patterns, the underlying structure of the data becomes clearer, allowing for more accurate modeling and forecasting. This process ensures that any insights derived from the data are not confounded by the non-stationary characteristics, leading to more robust and dependable conclusions.

# 9.5 )

For your retail data (from Exercise 7 in Section 2.10), find the appropriate order of differencing (after transformation if necessary) to obtain stationary data.

```{r 95}
# Load necessary libraries
library(tidyverse)
library(tsibble)
library(fpp3)
library(forecast)

# Set seed for reproducibility
set.seed(1234567)

# Select one random series from the aus_retail dataset
myseries <- aus_retail |> 
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

# Display the time series for visualization
myseries |> 
  gg_tsdisplay(Turnover)

# Calculate the optimal Box-Cox transformation lambda value
lambda <- myseries |> 
  features(Turnover, features = guerrero) |> 
  pull(lambda_guerrero)

# Determine the number of seasonal differences
nsdiffs <- myseries |> 
  features(box_cox(Turnover, lambda), features = unitroot_nsdiffs) |> 
  pull(nsdiffs)

# Ensure nsdiffs is a positive integer
nsdiffs <- max(nsdiffs, 1)

# Determine the number of first differences
ndiffs <- myseries |> 
  features(difference(box_cox(Turnover, lambda), lag = 12, differences = nsdiffs), 
           features = unitroot_ndiffs) |> 
  pull(ndiffs)

# Ensure ndiffs is a positive integer
ndiffs <- max(ndiffs, 1)

# Print the results
cat("The optimal Box-Cox lambda value for this series is", lambda,
    "\nThe optimal number of seasonal differences for this series is", nsdiffs,
    "\nThe optimal number of first differences for this series is", ndiffs, "\n")

# Plot the fully transformed and differenced series
myseries |> 
  autoplot(
    difference(
      difference(box_cox(Turnover, lambda), lag = 12, differences = nsdiffs)
    )
  ) +
  labs(title = "Stationary Retail Turnover Series", y = "Transformed Turnover")
```
Time Series of Turnover: The initial turnover data exhibits a distinct upward trend coupled with pronounced seasonal fluctuations, rendering it non-stationary. This is corroborated by the Autocorrelation Function (ACF) plot, which displays high autocorrelation at shorter lags, gradually diminishing, signaling the presence of both trend and seasonality. Additionally, the seasonal plot reveals consistent monthly patterns, with observable peaks and troughs that align with typical retail cycles, further emphasizing the series' non-stationary nature.

Stationary Retail Turnover Series: Following a Box-Cox transformation and the application of seasonal and first differencing, the retail turnover series achieves stationarity, manifesting as fluctuations around a stable mean close to zero. The transformative process successfully eliminates both trends and seasonal patterns, leaving behind only short-term, random variations. Consequently, this processed series is now appropriately conditioned for advanced statistical modeling, including accurate forecasting techniques, as it fulfills the criteria for stationarity required by such models.

# 9.6 ) 
Simulate and plot some data from simple ARIMA models.

a) Use the following R code to generate data from an AR(1) model with  
ϕ1 = 0.6 and σ2 = 1. The process starts with  y1 =0.

y <- numeric(100)
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]
sim <- tsibble(idx = seq_len(100), y = y, index = idx)

b)Produce a time plot for the series. How does the plot change as you change ϕ1?
  
  c) Write your own code to generate data from an MA(1) model with  θ1= 0.6 and σ2 =1.
  
  d) Produce a time plot for the series. How does the plot change as you change  θ1?
  
  e) Generate data from an ARMA(1,1) model with  ϕ1 =0.6, θ1=0.6 and σ2=1.
  
  f) Generate data from an AR(2) model with ϕ1= −0.8,ϕ2 =0.3 and σ2=1.(Note that these parameters will give a non-stationary series.)
  
  g)Graph the latter two series and compare them.

```{r 96}
# AR(1) Model
library(tsibble)
library(ggplot2)

# Simulate AR(1) model
set.seed(123)
y <- numeric(100)
e <- rnorm(100)
for (i in 2:100) {
  y[i] <- 0.6 * y[i - 1] + e[i] # AR(1) formula
}
sim_ar1 <- tsibble(idx = seq_len(100), y = y, index = idx)

# Time plot for AR(1)
ggplot(sim_ar1, aes(x = idx, y = y)) +
  geom_line(color = "blue") +
  labs(title = "AR(1) Model Simulation", x = "Time", y = "Value") +
  theme_minimal()

# MA(1) Model
# Simulate MA(1) model
set.seed(123)
e <- rnorm(101) # Generate errors
y <- numeric(100)
for (i in 2:100) {
  y[i] <- e[i] + 0.6 * e[i - 1] # MA(1) formula
}
sim_ma1 <- tsibble(idx = seq_len(100), y = y, index = idx)

# Time plot for MA(1)
ggplot(sim_ma1, aes(x = idx, y = y)) +
  geom_line(color = "red") +
  labs(title = "MA(1) Model Simulation", x = "Time", y = "Value") +
  theme_minimal()

#ARMA(1,1) Model
# Simulate ARMA(1,1) model
set.seed(123)
y <- numeric(100)
e <- rnorm(100)
for (i in 2:100) {
  y[i] <- 0.6 * y[i - 1] + e[i] + 0.6 * e[i - 1] # ARMA(1,1) formula
}
sim_arma11 <- tsibble(idx = seq_len(100), y = y, index = idx)

# Time plot for ARMA(1,1)
ggplot(sim_arma11, aes(x = idx, y = y)) +
  geom_line(color = "green") +
  labs(title = "ARMA(1,1) Model Simulation", x = "Time", y = "Value") +
  theme_minimal()

#AR(2) Model

# Simulate AR(2) model
set.seed(123)
y <- numeric(100)
e <- rnorm(100)
for (i in 3:100) {
  y[i] <- -0.8 * y[i - 1] + 0.3 * y[i - 2] + e[i] # AR(2) formula
}
sim_ar2 <- tsibble(idx = seq_len(100), y = y, index = idx)

# Time plot for AR(2)
ggplot(sim_ar2, aes(x = idx, y = y)) +
  geom_line(color = "purple") +
  labs(title = "AR(2) Model Simulation", x = "Time", y = "Value") +
  theme_minimal()
```

# 9.7 )

Consider aus_airpassengers, the total number of passengers (in millions) from Australian air carriers for the period 1970-2011.

a. Use ARIMA() to find an appropriate ARIMA model. What model was selected. Check that the residuals look like white noise. Plot forecasts for the next 10 periods.
b. Write the model in terms of the backshift operator.
c. Plot forecasts from an ARIMA(0,1,0) model with drift and compare these to part a.
d. Plot forecasts from an ARIMA(2,1,2) model with drift and compare these to parts a and c. Remove the constant and see what happens.
e. Plot forecasts from an ARIMA(0,2,1) model with a constant. What happens?
  
  ```{r 97}
# Load necessary libraries
library(fpp3)
library(ggplot2)

# Load the aus_airpassengers dataset
data <- aus_airpassengers

# Part (a) Find an appropriate ARIMA model
model_a <- data |> model(ARIMA(Passengers))
report(model_a)

# Check residuals
residuals_a <- augment(model_a)
autoplot(residuals_a, .resid) +
  labs(title = "Residuals of Fitted ARIMA Model", y = "Residuals")

ggAcf(residuals_a$.resid) +
  labs(title = "ACF of Residuals")

# Forecast for the next 10 periods
forecast_a <- model_a |> forecast(h = 10)

# Plot and save forecast (Part a)
p_a <- autoplot(forecast_a, data) +
  labs(title = "ARIMA Model Forecast (Part A)", y = "Passengers")
print(p_a) # Display plot in console
ggsave("forecast_a.png", plot = p_a)

# Part (b) Write the model in backshift operator terms
cat("The ARIMA model (selected in Part A) in terms of the backshift operator is printed by 'report()' output.\n")

# Part (c) ARIMA(0,1,0) with drift
model_c <- data |> model(ARIMA(Passengers ~ drift()))
forecast_c <- model_c |> forecast(h = 10)

# Plot and save forecast (Part c)
p_c <- autoplot(forecast_c, data) +
  labs(title = "ARIMA(0,1,0) Model with Drift", y = "Passengers")
print(p_c) # Display plot in console
ggsave("forecast_c.png", plot = p_c)

# Part (d) ARIMA(2,1,2) with drift
model_d <- data |> model(ARIMA(Passengers ~ pdq(2,1,2) + drift()))
forecast_d <- model_d |> forecast(h = 10)

# Plot and save forecast (Part d with constant)
p_d <- autoplot(forecast_d, data) +
  labs(title = "ARIMA(2,1,2) Model with Drift", y = "Passengers")
print(p_d) # Display plot in console
ggsave("forecast_d_with_constant.png", plot = p_d)

# Remove the constant (drift) and observe behavior
model_d_no_constant <- data |> model(ARIMA(Passengers ~ pdq(2,1,2)))
forecast_d_no_constant <- model_d_no_constant |> forecast(h = 10)

# Plot and save forecast (Part d without constant)
p_d_nc <- autoplot(forecast_d_no_constant, data) +
  labs(title = "ARIMA(2,1,2) Model Without Drift", y = "Passengers")
print(p_d_nc) # Display plot in console
ggsave("forecast_d_no_constant.png", plot = p_d_nc)

# Part (e) ARIMA(0,2,1) with a constant
model_e <- data |> model(ARIMA(Passengers ~ pdq(0,2,1) + drift()))
forecast_e <- model_e |> forecast(h = 10)

# Plot and save forecast (Part e)
p_e <- autoplot(forecast_e, data) +
  labs(title = "ARIMA(0,2,1) Model with Constant", y = "Passengers")
print(p_e) # Display plot in console
ggsave("forecast_e.png", plot = p_e)

# Observe the saved plots to analyze behavior
cat("All forecast plots have been saved as PNG files in the working directory.\n")

```
The ARIMA model provides a strong forecast of Australian air passengers based on historical data from 1970 to 2011. The model aligns well with the data's upward trend and projects continued growth. The confidence intervals indicate increasing uncertainty further into the future, but overall, the model suggests robust forecasting.

The ARIMA(0,1,0) model with drift, which forecasts a steady linear increase in passenger numbers. While this model captures the general growth trend, it lacks the flexibility of the selected ARIMA model, as it does not account for more complex dynamics present in the historical data.

The ARIMA(2,1,2) model with drift. This model offers a more nuanced representation of the passenger data, capturing both growth and some additional fluctuations. When the drift is removed, as shown in the fourth plot (ARIMA(2,1,2) without drift), the forecasts become stagnant, indicating the importance of the drift term for projecting future increases in passenger numbers.

The fifth plot illustrates an ARIMA(0,2,1) model with a constant. This model produces forecasts with a steeper growth trajectory compared to other models, emphasizing how the second-order differencing combined with a constant can amplify the growth trend.

# 9.8 )

For the United States GDP series (from global_economy):

a) if necessary, find a suitable Box-Cox transformation for the data;
b) fit a suitable ARIMA model to the transformed data using ARIMA();
c) try some other plausible models by experimenting with the orders chosen;
d) choose what you think is the best model and check the residual diagnostics;
e) produce forecasts of your fitted model. Do the forecasts look reasonable?
f) compare the results with what you would obtain using ETS() (with no transformation).

```{r 98}
# Load necessary libraries
library(fpp3)

# Filter United States GDP data
us_gdp <- global_economy |> filter(Country == "United States")

# Part (a): Box-Cox transformation if necessary
lambda <- us_gdp |> 
  features(GDP, features = guerrero) |> 
  pull(lambda_guerrero)
cat("The optimal Box-Cox lambda value is:", lambda, "\n")

if (lambda != 1) {
  us_gdp <- us_gdp |> mutate(GDP = box_cox(GDP, lambda))
}

# Part (b): Fit a suitable ARIMA model
model_arima <- us_gdp |> model(ARIMA(GDP))
report(model_arima)

# Part (c): Experiment with alternative ARIMA models
model_arima_alt1 <- us_gdp |> model(ARIMA(GDP ~ pdq(1,1,0)))
model_arima_alt2 <- us_gdp |> model(ARIMA(GDP ~ pdq(2,1,2)))
report(model_arima_alt1)
report(model_arima_alt2)

# Part (d): Manual Residual Diagnostics

# Extract residuals as a tibble and convert to a numeric vector
residuals_tibble <- model_arima |> residuals()
residuals_vector <- residuals_tibble$.resid

# Plot residuals
p_residuals <- autoplot(ts(residuals_vector)) +
  labs(title = "Residuals of the Selected ARIMA Model", y = "Residuals") +
  theme_minimal()
print(p_residuals)
ggsave("residuals_manual_plot.png", plot = p_residuals)

# Plot ACF of residuals
p_acf <- ggAcf(ts(residuals_vector)) +
  labs(title = "ACF of Residuals") +
  theme_minimal()
print(p_acf)
ggsave("acf_manual_residuals_plot.png", plot = p_acf)

# Perform the Ljung-Box test
ljung_box <- Box.test(residuals_vector, lag = 10, type = "Ljung-Box")
print(ljung_box)

# Assess results of the Ljung-Box test
if (ljung_box$p.value > 0.05) {
  cat("Residuals resemble white noise (no significant autocorrelation).\n")
} else {
  cat("Residuals show significant autocorrelation. Consider revising the model.\n")
}

# Part (e): Produce forecasts of the fitted model
forecasts <- model_arima |> forecast(h = 10)
p_forecast <- autoplot(forecasts, us_gdp) +
  labs(title = "ARIMA Model Forecasts", y = "GDP") +
  theme_minimal()
print(p_forecast)
ggsave("forecast_arima.png", plot = p_forecast)

# Part (f): Compare with forecasts obtained using ETS() (with no transformation)
model_ets <- us_gdp |> model(ETS(GDP))
forecasts_ets <- model_ets |> forecast(h = 10)
p_forecast_ets <- autoplot(forecasts_ets, us_gdp) +
  labs(title = "ETS Model Forecasts", y = "GDP") +
  theme_minimal()
print(p_forecast_ets)
ggsave("forecast_ets.png", plot = p_forecast_ets)

# Comparison of ARIMA and ETS forecasts
forecasts_combined <- bind_rows(
  forecasts |> mutate(Model = "ARIMA"),
  forecasts_ets |> mutate(Model = "ETS")
)
p_forecast_combined <- autoplot(forecasts_combined, us_gdp) +
  labs(title = "Comparison of ARIMA and ETS Forecasts", y = "GDP") +
  facet_wrap(~Model, scales = "free_y") +
  theme_minimal()
print(p_forecast_combined)
ggsave("forecast_combined.png", plot = p_forecast_combined)
```
# Interpretation
The Residuals are the differences between the observed and predicted values, and this plot helps evaluate whether these errors are randomly distributed around zero. In an ideal scenario, residuals should exhibit no discernible patterns, as this indicates the model has captured all the significant dynamics of the data. If clusters or trends are visible, it suggests that the model is missing key components. This plot generally aligns with the expectation of randomness, validating the ARIMA model's effectiveness in fitting the U.S. GDP data.

ACF of Residuals:
  
  The  autocorrelation function (ACF) of residuals, which measures the correlation of the residuals at varying time lags where each bar represents a lag, and those within the 95% confidence bounds  suggest no significant autocorrelation. This indicates that residuals resemble white noise. If bars exceed the bounds at certain lags, it signifies autocorrelation, meaning the model may require refinements. The plot supports the conclusion that the ARIMA model adequately accounts for the GDP data's temporal dependencies, as most bars remain within the confidence intervals.

ARIMA Model Forecasts:
The ARIMA model is extending predictions 10 periods into the future, also the model projects a steady continuation of the historical growth observed in the GDP series, with forecast values closely aligning with past trends. Confidence intervals, represented by shaded regions, widen as the forecast horizon extends, signifying greater uncertainty in long-term predictions. This plot highlights the ARIMA model's ability to capture and extrapolate the underlying growth trajectory effectively while accounting for forecast variability.

ETS Model Forecasts :
  This plot depicts forecasts produced by the ETS model, which is specifically designed for data with exponential trends and seasonality. The ETS model identifies the consistent growth pattern in the GDP data and extends it into future periods, maintaining an upward trajectory. Like the ARIMA model, the forecast includes confidence intervals that expand as predictions go further into the future. The results align well with historical data, showcasing the ETS model's strength in capturing exponential trends and making plausible predictions for the U.S. GDP series.

Comparison of ARIMA and ETS Forecasts :
The comparison of forecasts from the ARIMA and ETS model are both models exhibit similar upward growth trends, but subtle differences in their forecast intervals and trajectories are evident. For instance, the ARIMA model may produce narrower confidence intervals, suggesting higher precision in predictions. The ETS model, on the other hand, emphasizes exponential characteristics in its forecasts. This comparison highlights the strengths of each approach: ARIMA's flexibility in modeling temporal dependencies versus ETS's focus on exponential trends. Together, they provide valuable insights into future GDP growth, reinforcing the reliability of both models for time series forecasting.


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
