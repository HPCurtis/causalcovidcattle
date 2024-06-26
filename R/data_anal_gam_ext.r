library(mgcv)
library(tidyverse)
library(gratia)


# Import data----
# Import data seasonally adjusted from github repo.
df_sa <- read_csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_sa.csv")

# Remove the word words from column apart from state from the column names
names(df_sa) <- gsub("NumberSlaughteredCATTLEexclcalves", "", names(df_sa))
df_sa <- drop_na(df_sa)
# time of Covid lockdowns 
certain_date <- as.Date("2020-03-01")

# Add both the RowID and BeforeAfter columns
df_sa <- df_sa %>%
  mutate(
    t = row_number(),
    BeforeAfter = as.numeric(Date <= certain_date)
  )

# Separate data to pre and post Covid.
df_sapre <- df_sa %>%
  filter(BeforeAfter == 1) 

df_sapost <- df_sa %>%
  filter(BeforeAfter == 0)

ggplot(df_sapre, aes(x = Date, y = TotalState )) +
  geom_line()

# Model fiiting-----
# fit glm and gam model 
fitl <- lm(TotalState ~ t, data = df_sa)
fitgam <- gam(TotalState ~ s(t), method = "REML", data = df_sapre)

# AIC measure of the model fit---
AIC(fitl,fitgam)


newdata <- data.frame(
  t = c(164:179)
)
forecasts <- predict(fitgam, newdata, se.fit = TRUE)

# Extract the fit and standard errors
forecastmean <- forecasts$fit
forecastse <- forecasts$se.fit
# Calculate 95% confidence intervals
upper <- forecastmean + 1.96 * forecastse
lower <- forecastmean - 1.96 * forecastse

forcast_df <- data.frame(
  Date = df_sapost$Date,
  forecastmean = forecastmean,
  upper = upper,
  lower = lower
)

# Calculate mean and lower and upper bounds
Totalslaughteredimpact <- df_sapost$TotalState - forecastmean 
Totalslaughteredimpactupper <- df_sapost$TotalState - upper 
Totalslaughteredimpactlower <- df_sapost$TotalState - lower

# Estimate impact of Covid-19 over the forecast-able period.
totalmean <- sum(Totalslaughteredimpact) * 1000
totalupper <- sum(Totalslaughteredimpactupper) * 1000
totallower <- sum(Totalslaughteredimpactlower) * 1000

# Model plots----
fitgam %>% draw() + xlab()
fitgam %>% appraise()


post_impact <- ggplot() +
  geom_line(data = df_sa, aes(x = Date, y = TotalState), color = "blue", linetype = "solid") +
  geom_line(aes(x = forcast_df$Date, y = forcast_df$forecastmean), color = "blue") +
  geom_ribbon(aes(x = forcast_df$Date, ymin = forcast_df$lower, ymax = forcast_df$upper,), 
              fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") 

causal_impact_plot <- ggplot() +
  geom_line(data = drop_na(df_sa), aes(x = Date, y = TotalState), color = "blue") +
  geom_ribbon(data = forcast_df, 
              aes(x = Date, ymin =  post_covid$NumberSlaughteredCATTLEexclcalvesTotalState),
              ymax = yhat, fill = "red", alpha = 1) +
  labs(y = "Number of Cattle Slaughtered ('000)", 
       title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) #
#+
 # geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") +
  #annotate("text", x = yearquarter("2000 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Pre-COVID", color = "black", size = 2) +
  #annotate("text", x = yearquarter("2023 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Post-COVID", color = "black", size = 2)

