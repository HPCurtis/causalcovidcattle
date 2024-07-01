# Import relevant 
library(ggplot2)
library(fable)
library(feasts)
library(tsibble)

#Import data----
# Import data from github repo.
df_sa <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_sa.csv")

# Convert to date form.
df_sa$Date <- as.Date(df_sa$Date) 

# Set up data for all following code.
df_sa <- df_sa %>% drop_na() %>% 
  as_tsibble( index = Date) %>% 
  mutate(Date = yearquarter(Date)) 

# Plot the tiemseries basically.
timeseries_plot <- ggplot() +
  geom_line(data = df_sa, aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState), color = "blue", linetype = "solid") +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") +
  labs(x = "Date", y = "Number of Cattle Slaughtered ('000)", title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) +
  annotate("text", x = yearquarter("2000 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Pre-COVID", color = "black", size = 2) +
  annotate("text", x = yearquarter("2023 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Post-COVID", color = "black", size = 2)


#Pre/Post Covid----
pre_covid <- df_sa %>%
  filter(Date < yearquarter("2020 Q2")) %>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble(index = Date)

post_covid <- df_sa %>%
  filter(Date >=   yearquarter("2020 Q2"))%>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble(index = Date)

# Visualisations for the project

# Fit linear model for seasonal adjusted data.
fitlinear <- pre_covid |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))

# Generate forecasts for the post-covid period.
fc <- fitlinear %>%
  forecast(h = nrow(post_covid)) %>%
  # Get forecast intervals
  hilo()

plinear <- ggplot() +
  geom_line(data = df_sa, aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState), color = "blue", linetype = "solid") +
  geom_line(aes(x = fc$Date, y = fc$.mean), color = "blue") +
  geom_ribbon(aes(x = fc$Date, ymin = fc$`95%`$lower, ymax = fc$`95%`$upper), 
              fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") +
  labs(x = "Date", y = "Number of Cattle Slaughtered ('000)", title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) +  
  annotate("text", x = yearquarter("2000 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Pre-COVID", color = "black", size = 2) +
  annotate("text", x = yearquarter("2023 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Post-COVID", color = "black", size = 2)

#Calculate Causal impact----
yhat <- fc$.mean
yhatupper <- fc$`95%`$upper
yhatlower <- fc$`95%`$lower

# Calculate mean and lower and upper bounds
Totalslaughteredimpact <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhat 
Totalslaughteredimpactupper <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatupper 
Totalslaughteredimpactlower <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatlower

# Estimate impact of Covid-19 over the forecast-able period.
totalmean <- sum(Totalslaughteredimpact) * 1000
totalupper <- sum(Totalslaughteredimpactupper) * 1000
totallower <- sum(Totalslaughteredimpactlower) * 1000

# Tax levy loss calculation.
cattle_levy = 5 
lost_revenue_mean = totalmean * cattle_levy
lost_revenue_upper = totalupper * cattle_levy
lost_revenue_lower = totallower * cattle_levy

# Plot of causal impact 
causal_impact_plot <- ggplot() +
  geom_line(data = drop_na(df_sa), aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState), color = "blue") +
  geom_ribbon(data = fc, 
              aes(x = Date, ymin =  post_covid$NumberSlaughteredCATTLEexclcalvesTotalState),
                  ymax = yhat, fill = "red", alpha = 1) +
  labs(y = "Number of Cattle Slaughtered ('000)", 
       title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") +
  annotate("text", x = yearquarter("2000 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Pre-COVID", color = "black", size = 2) +
  annotate("text", x = yearquarter("2023 Q2"), y = max(df_sa$NumberSlaughteredCATTLEexclcalvesTotalState) * 1.05, label = "Post-COVID", color = "black", size = 2)

# Create table for saving and presentation in the README file to be uploaded.

  # Save plots out-----
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/timeseries.png",
       plot = timeseries_plot, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/linearforecast.png",
       plot = plinear, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/causal_impact.png",
       plot = causal_impact_plot, width = 6, height = 4, units = "in", dpi = 300)