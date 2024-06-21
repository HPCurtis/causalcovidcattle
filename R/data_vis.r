library(ggplot2)
library(fable)

#Import data----
# Import data from github repo.
df_og <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_og.csv")
df_sa <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_sa.csv")

df_og$Date <- as.Date(df_og$Date) 
df_sa$Date <- as.Date(df_sa$Date) 

#Pre/Post Covid----
pre_covid <- df_sa %>%
  drop_na() %>%
  filter(Date < ymd("2020-03-01")) %>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble( index = Date)

post_covid <- df_sa %>%
  drop_na() %>%
  filter(Date >=  ymd("2020-03-01") )%>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble( index = Date)

# Visualisations for the project

#Time series Decomposition------
# Conduct STL decomposition.
dcmp <- drop_na(df_sa) |>
  model(stl = STL(NumberSlaughteredCATTLEexclcalvesTotalState))

components(dcmp) |> autoplot()


# Fit forecast model for seasonal adjusted model.
fitlinear <- pre_covid |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))

fc <- fitlinear %>%
  forecast(h = nrow(post_covid)) %>%
  # Get forecast intervals
  hilo()

fc$Date <- as_date(fc$Date)

plinear <- fc %>% autoplot(drop_na(df_sa)) +
  labs(y = "Number of Cattle Slaughtered", 
       title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) + geom_vline(xintercept = as.Date("2020-03-01"))

# Save plot out
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/linearforecast.png", plot = plinear, width = 6, height = 4, units = "in", dpi = 300)

#Calculate Causal impact----
yhat <- fc$.mean
yhatupper <- fc$`95%`$upper
yhatlower <- fc$`95%`$lower

# Clauate mean and lower and upper bounds
Totalslaughteredimpact <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhat 
Totalslaughteredimpactupper <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatupper 
Totalslaughteredimpactlower <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatlower

# Estimate impact of Covid over the forecast-able period.
totalmean <- sum(Totalslaughteredimpact)
totalupper <- sum(Totalslaughteredimpactupper)
totallower <- sum(Totalslaughteredimpactlower)

# PLot of causal impact 

causal_impact_plot <- ggplot() +
  geom_line(data = drop_na(df_sa), aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState), color = "black") +
  geom_ribbon(data = fc, 
              aes(x = Date, ymin =  post_covid$NumberSlaughteredCATTLEexclcalvesTotalState),
                  ymax = yhat, fill = "blue", alpha = 0.2) +
  labs(y = "Number of Cattle Slaughtered", 
       title = "Total number of cattle (excl calves) across all Australian States") +
  theme(
    plot.title = element_text(size = 10),      # Title font size
    axis.title = element_text(size = 8),       # Axis titles font size
    axis.text = element_text(size = 7)         # Axis text font size
  ) +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") 

  # Save plots out-----
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/linearforecast.png",
       plot = plinear, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename = "/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/img/causal_impact.png",
       plot = causal_impact_plot, width = 6, height = 4, units = "in", dpi = 300)
