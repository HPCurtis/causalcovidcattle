library(ggplot2)
library(fable)

# Import data from github repo.
df_og <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_og.csv")
df_sa <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_sa.csv")

df_og$Date <- as.Date(df_og$Date)
df_sa$Date <- as.Date(df_sa$Date)

df_og <- df_og[,1:2] |> mutate(Date = yearmonth(Date))  |>
  as_tsibble( index = Date)

df_sa <- df_sa[,1:2] |> mutate(Date = yearmonth(Date))  |>
  as_tsibble(index = Date)

# Visualisations for the project

# Visualise timeseries oforiginal total number of cattle slaughtered.
ggplot(df_og, aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState)) +
  geom_line(color = "blue") +
  labs(title = "Total number of slaughtered cattle (excl calves) across all Australian states",
       x = "Date",
       y = "Number of Slaughtered Cattle") +
  theme_minimal()

# Visualise timeseries of seasonally adjsuted total number of cattle slaughtered.
ggplot(df_sa, aes(x = Date, y = NumberSlaughteredCATTLEexclcalvesTotalState)) +
  geom_line(color = "blue") +
  labs(title = "Total number of slaughtered cattle (excl calves) across all Australian states (seasonally adjusted",
       x = "Date",
       y = "Number of Slaughtered Cattle") +
  theme_minimal()

# 
fit <- as_tsibble(df_sa[,1:2]) |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))
fit |> forecast(h = "9 months")
