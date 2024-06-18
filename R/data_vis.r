library(ggplot2)
library(fable)

# Import data from github repo.
df_og <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_og.csv")
df_sa <- read.csv("https://raw.githubusercontent.com/HPCurtis/causalcovidcattle/main/data/cattle_sa.csv")

df_og$Date <- as.Date(df_og$Date)
df_sa$Date <- as.Date(df_sa$Date)

df_sa <- drop_na(df_sa)

# Visualisations for the project
ggplot(df_sa, aes(x = Date, y = Number.Slaughtered....CATTLE..excl..calves.....Total..State......11)) +
  geom_line(color = "blue") +
  labs(title = "Time Series Plot",
       x = "Date",
       y = "Number of Slaughtered Cattle (excl claves)") +
  theme_minimal()
