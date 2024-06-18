# The following script is used for data cleaning of cattle data slaughter data
# downloaded from the Australian bureau of statistics. 
#(see, https://www.abs.gov.au/statistics/industry/agriculture/livestock-products-australia/latest-release)

# Load in data cleaning packages.
library(dplyr)

# setwd this can be reomved once pushed to github and the data can called from github.
setwd("/home/harrison/Desktop/gitHubRepos/cattlecovidcausal/data/")

# Read in excel spreadsheet data.
df <- readxl::read_excel("cattle.xlsx", sheet = "Data1")
# add Data tile to dataframe column relvenat
df <- df %>% rename(Date = ...1 )

# Extract row to which identify 
identifier_row <- df[2, ]

# Identify columns related to raw and seasonally adjusted data
og_columns <- names(df)[which(identifier_row == "Original")]
sa_columns <- names(df)[which(identifier_row == "Seasonally Adjusted")]

# Create two data frames for original and seasonally adjusted.
df_og <- df[, c("Date", og_columns)]
df_sa <- df[, c("Date", sa_columns)]

# Drop the first 10 rows of excel work from the og and sa dataframes.
df_og <- df_og[-(1:9), ]
df_sa <- df_sa[-(1:9), ]

# Convert to serial numbers to date values.
df_og$Date <- as.numeric(df_og$Date)
df_og$Date <- as.Date(df_og$Date, origin = "1899-12-30")
