# Causal impact of Covid-19 on Australian Cattle livestock slaughter numbers

## Background and Overview
The following analysis is an investigation into whether Covid-19 had a causal impact on the Australian cattle market in the form of the number of cattle that were slaughtered. Specifically, the analysis here uses an interupted time-series (ITS) quasi-experimental methodology to evaluate this. The following work is highly inspired by the work of [Rami Kasparin](https://ramikrispin.github.io/2021/01/covid19-effect/), [Matheus Facure](https://matheusfacure.github.io/python-causality-handbook/landing-page.html) and the work of [CausalPy and all its developers](https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series) and generally [Hyndman & Athanasopoulos](https://otexts.com/fpp3/) incredible book and their associated timeseries analysis R packages that are simple brilliant.

## Data collection and cleaning.
The data used within the analysis is taken from the [Austalian Bureau of Statistics (ABS)](https://www.abs.gov.au/statistics/industry/agriculture/livestock-products-australia/latest-release). Here the data is the number of slaughtered cattle excluding calves from each Austalian state with the total for the whole of Australia calculated. The total is the primary focu of the analysis here.

# Analysis

### Data splitting
```{r}
# Convert to date form.
df_sa$Date <- as.Date(df_sa$Date) 

df_sa <- df_sa %>% drop_na() %>% 
  as_tsibble( index = Date) %>% 
  mutate(Date = yearquarter(Date)) 

#Pre/Post Covid----
pre_covid <- df_sa %>%
  filter(Date < yearquarter("2020 Q2")) %>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble(index = Date)

post_covid <- df_sa %>%
  filter(Date >=   yearquarter("2020 Q2"))%>%
  mutate(Date = yearquarter(Date)) %>%
  as_tsibble(index = Date)
```
The R code above shows that to conduct the analysis here the data had to be split between pre and post Covid-19. The date of 2020-03-01 was the date selected to be in line with Australias' Covid lockdowns (March 23, 2020) and the fidelity of sampling (quarterly) within the data collected by the ABS (Quarterly).

### Linear Modelling


```
# Fit linear model for seasonal adjusted data.
fitlinear <- pre_covid |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))
```
As the code above shows a linear model was applied to the timeseries data in order estimate the causal effect. 

This a simple example of an ITS analysis similar to that applied within the CausalPy package. As noted in their documentation more complex timeseries models can be applied to ITS analysis. None of these are applied here as the analysis is being conducted on the seasonally adjusted data provided by the ABS.

 Seasonal adjustments are very common within timeseries analyses (Hyndham & Athanasopoulos, 2021) they allow for modelling of the data to be easier by extracting variablity that in specific cases is not the focus of the analysis.
 
  This is the case with current analysis because we are interested in estimating a causal effect of Covid-19 on the total the number of cattle slaughtered across all Autralian States combined. So, by extracting seasonal variablity we are focusing on modelling the overall trend of the data and the causal impact of Covid-19 on this overall trend. Therefore, a linear model is approriate for this estimation task.
  
  Of course, the assumption of linearity might be potentially a strong one, but added complexity of non-linear models does not appear neccesary based on viewing the data in Fig 1. As the seasonally adjusted data shows the general overall increase in the number of cattle slaughtered through time that coincides with generally increased size and productivity of the cattle industry within Australia through time. 

![x](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/timeseries.png?raw=true)
Fig 1: Overall timeseries for ABS data on cattle salughter numbers pre and post COVID lockdown date.
## Forecasts
```
# Generate forecasts for the post-covid period.

fc <- fitlinear %>%
  forecast(h = nrow(post_covid)) %>%
  # Get forecast intervals
  hilo()
```

The number of forecast step values that were generated is the same as number of avaivible data points Post-Covid Recorded by the ABS. This was for 16 Quarters, so, three years post Covid-19 as defined by lockdown start.

![a](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/linearforecast.png?raw=true)
Fig 2: Plot of model forecast and prediction intervals


![t](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/causal_impact.png?raw=true)
Fig 3: Plot of estimated mean reductions in the total number of cattle slaughtered post COVID-19 Lockdowns. 

## Causal impact calculations
```
# Calculate the causal impact----
yhat <- fc$.mean
yhatupper <- fc$`95%`$upper
yhatlower <- fc$`95%`$lower

# Calculate the mean and lower and upper bounds
Totalslaughteredimpact <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhat 
Totalslaughteredimpactupper <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatupper 
Totalslaughteredimpactlower <- post_covid$NumberSlaughteredCATTLEexclcalvesTotalState - yhatlower

# Estimate impact of Covid-19 over the forecast-able period.
totalmean <- sum(Totalslaughteredimpact) * 1000
totalupper <- sum(Totalslaughteredimpactupper) * 1000
totallower <- sum(Totalslaughteredimpactlower) * 1000
```

The code above shows that the causal impact of COVID-19 were generated from model forcast estimates. These estiamtes being $\hat{y}$ and the associated 95% (Normally distributed) prediction intervals as measure of uncertainty. The difference  between the $\hat{y}$ vales and their associated intervals were subracted from observed post covid Total of cattle slaughtered. 

Table 1 shows the result of these calculations with a causal estimate of Covid-19 on the number of cattle slaiughter being a reduction of anywhere between 2,663,278 and 12,153,521 head of cattle being slaughtered. With an estimated mean reduction of 7,408,400 head of cattle due to Covid-19 over the post 3 year period.

## Tax levy calculations
```
cattle_levy = 5 
# Calculate tax revenue impact from causal estimates of cattle slaughter # numbers.
lost_revenue_mean = totalmean * cattle_levy
lost_revenue_upper = totalupper * cattle_levy
lost_revenue_lower = totallower * cattle_levy
```

### Generalised additive modelling
The following plots are the result of using the MGCV package and associated methods to fit a additive model to avoid repetition not all of the code is not provided in detail here. See, the R directory within the repository for that. But much of it is just variation of the same general workflow.

```
# Fit of genralied additive model smoother on timeseries data denoted s(t) using mgcv.
fitgam <- gam(TotalState ~ s(t), method = "REML", data = df_sapre)
```

![x](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/gamforecast.png?raw=true)
![y](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/gamforecast.png?raw=true)

## Model results

|                        |Mean|Lower|Upper|
|------------------------|------|---------|---|
| Total Cattle Slaughtered Linear|-7,408,400|-2,663,278|-12,153,521|
| Total Tax Revenue Cost $(AUS) Linear|37,041,999|13,316,392|60,767,606|
| Total Cattle Slaughtered GAM|-5,526,122|-2,707,164|-8,345,080|
| Total Tax Revenue Cost $(AUS) GAM|27,630,611|13,535,822|41,725,400|

Table 1. Causal estimates of impact on the total number on cattle slaughtered post Coivd-19 and the associated tax lost. With associated 95% prediction intervals for uncertainty estimation. 

## Real world impacts

-  Beef production equated to 20% of the Australian farm production a few year before Covid (2016-2017). Also, the beef industry was valued at $12.7 billion (Australian dollars) to the Australian economy 

- Tax levy 
Within Australia the sale of each head cattle their is a $5 (Aus) dollar levy tax on each head of cattle sold. The model estimates here suggest a Covid-19 cost to the Australian goverment anywhere from 13,316,392 to 60,767,606 in lost tax revenue.

## References

Clark, N. J., & Wells, K. (2023). Dynamic generalised additive models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 14(3), 771-784.

Facure, M. (2022). *Causal inference for the Brave and the True*. Self Published.

Hyndman, R.J., & Athanasopoulos, G. (2021) *Forecasting: principles and practice*, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3. Accessed on UTD.

Simpson, G. L. (2018). Modelling palaeoecological time series using generalised additive models. Frontiers in Ecology and Evolution, 6, 149.

Thompson, E. (2022). *How to escape from model land. How mathematical models can lead us astray and what we can do about.* Basic Books.

Web resources

https://ramikrispin.github.io/2021/01/covid19-effect/
https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series