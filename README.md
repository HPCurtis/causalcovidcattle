# Causal impact of Covid on Australian Cattle livestock slaughter numbers

## Background and Overview
The following analysis is an investigation into whether Covid-19 had any potential causal impact on the Australian cattle market in the form of the number of cattle that were slaughtered. Specifically, the analysis here uses an interupted time-series (ITS) quasi-experimental methodology to evalauate this. The following work is highly inspired by the work of [Rami Kasparin](https://ramikrispin.github.io/2021/01/covid19-effect/), [Matheus Facure](https://matheusfacure.github.io/python-causality-handbook/landing-page.html) and the work of [CausalPy and all its developers](https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series) and generally [Hyndman & Athanasopoulos](https://otexts.com/fpp3/) incredible book and their associated timeseries analysis R packages that are simple brilliant.

## Data collection and cleaning.
The data used within the analysis is taken from the [Austalian Bureau of Statistics (ABS)](https://www.abs.gov.au/statistics/industry/agriculture/livestock-products-australia/latest-release). Here the data collected is the number of slaughtered cattle excluding calves from each Austalian state with the total for whole Australia calculated as well.
## Analysis

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
The R code above shows that to conduct the analysis here the data had to be split between pre and post Covid-19. The date of 2020-03-01 was the date selected to be in line with Australias' Covid lockdowns (March 23, 2020) and the fidelity of sampling (quarterly) within the data collected by the ABS.

### Modelling

```
# Fit linear model for seasonal adjusted data.
fitlinear <- pre_covid |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))
```
As the code above shows a linear model was applied to the timeseries data in order estiamte the ITS causal effect. 

### Model complexity
This a simple example of an ITS analysis similar to that applied within the CausalPy package. As noted in their documentation more complex timeseries models can be applied to ITS anlayses. Non of these are applied here as the analysis is being conducted on the seasonally adjusted data provided by the ABS. Seasonal adjustments are very common within timeseries analyses (Hyndham & Athanasopoulos, 2021) they allow for modelling of the data to be easier by extracting varaiblity that in specific cases is not the focus of the analysis. This is the case with current analysis because we are interested in estimating a causal effect of Covid-19 on the total the number of cattle slaughtered across all Autralian States combined. So, by extracting seasonal variablity we are focusing on modelling the overall trend of the data and the causal impact of Covid-19 on this overall trend. Therefore, a linear model is approriate for this estimation task. The assumption of linearity might be potentially a strong one, but added complexity of non-linear models does not appear neccesary based on viewing the data in Fig 1. As the seasonally adjusted data shows the general overall increase in the number of cattle slaughtered through time that coicides with generally increased size and productivity of the cattle industry within Australia through time. 

![x](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/timeseries.png?raw=true)
Fig 1: 
## Forecasts
```
# Generate forecasts for the post-covid period.

fc <- fitlinear %>%
  forecast(h = nrow(post_covid)) %>%
  # Get forecast intervals
  hilo()
```

Forecast values were generated to the length of number of avaible data points Post-Covid within the data. This was for 16 Quarters, so, three years post Covid-19

![a](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/linearforecast.png?raw=true)
Fig 2: Plot of model forecast and prediction intervals


![t](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/causal_impact.png?raw=true)
Fig 3: 

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

The code above shows that to calculate the causal impact of COVID-19 from model estiamtes the forcast estiamtes $\hat{y}$ and the associated 95% predciton intervals as measure of uncertainty were extracted. The difference  between the $\hat{y}$ vales and their associated interval were subracted from observed post covid Total of cattle slaughtered. Table 1 shows the result of these calculations with a causal estimate of Covid on the number of cattle slaiughter being a reduction of anywhere between 2,663,278 and 12,153,521 head of cattle being slaughtered. With an estimated mean reduction of 7,408,400 head of cattle due to Covid-19.

## Tax levy calulations
```
cattle_levy = 5 
# Calculate tax revunue impact from causal estiamtes of cattle slaughter # numbers.
lost_revenue_mean = totalmean * cattle_levy
lost_revenue_upper = totalupper * cattle_levy
lost_revenue_lower = totallower * cattle_levy
```

|                        |Mean|Lower|Upper|
|------------------------|------|---------|---|
| Total Cattle Slaughtered|-7,408,400|-2,663,278|-12,153,521|
| Total Tax Revenue Cost $(AUS)|37,041,999|13,316,392|60,767,606|
Table 1.

## Real world impacts

-  Beef production equated to 20% of the Austalian farm production a few year before Covid (2016-2017). Also, the beef industry was valued at $12.7 billion (Australian dollars) to the Australian economy 

- Tax levy 
Within Australia the sale of each head cattle their is a $5 (Aus) dollar levy tax on each head of cattle sold. The modle estimates here suggest a Covid-19 cost to the Australian goverment anywhere from 13,316,392 to 60,767,606 in lost tax revenue.

Person and animal wealthcare considerations.
- Most Austalian cattle are kept for 18 months before slaughter a cow that is not slaughtered is costing more money in feeding and watering with any as profit that a cattle presents slowly being removed from the farmer. Feeding a fully grown cow brings no value to the farmer, 

## References

Hyndman, R.J., & Athanasopoulos, G. (2021) *Forecasting: principles and practice*, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3. Accessed on UTD.

Facure, M. (2022). *Causal inference for the Brave and the True*. Self Published.

Web resources

https://ramikrispin.github.io/2021/01/covid19-effect/
https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series