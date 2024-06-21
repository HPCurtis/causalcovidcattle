# Causal impact of Covid on Australian Cattle livestock slaughter numbers

## Background and Overview
The following analysis is investigation into whether Covid had any potential causal impact on the Australian cattle market in the form of the number of cattle that were slaughtered. Specifically, the analysis here uses an interupted time-series (ITS) quasi-experimental methodology to analyse whether the number of cattle slaughtered was impacted by the Covid-19 pandemic. The following work is highly inspired by the work of [Rami Kasparin](https://ramikrispin.github.io/2021/01/covid19-effect/), [Matheus Facure](https://matheusfacure.github.io/python-causality-handbook/landing-page.html) and the work of [CausalPy and all its developers](https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series) and generally [Hyndman & Athanasopoulos](https://otexts.com/fpp3/) incredible book and their associated timeseries analysis packages that are simple brilliant.

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
The R code above shows that to conduct the analysis here the data had to be split between pre and post Covid-19 0n the date of 2020-03-01 within the data avaible from the ABS

### Modelling

```
# Fit linear model for seasonal adjusted data.
fitlinear <- pre_covid |>
  model(trend_model = TSLM(NumberSlaughteredCATTLEexclcalvesTotalState ~ trend()))

# Generate forecasts for the post-covid period.
fc <- fitlinear %>%
  forecast(h = nrow(post_covid)) %>%
  # Get forecast intervals
  hilo()

```
As the code above shows the timeseries model applied to the timeseries data was a linear model. This a simple example of an ITS analysis similar to that applied by the CausalPy package. As noted in their documentation more complex timeseries models can be applied in ITS anlaysis. Non of these are applied here as the analysis is being conducted on the seasonally adjusted data provided by the ABS. Seasonal adjjustments are very common with timeseries analyses (Hyndham & Athanasopoulos, 2021) they allow for modelling of the data to be easier by extracting varaiblity that in specific cases in not the focus of an analysis. This is the case with current analysis because we are interested in estimating a causal effect of Covid-19  ont he the number of cattle slaughtered acroos all Autralian States combined. so by extating seasonal variablity we are focusing on modelling the overall trend of the data and the causal impact of covid on this overall trend. Therfore a linear model is approriate for this estiamtion task.

![x](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/timeseries.png?raw=true)
Fig 1: 

## Plots

![a](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/linearforecast.png?raw=true)

![t](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/causal_impact.png?raw=true)

## Causal impact calculations
```
# Calculate Causal impact----
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

To calculate the causal impact 


## Real world impacts

-  Beef production equated to 20% of the Austalian farm production a few year before covid (2016-2017). Also, the beef industry was valued at $12.7 billion (Australian dollars) to the Australian economy 

- Tax levy 
With the sale of cattle their is a $5 (Aus) dollar levy tax on each head of cattle with our estimates Covid-19 cost the Australian goverment anywhere from 13,316,392 to 60,767,606 in lost tax revenue.

- Most Austalian cattle are kept for 18 months before slaughter a cow that is not slaughtered is costing more money in feeding and watering with any as profit that a cattle presents slowly being removed from the farmer. Feeding a fully grown cow brings no value to the farmer, 

## References

Hyndman, R.J., & Athanasopoulos, G. (2021) *Forecasting: principles and practice*, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3. Accessed on UTD.

Facure, M. (2022). *Causal inference for the Brave and the True*. Self Published.

Web resources

https://ramikrispin.github.io/2021/01/covid19-effect/
https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series