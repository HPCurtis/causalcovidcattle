# Causal impact of Covid on Australian Cattle livestock slaughter numbers

## Background and Overview
The following analysis is investigation into whether Covid had any potential causal impact on the Australian cattle market in the form of the number of cattle that were slaughtered. Specifically, the analysis here uses an interupted time-series quasi-experimental methodology to analyse whether the number of cattle slaughtered was impacted by the Covid-19 pandemic. The following work is highly inspired by the work of [Rami Kasparin](https://ramikrispin.github.io/2021/01/covid19-effect/), [Matheus Facure](https://matheusfacure.github.io/python-causality-handbook/landing-page.html) and the work of [CausalPy and all its developers](https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series) and generally [Hyndman & Athanasopoulos](https://otexts.com/fpp3/) incredible book and their associated timeseries analysis packages that are simple brilliant.

## Analaysis

### Data splitting
```
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
```
The code above shows that to conduct the analysis here the data had to be split between pre and post Covid the date 

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
As the code above shows the timeseries model applied to the timeseries data was a linear model.

## Plots

![a](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/linearforecast.png?raw=true)

![t](https://github.com/HPCurtis/causalcovidcattle/blob/main/img/causal_impact.png?raw=true)


## References

Hyndman, R.J., & Athanasopoulos, G. (2021) *Forecasting: principles and practice*, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3. Accessed on UTD.

Facure, M. (2022). *Causal inference for the Brave and the True*. Self Published.

Web resources

https://ramikrispin.github.io/2021/01/covid19-effect/
https://causalpy.readthedocs.io/en/stable/examples.html#interrupted-time-series