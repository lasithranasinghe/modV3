---
title: "Mulltiple TS plots"
author: "Jay Achar"
date: "2/22/2022"
output: 
  html_document:
    code_folding: hide
---



# Important

I've noticed these parsing errors when reading in the data. I haven't looked through
them, but this would be important to check before finalizing things. 


```r
raw <- read_csv(here("data", "TB_notifications.csv"))
```

```
## Warning: 241 parsing failures.
##  row                 col           expected actual                                                         file
## 1303 new_sn_sexunk04     1/0/T/F/TRUE/FALSE    22  '/home/pjd/Documents/repos2/modV3/data/TB_notifications.csv'
## 1303 new_sn_sexunk514    1/0/T/F/TRUE/FALSE    33  '/home/pjd/Documents/repos2/modV3/data/TB_notifications.csv'
## 1303 new_sn_sexunk014    1/0/T/F/TRUE/FALSE    55  '/home/pjd/Documents/repos2/modV3/data/TB_notifications.csv'
## 1303 new_sn_sexunk15plus 1/0/T/F/TRUE/FALSE    632 '/home/pjd/Documents/repos2/modV3/data/TB_notifications.csv'
## 1304 new_sn_sexunk04     1/0/T/F/TRUE/FALSE    23  '/home/pjd/Documents/repos2/modV3/data/TB_notifications.csv'
## .... ................... .................. ...... ............................................................
## See problems(...) for more details.
```

## Define constants

It's often helpful to define standard variables that shouldn't change
at the top of your script. It's then easy to change later if you want to
re-run an analysis and also to see quickly what has been applied. 


```r
who_regions <- c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")

raw_vars_keep <-
        c(
                "country",
                "iso3",
                "g_whoregion",
                "year",
                "newrel_m04",
                "newrel_m514",
                "newrel_m014",
                "newrel_m15plus",
                "newrel_f04",
                "newrel_f514",
                "newrel_f014",
                "newrel_f15plus"
        )

age_groups <- c("014", "04", "15plus", "514")
```


## Data preparation

I tend to do all of my data preparation at the beginning. This might require
that you move later data cleaning tasks to this area, but having everything
mixed up is hard to work through for others looking at your code. 

Here, I've created two lists of data frames - **wide** and **long**. 
Their names tells me about their structure and the data frame names
(**country** and **region**) also help others to know what is contained. 


```r
wide <- list(
        country = raw[raw$year > 2012, raw_vars_keep]
)
wide$region <- wide$country %>% 
        select(-country, -iso3) %>% 
        group_by(g_whoregion, year) %>% 
        summarise(across(starts_with("newrel_"), ~ sum(.x, na.rm = TRUE)),
                  .groups = "drop")


# pivot longer
long <- list(
        country = pivot_longer(
                data = wide$country,
                cols = starts_with("newrel_"),
                values_to = "cases",
                names_to = "group"
        ) %>%
                mutate(group = str_remove(group,
                                          "^newrel_")) %>%
                separate(
                        col = "group",
                        into = c("sex", "age_group"),
                        sep = 1
                )
)

long$region <- long$country %>% 
        select(-country, -iso3) %>% 
        group_by(g_whoregion, year, sex, age_group) %>% 
        summarise(
                cases = sum(cases, na.rm = TRUE),
                .groups = "drop"
        )
```

## Arima modelling

I'll use nested list columns here to retain my origina data structure. Working with
long data is far easier than wide data so I'll start with that. 

Note how I've nested the data before training models. This creates list columns
which contain our models and forecasts against columns that identify what they are. 

Using iteration tools - e.g. the `purrr` package, simplifies all of this and reduces
the amount of code. This makes it easier to follow and reduces the potential for typos
to occur. 


```r
# sorry for the confusing code here - the purpose is to remove the
# output created by the auto.arima function since it clutters the
# HTML output of the report.

arima_models <- (function() {
        sink("/dev/null")
arima_models <- long$region %>% 
        # remove 2020
        filter(year < 2020) %>% 
        # nest data for each group of interest
        group_by(g_whoregion, sex, age_group) %>% 
        nest() %>% 
        # convert to time series = list-column
        mutate(ts = map(data, function(series) {
                series %>% 
                        # remove year variable prior to converting to TS
                        select(-year) %>% 
         ts(start = 2013, frequency = 1)       
        })) %>% 
        # fit arima.auto model to each groups data
        mutate(arima_model = map(ts, ~forecast::auto.arima(.x, d = 1, D = 1, 
                                        stepwise = FALSE, 
                                        approximation = FALSE, 
                                        trace = TRUE))) %>% 
        # run forecast for 2020 on each group
        mutate(forecast = map(arima_model, 
                              ~ forecast(.x, h = 1)))
sink()
return(arima_models)
})()
```

## Plotting

This is a little complex...I'll try and give a thorough description of what has
been done. 

Essentially there are two data frames being used by `ggplot` to create
each graphic. 

The first data frame includes all of the years and cases from the original data: 


```r
# create ts_df list
ts_list <- long$region %>% 
        filter(year < 2020) %>% 
        group_split(age_group, sex)
```



```r
head(ts_list[[1]])
```

```
## # A tibble: 6 x 5
##   g_whoregion  year sex   age_group cases
##   <chr>       <dbl> <chr> <chr>     <dbl>
## 1 AFR          2013 f     014       46519
## 2 AFR          2014 f     014       43928
## 3 AFR          2015 f     014       41023
## 4 AFR          2016 f     014       49294
## 5 AFR          2017 f     014       51373
## 6 AFR          2018 f     014       59170
```
You can see that the data frame is specific to a sex/age group. There should
therefore be 8 data frames in this `ts_list` represting each unique combination
of sex and age group. 


```r
forecast_list <- arima_models %>% 
        group_by(age_group, sex) %>% 
        group_split() %>% 
        map(
                function(group) {
                        data.frame(
                            region = group$g_whoregion,
                year = 2020,
                point = as.numeric(map(group$forecast, ~ .x$mean)),
                lower95 = as.numeric(map(group$forecast, ~ .x$lower[2])),
                upper95 = as.numeric(map(group$forecast, ~ .x$upper[2]))    
                        )
                }
        )
```

The second data frame includes the 2020 prediction information. This is grouped
in the same was as the previous data frame:


```r
head(forecast_list[[1]])
```

```
##   region year  point   lower95    upper95
## 1    AFR 2020  56346 46331.168  66360.832
## 2    AMR 2020   4981  4638.425   5323.575
## 3    EMR 2020  35661 31835.662  39486.338
## 4    EUR 2020   4237  3676.468   4797.532
## 5    SEA 2020 125425 69869.436 180980.564
## 6    WPR 2020  28335 15550.025  41119.975
```

Finally, these two lists of data frames are iterated over using `purrr::map2()`
to create a list of ggplot objects. An alternative would be to add
a call to `ggsave` within each iteration and use `purrr::walk2` instead
of `purrr::map2`. This save the ggplots automatically to disk. 



```r
map2(ts_list, 
     forecast_list,
     function(ts, forecast) {
             ggplot(ts,
                    aes(x = year,
                        y = cases,
                        color = g_whoregion)) +
                     geom_line() +
                     geom_pointinterval(
                             data = forecast,
                             aes(
                                     x = year,
                                     y = point,
                                     ymin = lower95,
                                     ymax = upper95,
                                     color = region
                             ),
                             orientation = "vertical",
                             position = position_dodgejust(width = 0.3)
                     )
     })
```

```
## [[1]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```
## 
## [[2]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png)

```
## 
## [[3]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-3.png)

```
## 
## [[4]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-4.png)

```
## 
## [[5]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-5.png)

```
## 
## [[6]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-6.png)

```
## 
## [[7]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-7.png)

```
## 
## [[8]]
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-8.png)



## Next steps

The plots are raw - they will need titles, better colors and more formatting.
I wonder whether you might be able to do some of this using this same code structure.

Regarding the plots themselves, we should think carefully about the y-axes. Right
now they are defined by ggplot based on the data in each plot. This results in the lines
being in acceptable places on the canvas, but would make comparison of different
plots more challenging and potentially might lead the casual observer to the wrong conclusion.



