---
title: "Ongoing Opioid Usage Data Exploration"
author:  "Dennis Wollersheim"
date: "2018-02-08"
output: html_document
---



```
## Warning in gpclibPermit(): support for gpclib will be withdrawn from maptools at the next major release
```
We first divide up the data into continious drug taking groups.

The data is arranged by person and drug type (major type, eg morphine, codeine, etc), and sorted in supply date order .  
The key idea is we 'know' the rate that a person consumed a drug based on the length of time between prescriptions.
THere are 2 types of purchaser:  single drug purchaser, and multiple drug purchaser.

Once someone is a multiple purchaser, we break up their purchases into episodes.  An episode is ongoing if they have at least 0.25 doses / day, where doses are calculated based on number of pills, or scheduled time wearing patches, and number of days is calculated based on the time between this prescription and the next. 

When we don't have a successive prescription at the 0.25 doses / day rate, we:
1.  terminate the episode.  After this, patient may have other episodes of taking this drug, and
2.  to determine the length of the overall drug taking episode (using_days) , we assume that they take this current prescription at the same average DDD rate as they did for the other prescriptions in the episode 

For analysis, we can:
1.  compare single episode purchaser vs multiple episode purchaser: demographics, drug type, DDD
2.  compare within and between drug purchase episodes:  length, drug type, DDD, time series within and between
3.  compare multiple episode purchasers:  length of episodes, number of episodes, drug type, DDD, demographics ; eg what is the difference between a people who have short episodes and pp who have long episodes
4.  compare the interaction between different drug types across the life of of a person

## what do we know about drug use duration
* when we have multiple prescriptions, we know the time between uses
* when we have single prescriptions, we have point-in-time data

## how to calculate using_days, the number of days an episode spans
* where there is only a single prescription in an episode, we cannot calculate using_days;  we use NA here
* Where there is more than 1 prescripition in an episode, using_days for each prescription is the number of days between this and the next prescription in an episode
* for the last prescription of an multi-prescription episode
* calculate the average doses / day for the episode (using ddd), not including the last episode
* take the ddd of the last prescription, and calculate the number of days that it applies to, based on the previous average


# looking at a single person, high user

```
## # A tibble: 234 x 9
## # Groups:   pin, gender, age, state, type_name, supply_date [234]
##           pin gender   age state               type_name supply_date ndays difference     rate
##         <dbl>  <chr> <chr> <chr>                   <chr>      <date> <int>     <time>    <dbl>
##  1 1841086769     F     26  NSW                    Bupre  2016-07-26    14     3 days 4.666667
##  2 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-08-29    48     9 days 5.333333
##  3 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-09-07    56    16 days 3.500000
##  4 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-09-23    56    22 days 2.545455
##  5 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-10-15    56    24 days 2.333333
##  6 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-11-08    28     9 days 3.111111
##  7 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-11-17    28    23 days 1.217391
##  8 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2013-12-10    56    28 days 2.000000
##  9 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2014-01-07    56     9 days 6.222222
## 10 1841086769     F     26  NSW  Oxycodone and Oxy+Nalox  2014-01-16    20     5 days 4.000000
## # ... with 224 more rows
```



# number of episodes per person

```r
df_differences %>%
  group_by( pin, type_name, episode) %>%
  summarize( no_episode = max(episode ) ) %>%
  ggplot( aes( no_episode )) + geom_histogram() + ggtitle("maximum number of pill usage groups per person")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk episodes_per_person](figure/episodes_per_person-1.png)

# Look at a multi episode Tramadol user

```r
#df_differences %>%
#  group_by( pin, type_name, episode) %>%
#  summarize( no_episode = max(episode ) ) %>%
#  filter( no_episode == 6 )

df_differences %>%
  filter( pin == "9568561825") %>%
  filter( type_name=="Tramadol") %>%
  arrange( pin, type_name, supply_date )
```

```
## # A tibble: 16 x 15
## # Groups:   pin, type_name, episode [7]
##           pin gender   age state type_name supply_date pill_days  no_doses difference      rate ddd_per_day episode n_within mean_ddd_day using_days
##         <dbl>  <chr> <chr> <chr>     <chr>      <date>     <int>     <dbl>      <int>     <dbl>       <dbl>   <dbl>    <int>        <dbl>      <dbl>
##  1 9568561825     F     76  QLD   Tramadol  2013-04-04        60 10.000000        253 0.2371542  0.03952569       0        1           NA         NA
##  2 9568561825     F     76  QLD   Tramadol  2013-12-13        20  6.666667         10 2.0000000  0.66666667       1        1   0.32828283         10
##  3 9568561825     F     76  QLD   Tramadol  2013-12-23        20  3.333333         22 0.9090909  0.15151515       1        2   0.32828283         22
##  4 9568561825     F     76  QLD   Tramadol  2014-01-14        20  3.333333         20 1.0000000  0.16666667       1        3   0.32828283         20
##  5 9568561825     F     76  QLD   Tramadol  2014-02-03        20  3.333333        158 0.1265823  0.02109705       1        4   0.32828283         10
##  6 9568561825     F     76  QLD   Tramadol  2014-07-11        20  3.333333         48 0.4166667  0.06944444       2        1   0.06944444         48
##  7 9568561825     F     76  QLD   Tramadol  2014-08-28        20  3.333333        100 0.2000000  0.03333333       2        2   0.06944444         48
##  8 9568561825     F     76  QLD   Tramadol  2014-12-06        20  3.333333         25 0.8000000  0.13333333       3        1   0.13333333         25
##  9 9568561825     F     76  QLD   Tramadol  2014-12-31        20  3.333333        113 0.1769912  0.02949853       3        2   0.13333333         25
## 10 9568561825     F     76  QLD   Tramadol  2015-04-23        20  3.333333         62 0.3225806  0.05376344       4        1   0.05376344         62
## 11 9568561825     F     76  QLD   Tramadol  2015-06-24        20  3.333333        190 0.1052632  0.01754386       4        2   0.05376344         62
## 12 9568561825     F     76  QLD   Tramadol  2015-12-31        20  3.333333        146 0.1369863  0.02283105       5        1           NA         NA
## 13 9568561825     F     76  QLD   Tramadol  2016-05-25        20  3.333333         36 0.5555556  0.09259259       6        1   0.07667493         36
## 14 9568561825     F     76  QLD   Tramadol  2016-06-30        20  3.333333         79 0.2531646  0.04219409       6        2   0.07667493         79
## 15 9568561825     F     76  QLD   Tramadol  2016-09-17        20  3.333333         35 0.5714286  0.09523810       6        3   0.07667493         35
## 16 9568561825     F     76  QLD   Tramadol  2016-10-22        20  3.333333         61 0.3278689  0.05464481       6        4   0.07667493         43
```



# length of entire episode, for those people took pills for longer than 1 prescription

```r
df_differences %>% group_by( pin, type_name, episode ) %>% 
	mutate( n=n() ) %>%
	filter( n > 1 ) %>%
	ungroup() %>%
	{.} -> df_differences_multi

df_differences %>% group_by( pin, type_name, episode ) %>% 
	mutate( n=n() ) %>%
	filter( n == 1 ) %>%
	ungroup() %>%
	{.} -> df_differences_single


df_differences_multi %>%
  group_by( pin, type_name, episode) %>%
  summarize( ndays = max(supply_date ) - min(supply_date) ) %>%
  ggplot( aes( ndays )) + geom_histogram( aes(fill=type_name)) + ggtitle("length of pill usage groups in days") 
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk length_of_episode](figure/length_of_episode-1.png)

# difference between single  and multi drug episodes.  People don't like codeine, but Tramadol is a goer

```r
df_differences %>% 
  group_by( pin, type_name, episode) %>%
  summarize( one_off = ifelse( n() > 1, "Multi", "Single")) %>%
  ggplot( aes( one_off )) + geom_histogram( aes(fill=type_name), stat="count") + ggtitle("Is a drug used on an ongoing basis?") 
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![plot of chunk single_multi_compare](figure/single_multi_compare-1.png)
# people who had more than one episode, and had an episode with >100 scripts

```r
df_differences %>% group_by( pin, type_name, episode ) %>% filter( n() > 1 ) %>%
  summarize( ndays = max(supply_date ) - min(supply_date), n_scripts = max(n_within))  %>% 
  filter( n_scripts > 100) 
```

```
## # A tibble: 5 x 5
## # Groups:   pin, type_name [5]
##          pin               type_name episode     ndays n_scripts
##        <dbl>                   <chr>   <dbl>    <time>     <dbl>
## 1 1841086769 Oxycodone and Oxy+Nalox       0 1300 days       226
## 2 4548185775                Fentanyl       0 1511 days       102
## 3 6206432177                Tramadol       0  770 days       104
## 4 7674105569                Morphine       0 1507 days       140
## 5 7708389173                Morphine       0 1534 days       123
```
# the range of using_days and number of episodes for each state and age group

```r
df_differences %>%
  group_by( pin, gender, age, type_name) %>%
  summarize( using_days = sum( using_days ), ngroup = max( episode)  ) %>%
  ggplot( aes( x=using_days, y=ngroup )) + 
  geom_point( aes(color = gender)) + 
  ggtitle("length of total pill days vs # of pill usage episodes") +
 facet_grid(type_name ~ age_grouping( age, 25 ),  margins = FALSE) 
```

```
## Warning: Removed 353 rows containing missing values (geom_point).
```

![plot of chunk facet_by_age_state](figure/facet_by_age_state-1.png)

```r
df_differences %>%
  group_by( pin, gender, age, state) %>%
  summarize( using_days = sum( using_days ), ngroup = max( episode)  ) %>%
  ggplot( aes( x=using_days, y=ngroup )) + 
  geom_point( aes(color = gender)) + 
  ggtitle("length of total pill days vs # of pill usage episodes") +
 facet_grid(state ~ age_grouping( age, 25 ),  margins = FALSE) 
```

```
## Warning: Removed 295 rows containing missing values (geom_point).
```

![plot of chunk facet_by_age_state](figure/facet_by_age_state-2.png)
# characteristics of long episode users (> 5 prescriptions )
# rate is drug taking rate, number of pills / day generally

```r
df_differences %>% 
  group_by( pin, type_name, episode ) %>%
  summarise( rate_sd = sd( rate )
            , rate_mean = mean( rate )
            , no_scripts=n()
            , first = min(supply_date)
            , last = max(supply_date)
            , len = last - first 
            ) %>%
  arrange((rate_sd)) %>%
  filter( no_scripts > 5 ) 
```

```
## # A tibble: 132 x 9
## # Groups:   pin, type_name [122]
##           pin               type_name episode    rate_sd rate_mean no_scripts      first       last       len
##         <dbl>                   <chr>   <dbl>      <dbl>     <dbl>      <int>     <date>     <date>    <time>
##  1 7561543782                   Bupre       0 0.02704056 1.0006386          8 2016-09-12 2017-02-27  168 days
##  2 6090938648                   Bupre       0 0.13088021 1.0138889          6 2014-04-29 2014-07-10   72 days
##  3  448216859                   Bupre       0 0.13897136 0.9526775         17 2013-01-14 2014-04-15  456 days
##  4 9086705165 Oxycodone and Oxy+Nalox       0 0.19642870 2.0588671          7 2016-09-09 2017-02-07  151 days
##  5 5445313665 Oxycodone and Oxy+Nalox       1 0.23143383 1.0307914          7 2015-04-20 2015-09-19  152 days
##  6 4824399743                   Bupre       0 0.24468066 0.9661621         11 2013-01-11 2013-06-06  146 days
##  7 7116271468 Oxycodone and Oxy+Nalox       0 0.26275628 0.7480072         17 2013-02-14 2014-11-12  636 days
##  8  804955211 Oxycodone and Oxy+Nalox       0 0.26559809 1.9546553         46 2013-07-27 2016-12-09 1231 days
##  9 4196020787            Para_Codeine       0 0.32161830 0.6938327         11 2013-01-22 2013-11-22  304 days
## 10 2568560621                Fentanyl       0 0.32992244 1.0829798         45 2013-01-07 2014-10-22  653 days
## # ... with 122 more rows
```

# what is the distributaion of drug taking rates, for ongoing episodes

```r
df_differences %>% 
  filter( rate < 10 ) %>%
 ggplot(aes( rate )) + geom_histogram(bins=100) 
```

![plot of chunk rates](figure/rates-1.png)

#  slope histogram - the difference between successive rates

```r
df_differences %>% 
  group_by(pin,  type_name) %>%
  arrange( pin, type_name, supply_date ) %>%
  mutate( slope = lead(rate) - rate) %>%
  select( slope, pin, type_name, difference, rate, everything()) %>% 
  filter( !is.na(slope) & abs(slope) < 10 ) %>%
  ggplot(aes( slope )) + geom_histogram(bins=100) 
```

![plot of chunk slope_histogram](figure/slope_histogram-1.png)


#  per-patient slopes with unit widths

```r
df_differences %>% 
  group_by(pin, type_name) %>%
  arrange( pin, type_name, supply_date ) %>%
  mutate( slope = lead(rate) - rate
         , id = row_number()) %>%
  select( id, slope, pin, type_name, difference, rate, everything()) %>% 
  filter( !is.na(slope) ) %>%
 ggplot(aes( x=id, y=slope, group=pin )) + geom_line() 
```

![plot of chunk slope_individuals](figure/slope_individuals-1.png)

#  graphing per-patient slopes with dates, by agegroup and gender

```r
df_differences %>% 
  group_by(pin, type_name) %>%
  arrange( pin, type_name, supply_date ) %>%
  mutate( slope = lead(rate) - rate
         , id = row_number()) %>%
  select( id, slope, pin, type_name, difference, rate, everything()) %>% 
  filter( !is.na(slope) ) %>%
 ggplot(aes( x=supply_date, y=slope, group=pin )) + geom_line() +
 facet_grid(gender~ age_grouping( age, 30 ),  margins = FALSE) 
```

![plot of chunk Slope_individuals_faceted](figure/Slope_individuals_faceted-1.png)

