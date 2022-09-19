Case Study 2: How Can a Wellness Technology Company Play It Smart?
================
Quang Huy Vu
Sep 15, 2022

## Introduction

To begin analyzing part of this case study, the listed questions to
discover are:

-   What are some trends in the smart device usage market?

    -   What are the metrics used to define the usage of smart devices?

    -   What are the individual patterns in daily calories burnt, total
        steps taken or distance traveled, and the total sleep time?

    -   How do these metrics correlate to each other?

-   How could these trends apply to Bellabeat’s customers? And to one of
    Bellabeat’s products?

    -   What are the user segments available in the market?

    -   How do user segments differ from each other in terms of daily
        calories burnt, total steps taken or distance traveled, and the
        total time of sleep?

    -   What are the correlations of these metrics within each segment,
        and how do they differ for each segment?

-   How could these trends help influence Bellabeat’s marketing
    strategy?

    -   What are the key findings?

    -   What key selling points/propositions can Bellabeat use to
        address each customer’s need/behavior?

    -   What are the overall marketing and growth strategies for
        Bellabeat based on those key selling points/propositions and the
        findings we have?

Let’s import libraries for visualization …

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(skimr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(ggpubr)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

… and import files for further analyses.

``` r
setwd("D:/Learning/Google Data Analytics Certificate/Capstone Project/Project 2/data")
d_total <- read.csv("daily_total.csv")
h_total <- read.csv("hourly_total.csv")
```

And reformat date/datetime data to DATE and DATETIME

``` r
d_total <- d_total %>% 
  mutate(date = as.Date(date,format="%Y-%m-%d"))
h_total <- h_total %>% 
  mutate(datetime = as.POSIXct(datetime,format="%Y-%m-%d %H:%M:%S"))

head(d_total)
```

    ##   X         id       date totalsteps totaldistance trackerdistance
    ## 1 1 1503960366 2016-04-12      13162          8.50            8.50
    ## 2 2 1503960366 2016-04-13      10735          6.97            6.97
    ## 3 3 1503960366 2016-04-15       9762          6.28            6.28
    ## 4 4 1503960366 2016-04-16      12669          8.16            8.16
    ## 5 5 1503960366 2016-04-17       9705          6.48            6.48
    ## 6 6 1503960366 2016-04-19      15506          9.88            9.88
    ##   loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                        0               1.88                     0.55
    ## 2                        0               1.57                     0.69
    ## 3                        0               2.14                     1.26
    ## 4                        0               2.71                     0.41
    ## 5                        0               3.19                     0.78
    ## 6                        0               3.53                     1.32
    ##   lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                6.06                       0                25
    ## 2                4.71                       0                21
    ## 3                2.83                       0                29
    ## 4                5.04                       0                36
    ## 5                2.51                       0                38
    ## 6                5.03                       0                50
    ##   fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                  13                  328              728     1985
    ## 2                  19                  217              776     1797
    ## 3                  34                  209              726     1745
    ## 4                  10                  221              773     1863
    ## 5                  20                  164              539     1728
    ## 6                  31                  264              775     2035
    ##   totalsleeprecords totalminutesasleep totaltimeinbed
    ## 1                 1                327            346
    ## 2                 2                384            407
    ## 3                 1                412            442
    ## 4                 2                340            367
    ## 5                 1                700            712
    ## 6                 1                304            320

``` r
head(h_total)
```

    ##   X         id            datetime steptotal calories
    ## 1 1 1503960366 2016-04-12 00:00:00       373       81
    ## 2 2 1503960366 2016-04-12 01:00:00       160       61
    ## 3 3 1503960366 2016-04-12 02:00:00       151       59
    ## 4 4 1503960366 2016-04-12 03:00:00         0       47
    ## 5 5 1503960366 2016-04-12 04:00:00         0       48
    ## 6 6 1503960366 2016-04-12 05:00:00         0       48

## 1. What are some trends in the smart device usage market?

### 1.1 What are the metrics used to define the usage of smart devices?

As discovered, the metrics to be considered are: \* Total steps taken
daily \* Total calories burnt daily \* Total distance traveled daily \*
Total sleep time daily \* % of total sleep time over total in-bed time
\* Steps taken hourly \* Calories burnt hourly \* Total days used smart
devices \* Time used smart devices

### 1.2 What are the individual patterns in daily calories burnt, total steps taken or distance traveled, and the total sleep time?

#### 1.2.1 Daily patterns

Metrics: (a) steps taken, (b) calories burnt, (c) distance traveled, (d)
sleep time, (e) % of total sleep time over total in-bed time

Let’s create some plots to represent these metrics on 2 levels: \* daily
\* by weekday

##### Daily

``` r
d_data <- d_total %>%
    mutate(date=as.POSIXct(date,format = "%Y-%m-%d")) %>% 
    group_by (date) %>% 
    arrange(desc(date)) %>% 
    summarize(daily_steps = mean(totalsteps),daily_sleep = mean(totalminutesasleep), daily_distance = mean(totaldistance), daily_calories = mean(calories), sleep_percentage = mean(totalminutesasleep)/mean(totaltimeinbed)*100)

ggarrange(
      ggplot(data= d_data,mapping = aes(x=date,y=daily_steps)) 
      + geom_col(fill="#72A0C1",stat="identity")
      + labs(title = "Daily steps taken",x="",y="")
      + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
              axis.text.x = element_text(angle = 90))
      + scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d")),
      
      ggplot(data= d_data,mapping = aes(x=date,y=daily_sleep))
      + geom_col(fill="#0066b2",stat="identity")
      + labs(title="Daily sleep time",x="",y="")
      + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
              axis.text.x = element_text(angle = 90))
      + scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d")),
      
      ggplot(data= d_data,mapping = aes(x=date,y=daily_distance))
      + geom_col(fill="#00308F",stat="identity")
      + labs(title="Daily distance traveled",x="",y="")
      + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
              axis.text.x = element_text(angle = 90))
      + scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d")),
      
      ggplot(data= d_data,mapping = aes(x=date,y=daily_calories))
      + geom_col(fill="#5F9EA0",stat="identity")
      + labs(title="Daily calories burnt",x="",y="")
      + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
              axis.text.x = element_text(angle = 90))
      + scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d")),
      
      ggplot(data= d_data,mapping = aes(x=date,y=sleep_percentage))
      + geom_col(fill="#A3C1AD",stat="identity")
      + labs(title="Daily sleep percentage",x="",y="")
      + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
              axis.text.x = element_text(angle = 90))
      + scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d"))
    )
```

    ## Warning: Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat

![](Challenge2_DataVisualization_files/figure-gfm/Visualize%20metrics%20daily-1.png)<!-- -->

``` r
ggplot(data= d_data,mapping = aes(x=date,y=daily_sleep))+ 
  geom_col(fill="#0066b2",stat="identity")+ 
  labs(title="Daily sleep time",x="",y="")+ 
  theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
        axis.text.x = element_text(angle = 90))+ 
  scale_x_datetime(date_breaks = "3 day", labels = date_format("%b %d"))
```

    ## Warning: Ignoring unknown parameters: stat

![](Challenge2_DataVisualization_files/figure-gfm/Sleep%20time-1.png)<!-- -->

Through these visuals, daily step and distance move together. This makes
sense because the more steps users take, the longer the distance is.
However, there is one aspect to consider is if they run or walk, the
distance might differ. Therefore, we will add one more metric, which is
distance per step.

And most users have less sleep time than the standard of 8 hours of
sleep time.

``` r
d_data <- d_data %>% 
  group_by(date) %>% 
  arrange(desc(date)) %>% 
  summarize(daily_steps, daily_sleep, daily_distance, daily_calories, sleep_percentage, distance_per_step= daily_distance/daily_steps)

ggplot(data=d_data, mapping = aes(x=date,y=distance_per_step)) + geom_col(fill="#B9D9EB",stat="identity") + labs(title="Daily distance per step",x="",y="") + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'))
```

    ## Warning: Ignoring unknown parameters: stat

![](Challenge2_DataVisualization_files/figure-gfm/Add%20distance%20per%20step-1.png)<!-- -->
Through this visual, the distance per step does not vary much. This
metric might point us to see if the user is active on running as an
exercise activity. Due to the lack of data of hourly distance, we will
not be able to look at distance per step hourly to prove this point. So
we will drop this metric.

``` r
d_data <- select(d_data,-distance_per_step)
```

Furthermore, there might be a positive correlation between step/distance
and calories burnt. We would want to check this in the next sections of
correlation check.

##### By weekday

``` r
weekday_data <- d_data %>%
    mutate(weekday = factor(weekdays(date),levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
    group_by(weekday) %>%
    summarize(weekday_steps = mean(daily_steps), weekday_sleep = mean(daily_sleep), weekday_distance = mean(daily_distance), weekday_calories = mean(daily_calories), weekday_sleep_percentage = mean( sleep_percentage))

head(weekday_data)
```

    ## # A tibble: 6 × 6
    ##   weekday   weekday_steps weekday_sleep weekday_distance weekday_calor…¹ weekd…²
    ##   <fct>             <dbl>         <dbl>            <dbl>           <dbl>   <dbl>
    ## 1 Monday            9183.          405.             6.43           2499.    91.2
    ## 2 Tuesday           8025.          435.             5.72           2379.    92.6
    ## 3 Wednesday         7810.          405.             5.50           2208.    92.4
    ## 4 Thursday          7936.          405.             5.54           2332.    91.2
    ## 5 Friday            9857.          418.             7.01           2504.    91.2
    ## 6 Saturday          7295.          455.             5.19           2279.    90.1
    ## # … with abbreviated variable names ¹​weekday_calories,
    ## #   ²​weekday_sleep_percentage

``` r
ggarrange(
    ggplot(data= weekday_data,mapping = aes(x=weekday,y=weekday_steps)) 
    + geom_col(fill="#72A0C1",stat="identity")
    + geom_smooth()
    + labs(title = "Daily steps taken",x="",y="")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90)),
  
    ggplot(data= weekday_data,mapping = aes(x=weekday,y=weekday_distance))
    + geom_col(fill="#00308F",stat="identity")
    + geom_smooth()
    + labs(title="Daily distance traveled",x="",y="")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90)),
    
    ggplot(data= weekday_data,mapping = aes(x=weekday,y=weekday_calories))
    + geom_col(fill="#5F9EA0",stat="identity")
    + geom_smooth()
    + labs(title="Daily calories burnt",x="",y="")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90)),
    
    ggplot(data= weekday_data,mapping = aes(x=weekday,y=weekday_sleep))
    + geom_col(fill="#0066b2",stat="identity")
    + geom_smooth()
    + labs(title="Daily sleep time",x="",y="")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90)),
    
    ggplot(data= weekday_data,mapping = aes(x=weekday,y=weekday_sleep_percentage))
    + geom_col(fill="#A3C1AD",stat="identity")
    + geom_smooth()
    + labs(title="Daily sleep percentage",x="",y="")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90))
    )
```

    ## Warning: Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat
    ## Ignoring unknown parameters: stat

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Visualize%20metrics%20by%20weekday-1.png)<!-- -->

#### 1.2.2 Hourly patterns

There are two data frames that have the data we need, which are hourly
steps and calories. Lets look into these data.

``` r
h_data <- h_total %>% 
  mutate(time=hour(datetime)) %>% 
  group_by(time) %>% 
  arrange(time) %>% 
  summarize(hourly_step = mean(steptotal),hourly_calories = mean(calories))
head(h_data,n=24)
```

    ## # A tibble: 24 × 3
    ##     time hourly_step hourly_calories
    ##    <int>       <dbl>           <dbl>
    ##  1     0       42.2             71.8
    ##  2     1       23.1             70.2
    ##  3     2       17.1             69.2
    ##  4     3        6.43            67.5
    ##  5     4       12.7             68.3
    ##  6     5       43.9             81.7
    ##  7     6      179.              87.0
    ##  8     7      306.              94.5
    ##  9     8      428.             103. 
    ## 10     9      433.             106. 
    ## # … with 14 more rows

``` r
ggarrange(
    ggplot(data= h_data,mapping = aes(x=time,y=hourly_step,fill = hourly_step)) 
    + geom_col()
    + geom_smooth()
    + labs(title = "Hourly step taken",x="",y="")
    + scale_fill_gradient(low = "green", high = "red")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90),
            legend.title=element_text(size=9),
            legend.key.height=unit(0.5,'cm'),
            legend.key.width=unit(0.3,'cm'),
            legend.text=element_text(size=8))
  ,
  
    ggplot(data= h_data,mapping = aes(x=time,y=hourly_calories,fill = hourly_calories))
    + geom_col()
    + geom_smooth()
    + labs(title="Hourly calories burnt",x="",y="")
    + scale_fill_gradient(low = "green", high = "red")
    + theme(plot.title = element_text(hjust = 0.5, size=10, face='bold'),
            axis.text.x = element_text(angle = 90),
            legend.title=element_text(size=9),
            legend.key.height=unit(0.5,'cm'),
            legend.key.width=unit(0.3,'cm'),
            legend.text=element_text(size=8))
    )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Hourly%20patterns-1.png)<!-- -->
Data shows that though average daily steps during 0 AM - 5 AM is
minimal, calories burnt is still high. This shows that body still burn
calories while sleeping.

Hourly steps & hourly calories burnt focus on two time periods:

-   12 PM - 2 PM: which is the time of lunch and afternoon where users
    take a walk for food and do some exercise to burn the calories taken
    or refresh the energy for the afternoon work session.

-   5 PM - 7 PM: which is the time which most people do exercise after
    work.

### 1.3 How do these metrics correlate to each other?

As we found some patterns of correlation earlier between daily/hourly
steps and daily/hourly calories, and we would like to test the
correlation between daily calories and daily sleep time.

Let’s visualize the correlation between these metrics.

#### 1.3.1 Daily/hourly steps and daily/hourly calories

-   Daily

First we would check if daily steps and daily distance traveled
correlate perfectly.

``` r
ggscatter(d_data, x = "daily_steps", y = "daily_distance", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Distance")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20check%20between%20daily%20steps%20and%20daily%20distance-1.png)<!-- -->

With R=1, daily steps correlate perfectly with daily distance. That
means the more steps taken, the longer the distance is.

``` r
ggscatter(d_data, x = "daily_steps", y = "daily_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Calories")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20check%20between%20daily%20steps%20and%20daily%20calories-1.png)<!-- -->

-   Hourly

``` r
ggscatter(h_data, x = "hourly_step", y = "hourly_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hourly Steps", ylab = "Hourly Calories")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20check%20between%20hourly%20calories%20and%20hourly%20calories-1.png)<!-- -->
The visuals show the number of steps and the amount of calories burnt
strongly correlate, especially considering hourly figures.

#### 1.3.2 Daily steps and daily sleep time

``` r
ggscatter(d_data, x = "daily_steps", y = "daily_sleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Sleep Time")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20check%20between%20daily%20steps%20and%20daily%20sleep%20time-1.png)<!-- -->
The chart shows that daily steps taken and daily sleep time have a
slightly negative correlation to each other. The more steps taken, the
less time users sleep. However, the quality of sleep might be another
factor to consider about, which we do not have enough data about this
matter and the lack of sampling size. Since then, we will conclude that
daily steps barely have any effect on daily sleep time, hence daily
calories has an insignificant effect on daily sleep.

## 2. How could these trends apply to Bellabeat customers? And to one of Bellabeat’s products?

### 2.1 What are the user segments available in the market?

#### 2.1.1 By usage

Now that we have seen some basic understanding about sleep time,
calories burnt, and distance traveled, we want to see how often do the
users in our sample use their device. That way we can plan our marketing
strategy and see what features would benefit the use of smart devices.

We will calculate the number of users that use their smart device on a
daily basis, classifying our sample into three categories knowing that
the date interval is 31 days:

-   high use - users who use their device between 21 and 31 days.

-   moderate use - users who use their device between 10 and 20 days.

-   low use - users who use their device between 1 and 10 days.

First we will create a new data frame grouping by Id, calculating number
of days used and creating a new column with the classification explained
above.

``` r
usage_data <- d_total %>% 
  group_by(id) %>% 
  summarize(days_used = sum(n())) %>% 
  mutate(usage= case_when(
      days_used >=1 & days_used <=10 ~ "low use",
      days_used >=11 & days_used <=20 ~ "medium use",
      days_used >=21 & days_used <= 31 ~ "high use"
          ))
head(usage_data)
```

    ## # A tibble: 6 × 3
    ##           id days_used usage   
    ##        <dbl>     <int> <chr>   
    ## 1 1503960366        25 high use
    ## 2 1644430081         4 low use 
    ## 3 1844505072         3 low use 
    ## 4 1927972279         5 low use 
    ## 5 2026352035        28 high use
    ## 6 2320127002         1 low use

Then, we want to summarize the percentage of each usage group to better
visualize the data.

``` r
usage_percentage <- usage_data %>% 
  mutate(usage = factor(usage, levels = c("high use", "medium use", "low use"))) %>% 
  group_by(usage) %>% 
  summarize(total=sum(n())) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(usage) %>% 
  summarize(percentage=total/totals) %>% 
  mutate(label = percent(percentage))

head(usage_percentage)
```

    ## # A tibble: 3 × 3
    ##   usage      percentage label
    ##   <fct>           <dbl> <chr>
    ## 1 high use        0.5   50%  
    ## 2 medium use      0.125 12%  
    ## 3 low use         0.375 38%

``` r
ggplot(data=usage_percentage,mapping = aes(x="",y=percentage,fill=usage))+ 
  geom_bar(stat="identity", width = 1)+ 
  coord_polar("y", start=0)+ 
  labs(title="Daily use of smart device")+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"))+
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"),
                    labels = c("High use - 21 to 31 days",
                                 "Moderate use - 11 to 20 days",
                                 "Low use - 1 to 10 days"))
```

![](Challenge2_DataVisualization_files/figure-gfm/Usage%20percentage-1.png)<!-- -->

With the created visual, we can see:

-   50% of the users in our sample use the devices frequently - 21 to 30
    days.
-   12% use 11 to 20 days.
-   38% rarely use their devices.

We also want to look more at how many minutes users wear their devices
everyday and see how that matches with the usage we found out earlier.

``` r
daily_usage <- merge(d_total,usage_data,by = c("id"))
head(daily_usage)
```

    ##           id X       date totalsteps totaldistance trackerdistance
    ## 1 1503960366 1 2016-04-12      13162          8.50            8.50
    ## 2 1503960366 2 2016-04-13      10735          6.97            6.97
    ## 3 1503960366 3 2016-04-15       9762          6.28            6.28
    ## 4 1503960366 4 2016-04-16      12669          8.16            8.16
    ## 5 1503960366 5 2016-04-17       9705          6.48            6.48
    ## 6 1503960366 6 2016-04-19      15506          9.88            9.88
    ##   loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                        0               1.88                     0.55
    ## 2                        0               1.57                     0.69
    ## 3                        0               2.14                     1.26
    ## 4                        0               2.71                     0.41
    ## 5                        0               3.19                     0.78
    ## 6                        0               3.53                     1.32
    ##   lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                6.06                       0                25
    ## 2                4.71                       0                21
    ## 3                2.83                       0                29
    ## 4                5.04                       0                36
    ## 5                2.51                       0                38
    ## 6                5.03                       0                50
    ##   fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                  13                  328              728     1985
    ## 2                  19                  217              776     1797
    ## 3                  34                  209              726     1745
    ## 4                  10                  221              773     1863
    ## 5                  20                  164              539     1728
    ## 6                  31                  264              775     2035
    ##   totalsleeprecords totalminutesasleep totaltimeinbed days_used    usage
    ## 1                 1                327            346        25 high use
    ## 2                 2                384            407        25 high use
    ## 3                 1                412            442        25 high use
    ## 4                 2                340            367        25 high use
    ## 5                 1                700            712        25 high use
    ## 6                 1                304            320        25 high use

We need to create a data frame calculating the total amount of minutes
users wore the device (by total minutes tracked = very active minutes +
fairly active minutes + lightly active minutes + sedentary minutes) and
create 3 different categories:

-   All day: devices was worn all day.

-   More than half day - device was worn more than half of the

-   Less than half day - device was worn less than half of the day.

``` r
minute_worn <- daily_usage %>% 
  mutate(total_worn = veryactiveminutes + fairlyactiveminutes + lightlyactiveminutes + sedentaryminutes) %>% 
  mutate(minute_worn_percentage = total_worn/(24*60)*100) %>% 
  mutate(worn=case_when(
    minute_worn_percentage == 100 ~ "All day",
    minute_worn_percentage <100 & minute_worn_percentage >= 50 ~ "More than half day",
    minute_worn_percentage <50 ~ "Less than half day"
  ))

head(minute_worn, n=300)
```

    ##             id   X       date totalsteps totaldistance trackerdistance
    ## 1   1503960366   1 2016-04-12      13162          8.50            8.50
    ## 2   1503960366   2 2016-04-13      10735          6.97            6.97
    ## 3   1503960366   3 2016-04-15       9762          6.28            6.28
    ## 4   1503960366   4 2016-04-16      12669          8.16            8.16
    ## 5   1503960366   5 2016-04-17       9705          6.48            6.48
    ## 6   1503960366   6 2016-04-19      15506          9.88            9.88
    ## 7   1503960366   7 2016-04-20      10544          6.68            6.68
    ## 8   1503960366   8 2016-04-21       9819          6.34            6.34
    ## 9   1503960366   9 2016-04-23      14371          9.04            9.04
    ## 10  1503960366  10 2016-04-24      10039          6.41            6.41
    ## 11  1503960366  11 2016-04-25      15355          9.80            9.80
    ## 12  1503960366  12 2016-04-26      13755          8.79            8.79
    ## 13  1503960366  13 2016-04-28      13154          8.53            8.53
    ## 14  1503960366  14 2016-04-29      11181          7.15            7.15
    ## 15  1503960366  15 2016-04-30      14673          9.25            9.25
    ## 16  1503960366  16 2016-05-01      10602          6.81            6.81
    ## 17  1503960366  17 2016-05-02      14727          9.71            9.71
    ## 18  1503960366  18 2016-05-03      15103          9.66            9.66
    ## 19  1503960366  19 2016-05-05      14070          8.90            8.90
    ## 20  1503960366  20 2016-05-06      12159          8.03            8.03
    ## 21  1503960366  21 2016-05-07      11992          7.71            7.71
    ## 22  1503960366  22 2016-05-08      10060          6.58            6.58
    ## 23  1503960366  23 2016-05-09      12022          7.72            7.72
    ## 24  1503960366  24 2016-05-10      12207          7.77            7.77
    ## 25  1503960366  25 2016-05-11      12770          8.13            8.13
    ## 26  1644430081  26 2016-04-29       3176          2.31            2.31
    ## 27  1644430081  27 2016-04-30      18213         13.24           13.24
    ## 28  1644430081  28 2016-05-02       3758          2.73            2.73
    ## 29  1644430081  29 2016-05-08       6724          4.89            4.89
    ## 30  1844505072  30 2016-04-15       3844          2.54            2.54
    ## 31  1844505072  31 2016-04-30       4014          2.67            2.67
    ## 32  1844505072  32 2016-05-01       2573          1.70            1.70
    ## 33  1927972279  33 2016-04-12        678          0.47            0.47
    ## 34  1927972279  34 2016-04-13        356          0.25            0.25
    ## 35  1927972279  35 2016-04-15        980          0.68            0.68
    ## 36  1927972279  36 2016-04-26       3761          2.60            2.60
    ## 37  1927972279  37 2016-04-28       1675          1.16            1.16
    ## 38  2026352035  38 2016-04-12       4414          2.74            2.74
    ## 39  2026352035  39 2016-04-13       4993          3.10            3.10
    ## 40  2026352035  40 2016-04-14       3335          2.07            2.07
    ## 41  2026352035  41 2016-04-15       3821          2.37            2.37
    ## 42  2026352035  42 2016-04-16       2547          1.58            1.58
    ## 43  2026352035  43 2016-04-17        838          0.52            0.52
    ## 44  2026352035  44 2016-04-19       2424          1.50            1.50
    ## 45  2026352035  45 2016-04-20       7222          4.48            4.48
    ## 46  2026352035  46 2016-04-21       2467          1.53            1.53
    ## 47  2026352035  47 2016-04-22       2915          1.81            1.81
    ## 48  2026352035  48 2016-04-23      12357          7.71            7.71
    ## 49  2026352035  49 2016-04-24       3490          2.16            2.16
    ## 50  2026352035  50 2016-04-25       6017          3.73            3.73
    ## 51  2026352035  51 2016-04-27       6088          3.77            3.77
    ## 52  2026352035  52 2016-04-28       6375          3.95            3.95
    ## 53  2026352035  53 2016-04-29       7604          4.71            4.71
    ## 54  2026352035  54 2016-04-30       4729          2.93            2.93
    ## 55  2026352035  55 2016-05-01       3609          2.28            2.28
    ## 56  2026352035  56 2016-05-02       7018          4.35            4.35
    ## 57  2026352035  57 2016-05-04       6564          4.07            4.07
    ## 58  2026352035  58 2016-05-05      12167          7.54            7.54
    ## 59  2026352035  59 2016-05-06       8198          5.08            5.08
    ## 60  2026352035  60 2016-05-07       4193          2.60            2.60
    ## 61  2026352035  61 2016-05-08       5528          3.45            3.45
    ## 62  2026352035  62 2016-05-09      10685          6.62            6.62
    ## 63  2026352035  63 2016-05-10        254          0.16            0.16
    ## 64  2026352035  64 2016-05-11       8580          5.32            5.32
    ## 65  2026352035  65 2016-05-12       8891          5.51            5.51
    ## 66  2320127002  66 2016-04-23       5079          3.42            3.42
    ## 67  2347167796  67 2016-04-13      10352          7.01            7.01
    ## 68  2347167796  68 2016-04-14      10129          6.70            6.70
    ## 69  2347167796  69 2016-04-15      10465          6.92            6.92
    ## 70  2347167796  70 2016-04-17       5472          3.62            3.62
    ## 71  2347167796  71 2016-04-18       8247          5.45            5.45
    ## 72  2347167796  72 2016-04-19       6711          4.44            4.44
    ## 73  2347167796  73 2016-04-21      10080          6.75            6.75
    ## 74  2347167796  74 2016-04-22       7804          5.16            5.16
    ## 75  2347167796  75 2016-04-23      16901         11.37           11.37
    ## 76  2347167796  76 2016-04-24       9471          6.26            6.26
    ## 77  2347167796  77 2016-04-25       9482          6.38            6.38
    ## 78  2347167796  78 2016-04-26       5980          3.95            3.95
    ## 79  2347167796  79 2016-04-27      11423          7.58            7.58
    ## 80  2347167796  80 2016-04-28       5439          3.60            3.60
    ## 81  2347167796  81 2016-04-29         42          0.03            0.03
    ## 82  3977333714  82 2016-04-12       8856          5.98            5.98
    ## 83  3977333714  83 2016-04-13      10035          6.71            6.71
    ## 84  3977333714  84 2016-04-14       7641          5.11            5.11
    ## 85  3977333714  85 2016-04-15       9010          6.06            6.06
    ## 86  3977333714  86 2016-04-16      13459          9.00            9.00
    ## 87  3977333714  87 2016-04-17      10415          6.97            6.97
    ## 88  3977333714  88 2016-04-18      11663          7.80            7.80
    ## 89  3977333714  89 2016-04-19      12414          8.78            8.78
    ## 90  3977333714  90 2016-04-20      11658          7.83            7.83
    ## 91  3977333714  91 2016-04-21       6093          4.08            4.08
    ## 92  3977333714  92 2016-04-22       8911          5.96            5.96
    ## 93  3977333714  93 2016-04-23      12058          8.07            8.07
    ## 94  3977333714  94 2016-04-24      14112         10.00           10.00
    ## 95  3977333714  95 2016-04-25      11177          8.48            8.48
    ## 96  3977333714  96 2016-04-26      11388          7.62            7.62
    ## 97  3977333714  97 2016-04-27       7193          5.04            5.04
    ## 98  3977333714  98 2016-04-28       7114          4.88            4.88
    ## 99  3977333714  99 2016-04-29      10645          7.75            7.75
    ## 100 3977333714 100 2016-04-30      13238          9.20            9.20
    ## 101 3977333714 101 2016-05-01      10414          7.07            7.07
    ## 102 3977333714 102 2016-05-02      16520         11.05           11.05
    ## 103 3977333714 103 2016-05-03      14335          9.59            9.59
    ## 104 3977333714 104 2016-05-04      13559          9.44            9.44
    ## 105 3977333714 105 2016-05-05      12312          8.58            8.58
    ## 106 3977333714 106 2016-05-06      11677          8.28            8.28
    ## 107 3977333714 107 2016-05-07      11550          7.73            7.73
    ## 108 3977333714 108 2016-05-08      13585          9.09            9.09
    ## 109 3977333714 109 2016-05-10      13072          8.78            8.78
    ## 110 4020332650 110 2016-04-12       8539          6.12            6.12
    ## 111 4020332650 111 2016-04-16       1982          1.42            1.42
    ## 112 4020332650 112 2016-05-03       4496          3.22            3.22
    ## 113 4020332650 113 2016-05-04      10252          7.35            7.35
    ## 114 4020332650 114 2016-05-05      11728          8.43            8.43
    ## 115 4020332650 115 2016-05-06       4369          3.13            3.13
    ## 116 4020332650 116 2016-05-08       5862          4.20            4.20
    ## 117 4020332650 117 2016-05-10       5546          3.98            3.98
    ## 118 4319703577 118 2016-04-14      10210          6.88            6.88
    ## 119 4319703577 119 2016-04-15       5664          3.80            3.80
    ## 120 4319703577 120 2016-04-16       4744          3.18            3.18
    ## 121 4319703577 121 2016-04-18       2276          1.55            1.55
    ## 122 4319703577 122 2016-04-19       8925          5.99            5.99
    ## 123 4319703577 123 2016-04-20       8954          6.01            6.01
    ## 124 4319703577 124 2016-04-21       3702          2.48            2.48
    ## 125 4319703577 125 2016-04-22       4500          3.02            3.02
    ## 126 4319703577 126 2016-04-23       4935          3.31            3.31
    ## 127 4319703577 127 2016-04-24       4081          2.74            2.74
    ## 128 4319703577 128 2016-04-25       9259          6.21            6.21
    ## 129 4319703577 129 2016-04-26       9899          6.64            6.64
    ## 130 4319703577 130 2016-04-27      10780          7.23            7.23
    ## 131 4319703577 131 2016-04-28      10817          7.28            7.28
    ## 132 4319703577 132 2016-04-29       7990          5.36            5.36
    ## 133 4319703577 133 2016-04-30       8221          5.52            5.52
    ## 134 4319703577 134 2016-05-01       1251          0.84            0.84
    ## 135 4319703577 135 2016-05-02       9261          6.24            6.24
    ## 136 4319703577 136 2016-05-03       9648          6.47            6.47
    ## 137 4319703577 137 2016-05-06       9524          6.42            6.42
    ## 138 4319703577 138 2016-05-07       7937          5.33            5.33
    ## 139 4319703577 139 2016-05-08       3672          2.46            2.46
    ## 140 4319703577 140 2016-05-09      10378          6.96            6.96
    ## 141 4319703577 141 2016-05-10       9487          6.37            6.37
    ## 142 4319703577 142 2016-05-11       9129          6.13            6.13
    ## 143 4319703577 143 2016-05-12         17          0.01            0.01
    ## 144 4388161847 144 2016-04-15       8758          6.73            6.73
    ## 145 4388161847 145 2016-04-16       6580          5.06            5.06
    ## 146 4388161847 146 2016-04-17       4660          3.58            3.58
    ## 147 4388161847 147 2016-04-18      11009          9.10            9.10
    ## 148 4388161847 148 2016-04-19      10181          7.83            7.83
    ## 149 4388161847 149 2016-04-20      10553          8.12            8.12
    ## 150 4388161847 150 2016-04-21      10055          7.73            7.73
    ## 151 4388161847 151 2016-04-22      12139          9.34            9.34
    ## 152 4388161847 152 2016-04-23      13236         10.18           10.18
    ## 153 4388161847 153 2016-04-24      10243          7.88            7.88
    ## 154 4388161847 154 2016-04-26       9461          7.28            7.28
    ## 155 4388161847 155 2016-04-27      11193          8.61            8.61
    ## 156 4388161847 156 2016-04-28      10074          7.75            7.75
    ## 157 4388161847 157 2016-04-30      12533          9.64            9.64
    ## 158 4388161847 158 2016-05-01      10255          7.89            7.89
    ## 159 4388161847 159 2016-05-02      10096          8.40            8.40
    ## 160 4388161847 160 2016-05-04      12375          9.52            9.52
    ## 161 4388161847 161 2016-05-05       9603          7.38            7.38
    ## 162 4388161847 162 2016-05-07      22770         17.54           17.54
    ## 163 4388161847 163 2016-05-08      17298         14.38           14.38
    ## 164 4388161847 164 2016-05-09      10218          7.86            7.86
    ## 165 4388161847 165 2016-05-10      10299          7.92            7.92
    ## 166 4388161847 166 2016-05-11      10201          7.84            7.84
    ## 167 4445114986 167 2016-04-12       3276          2.20            2.20
    ## 168 4445114986 168 2016-04-13       2961          1.99            1.99
    ## 169 4445114986 169 2016-04-14       3974          2.67            2.67
    ## 170 4445114986 170 2016-04-15       7198          4.83            4.83
    ## 171 4445114986 171 2016-04-16       3945          2.65            2.65
    ## 172 4445114986 172 2016-04-17       2268          1.52            1.52
    ## 173 4445114986 173 2016-04-19       2064          1.39            1.39
    ## 174 4445114986 174 2016-04-20       2072          1.39            1.39
    ## 175 4445114986 175 2016-04-21       3809          2.56            2.56
    ## 176 4445114986 176 2016-04-22       6831          4.58            4.58
    ## 177 4445114986 177 2016-04-25       3385          2.27            2.27
    ## 178 4445114986 178 2016-04-26       6326          4.41            4.41
    ## 179 4445114986 179 2016-04-27       7243          5.03            5.03
    ## 180 4445114986 180 2016-04-28       4493          3.01            3.01
    ## 181 4445114986 181 2016-04-29       4676          3.14            3.14
    ## 182 4445114986 182 2016-04-30       6222          4.18            4.18
    ## 183 4445114986 183 2016-05-01       5232          3.51            3.51
    ## 184 4445114986 184 2016-05-02       6910          4.75            4.75
    ## 185 4445114986 185 2016-05-03       7502          5.18            5.18
    ## 186 4445114986 186 2016-05-04       2923          1.96            1.96
    ## 187 4445114986 187 2016-05-05       3800          2.55            2.55
    ## 188 4445114986 188 2016-05-06       4514          3.03            3.03
    ## 189 4445114986 189 2016-05-07       5183          3.59            3.59
    ## 190 4445114986 190 2016-05-08       7303          4.90            4.90
    ## 191 4445114986 191 2016-05-09       5275          3.54            3.54
    ## 192 4445114986 192 2016-05-10       3915          2.63            2.63
    ## 193 4445114986 193 2016-05-11       9105          6.11            6.11
    ## 194 4445114986 194 2016-05-12        768          0.52            0.52
    ## 195 4558609924 195 2016-04-21      13743          9.08            9.08
    ## 196 4558609924 196 2016-04-26       9148          6.05            6.05
    ## 197 4558609924 197 2016-04-29       7833          5.18            5.18
    ## 198 4558609924 198 2016-05-01       3428          2.27            2.27
    ## 199 4558609924 199 2016-05-08       6543          4.33            4.33
    ## 200 4702921684 200 2016-04-12       7213          5.88            5.88
    ## 201 4702921684 201 2016-04-13       6877          5.58            5.58
    ## 202 4702921684 202 2016-04-14       7860          6.37            6.37
    ## 203 4702921684 203 2016-04-15       6506          5.28            5.28
    ## 204 4702921684 204 2016-04-16      11140          9.03            9.03
    ## 205 4702921684 205 2016-04-17      12692         10.29           10.29
    ## 206 4702921684 206 2016-04-18       9105          7.38            7.38
    ## 207 4702921684 207 2016-04-19       6708          5.44            5.44
    ## 208 4702921684 208 2016-04-20       8793          7.13            7.13
    ## 209 4702921684 209 2016-04-21       6530          5.30            5.30
    ## 210 4702921684 210 2016-04-23      15126         12.27           12.27
    ## 211 4702921684 211 2016-04-24      15050         12.22           12.22
    ## 212 4702921684 212 2016-04-25       9167          7.43            7.43
    ## 213 4702921684 213 2016-04-26       6108          4.95            4.95
    ## 214 4702921684 214 2016-04-27       7047          5.72            5.72
    ## 215 4702921684 215 2016-04-28       9023          7.32            7.32
    ## 216 4702921684 216 2016-04-29       9930          8.05            8.05
    ## 217 4702921684 217 2016-04-30      10144          8.23            8.23
    ## 218 4702921684 218 2016-05-03       9454          7.67            7.67
    ## 219 4702921684 219 2016-05-04       8161          6.62            6.62
    ## 220 4702921684 220 2016-05-05       8614          6.99            6.99
    ## 221 4702921684 221 2016-05-06       6943          5.63            5.63
    ## 222 4702921684 222 2016-05-07      14370         11.65           11.65
    ## 223 4702921684 223 2016-05-09       8232          6.68            6.68
    ## 224 4702921684 224 2016-05-10      10613          8.61            8.61
    ## 225 4702921684 225 2016-05-11       9810          7.96            7.96
    ## 226 4702921684 226 2016-05-12       2752          2.23            2.23
    ## 227 5553957443 227 2016-04-12      11596          7.57            7.57
    ## 228 5553957443 228 2016-04-13       4832          3.16            3.16
    ## 229 5553957443 229 2016-04-14      17022         11.12           11.12
    ## 230 5553957443 230 2016-04-15      16556         10.86           10.86
    ## 231 5553957443 231 2016-04-16       5771          3.77            3.77
    ## 232 5553957443 232 2016-04-17        655          0.43            0.43
    ## 233 5553957443 233 2016-04-18       3727          2.43            2.43
    ## 234 5553957443 234 2016-04-19      15482         10.11           10.11
    ## 235 5553957443 235 2016-04-20       2713          1.77            1.77
    ## 236 5553957443 236 2016-04-21      12346          8.06            8.06
    ## 237 5553957443 237 2016-04-22      11682          7.63            7.63
    ## 238 5553957443 238 2016-04-23       4112          2.69            2.69
    ## 239 5553957443 239 2016-04-24       1807          1.18            1.18
    ## 240 5553957443 240 2016-04-25      10946          7.19            7.19
    ## 241 5553957443 241 2016-04-26      11886          7.76            7.76
    ## 242 5553957443 242 2016-04-27      10538          6.88            6.88
    ## 243 5553957443 243 2016-04-28      11393          7.63            7.63
    ## 244 5553957443 244 2016-04-29      12764          8.33            8.33
    ## 245 5553957443 245 2016-04-30       1202          0.78            0.78
    ## 246 5553957443 246 2016-05-01       5164          3.37            3.37
    ## 247 5553957443 247 2016-05-02       9769          6.38            6.38
    ## 248 5553957443 248 2016-05-03      12848          8.39            8.39
    ## 249 5553957443 249 2016-05-04       4249          2.77            2.77
    ## 250 5553957443 250 2016-05-05      14331          9.51            9.51
    ## 251 5553957443 251 2016-05-06       9632          6.29            6.29
    ## 252 5553957443 252 2016-05-07       1868          1.22            1.22
    ## 253 5553957443 253 2016-05-08       6083          4.00            4.00
    ## 254 5553957443 254 2016-05-09      11611          7.58            7.58
    ## 255 5553957443 255 2016-05-10      16358         10.71           10.71
    ## 256 5553957443 256 2016-05-11       4926          3.22            3.22
    ## 257 5553957443 257 2016-05-12       3121          2.04            2.04
    ## 258 5577150313 258 2016-04-12       8135          6.08            6.08
    ## 259 5577150313 259 2016-04-13       5077          3.79            3.79
    ## 260 5577150313 260 2016-04-14       8596          6.42            6.42
    ## 261 5577150313 261 2016-04-15      12087          9.08            9.08
    ## 262 5577150313 262 2016-04-16      14269         10.66           10.66
    ## 263 5577150313 263 2016-04-17      12231          9.14            9.14
    ## 264 5577150313 264 2016-04-18       9893          7.39            7.39
    ## 265 5577150313 265 2016-04-19      12574          9.42            9.42
    ## 266 5577150313 266 2016-04-20       8330          6.22            6.22
    ## 267 5577150313 267 2016-04-21      10830          8.09            8.09
    ## 268 5577150313 268 2016-04-22       9172          6.85            6.85
    ## 269 5577150313 269 2016-04-23       7638          5.71            5.71
    ## 270 5577150313 270 2016-04-24      15764         11.78           11.78
    ## 271 5577150313 271 2016-04-25       6393          4.78            4.78
    ## 272 5577150313 272 2016-04-26       5325          3.98            3.98
    ## 273 5577150313 273 2016-04-27       6805          5.14            5.14
    ## 274 5577150313 274 2016-04-28       9841          7.43            7.43
    ## 275 5577150313 275 2016-04-29       7924          5.92            5.92
    ## 276 5577150313 276 2016-04-30      12363          9.24            9.24
    ## 277 5577150313 277 2016-05-01      13368          9.99            9.99
    ## 278 5577150313 278 2016-05-02       7439          5.56            5.56
    ## 279 5577150313 279 2016-05-03      11045          8.25            8.25
    ## 280 5577150313 280 2016-05-04       5206          3.89            3.89
    ## 281 5577150313 281 2016-05-05       7550          5.64            5.64
    ## 282 5577150313 282 2016-05-10       8869          6.65            6.65
    ## 283 5577150313 283 2016-05-11       4038          3.04            3.04
    ## 284 6117666160 284 2016-04-16      14450         10.91           10.91
    ## 285 6117666160 285 2016-04-17       7150          5.40            5.40
    ## 286 6117666160 286 2016-04-18       5153          3.91            3.91
    ## 287 6117666160 287 2016-04-19      11135          8.41            8.41
    ## 288 6117666160 288 2016-04-20      10449          8.02            8.02
    ## 289 6117666160 289 2016-04-21      19542         15.01           15.01
    ## 290 6117666160 290 2016-04-22       8206          6.20            6.20
    ## 291 6117666160 291 2016-04-23      11495          8.68            8.68
    ## 292 6117666160 292 2016-04-24       7623          5.76            5.76
    ## 293 6117666160 293 2016-04-27       9411          7.11            7.11
    ## 294 6117666160 294 2016-04-28       3403          2.60            2.60
    ## 295 6117666160 295 2016-04-29       9592          7.24            7.24
    ## 296 6117666160 296 2016-05-01       8915          6.73            6.73
    ## 297 6117666160 297 2016-05-05       9799          7.40            7.40
    ## 298 6117666160 298 2016-05-06       3365          2.68            2.68
    ## 299 6117666160 299 2016-05-07       7336          5.54            5.54
    ## 300 6117666160 300 2016-05-08       7328          5.53            5.53
    ##     loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                          0               1.88                     0.55
    ## 2                          0               1.57                     0.69
    ## 3                          0               2.14                     1.26
    ## 4                          0               2.71                     0.41
    ## 5                          0               3.19                     0.78
    ## 6                          0               3.53                     1.32
    ## 7                          0               1.96                     0.48
    ## 8                          0               1.34                     0.35
    ## 9                          0               2.81                     0.87
    ## 10                         0               2.92                     0.21
    ## 11                         0               5.29                     0.57
    ## 12                         0               2.33                     0.92
    ## 13                         0               3.54                     1.16
    ## 14                         0               1.06                     0.50
    ## 15                         0               3.56                     1.42
    ## 16                         0               2.29                     1.60
    ## 17                         0               3.21                     0.57
    ## 18                         0               3.73                     1.05
    ## 19                         0               2.92                     1.08
    ## 20                         0               1.97                     0.25
    ## 21                         0               2.46                     2.12
    ## 22                         0               3.53                     0.32
    ## 23                         0               3.45                     0.53
    ## 24                         0               3.35                     1.16
    ## 25                         0               2.56                     1.01
    ## 26                         0               0.00                     0.00
    ## 27                         0               0.63                     3.14
    ## 28                         0               0.07                     0.31
    ## 29                         0               0.00                     0.00
    ## 30                         0               0.00                     0.00
    ## 31                         0               0.00                     0.00
    ## 32                         0               0.00                     0.26
    ## 33                         0               0.00                     0.00
    ## 34                         0               0.00                     0.00
    ## 35                         0               0.00                     0.00
    ## 36                         0               0.00                     0.00
    ## 37                         0               0.00                     0.00
    ## 38                         0               0.19                     0.35
    ## 39                         0               0.00                     0.00
    ## 40                         0               0.00                     0.00
    ## 41                         0               0.00                     0.00
    ## 42                         0               0.00                     0.00
    ## 43                         0               0.00                     0.00
    ## 44                         0               0.00                     0.00
    ## 45                         0               0.00                     0.00
    ## 46                         0               0.00                     0.00
    ## 47                         0               0.00                     0.00
    ## 48                         0               0.00                     0.00
    ## 49                         0               0.00                     0.00
    ## 50                         0               0.00                     0.00
    ## 51                         0               0.00                     0.00
    ## 52                         0               0.00                     0.00
    ## 53                         0               0.00                     0.00
    ## 54                         0               0.00                     0.00
    ## 55                         0               0.00                     0.00
    ## 56                         0               0.00                     0.00
    ## 57                         0               0.00                     0.00
    ## 58                         0               0.00                     0.00
    ## 59                         0               0.00                     0.00
    ## 60                         0               0.00                     0.00
    ## 61                         0               0.00                     0.00
    ## 62                         0               0.00                     0.00
    ## 63                         0               0.00                     0.00
    ## 64                         0               0.00                     0.00
    ## 65                         0               0.00                     0.00
    ## 66                         0               0.00                     0.00
    ## 67                         0               1.66                     1.94
    ## 68                         0               0.02                     2.74
    ## 69                         0               0.07                     1.42
    ## 70                         0               0.08                     0.28
    ## 71                         0               0.79                     0.86
    ## 72                         0               0.00                     0.00
    ## 73                         0               1.85                     1.53
    ## 74                         0               0.56                     1.68
    ## 75                         0               2.78                     1.45
    ## 76                         0               0.00                     0.00
    ## 77                         0               1.27                     0.52
    ## 78                         0               0.00                     0.00
    ## 79                         0               1.86                     0.40
    ## 80                         0               0.00                     0.00
    ## 81                         0               0.00                     0.00
    ## 82                         0               3.06                     0.91
    ## 83                         0               2.03                     2.13
    ## 84                         0               0.32                     0.97
    ## 85                         0               1.05                     1.75
    ## 86                         0               2.03                     4.00
    ## 87                         0               0.70                     2.35
    ## 88                         0               0.25                     3.73
    ## 89                         0               2.24                     2.45
    ## 90                         0               0.20                     4.35
    ## 91                         0               0.00                     0.00
    ## 92                         0               2.33                     0.58
    ## 93                         0               0.00                     4.22
    ## 94                         0               3.27                     4.56
    ## 95                         0               5.62                     0.43
    ## 96                         0               0.45                     4.22
    ## 97                         0               0.00                     0.42
    ## 98                         0               1.37                     0.29
    ## 99                         0               3.74                     1.30
    ## 100                        0               3.69                     2.10
    ## 101                        0               2.67                     1.98
    ## 102                        0               1.54                     6.48
    ## 103                        0               3.32                     1.74
    ## 104                        0               1.81                     4.58
    ## 105                        0               1.76                     4.11
    ## 106                        0               3.11                     2.51
    ## 107                        0               0.00                     4.13
    ## 108                        0               0.68                     5.24
    ## 109                        0               0.07                     5.40
    ## 110                        0               0.15                     0.24
    ## 111                        0               0.45                     0.37
    ## 112                        0               0.00                     0.00
    ## 113                        0               0.67                     1.04
    ## 114                        0               2.62                     1.68
    ## 115                        0               0.00                     0.00
    ## 116                        0               0.00                     0.00
    ## 117                        0               0.00                     0.00
    ## 118                        0               0.11                     0.33
    ## 119                        0               0.00                     0.00
    ## 120                        0               0.00                     0.00
    ## 121                        0               0.07                     0.33
    ## 122                        0               0.00                     0.00
    ## 123                        0               0.00                     0.68
    ## 124                        0               0.00                     0.00
    ## 125                        0               0.06                     0.81
    ## 126                        0               0.00                     0.00
    ## 127                        0               0.06                     0.20
    ## 128                        0               0.00                     0.28
    ## 129                        0               0.57                     0.92
    ## 130                        0               0.41                     1.92
    ## 131                        0               1.01                     0.33
    ## 132                        0               0.45                     0.79
    ## 133                        0               0.40                     1.61
    ## 134                        0               0.00                     0.00
    ## 135                        0               0.00                     0.44
    ## 136                        0               0.58                     1.07
    ## 137                        0               0.41                     0.47
    ## 138                        0               0.19                     1.05
    ## 139                        0               0.00                     0.00
    ## 140                        0               0.14                     0.56
    ## 141                        0               0.21                     0.46
    ## 142                        0               0.20                     0.74
    ## 143                        0               0.00                     0.00
    ## 144                        0               0.00                     0.00
    ## 145                        0               0.21                     0.40
    ## 146                        0               0.00                     0.00
    ## 147                        0               3.56                     0.40
    ## 148                        0               1.37                     0.69
    ## 149                        0               1.10                     1.72
    ## 150                        0               0.37                     0.39
    ## 151                        0               3.30                     1.11
    ## 152                        0               4.50                     0.32
    ## 153                        0               1.08                     0.51
    ## 154                        0               0.94                     1.06
    ## 155                        0               0.70                     2.51
    ## 156                        0               1.29                     0.43
    ## 157                        0               0.70                     2.00
    ## 158                        0               1.01                     0.68
    ## 159                        0               3.77                     0.08
    ## 160                        0               2.79                     0.93
    ## 161                        0               0.63                     1.67
    ## 162                        0               9.45                     2.77
    ## 163                        0               9.89                     1.26
    ## 164                        0               0.34                     0.73
    ## 165                        0               0.81                     0.65
    ## 166                        0               0.53                     0.79
    ## 167                        0               0.00                     0.00
    ## 168                        0               0.00                     0.00
    ## 169                        0               0.00                     0.00
    ## 170                        0               0.00                     0.00
    ## 171                        0               0.00                     0.00
    ## 172                        0               0.00                     0.00
    ## 173                        0               0.00                     0.00
    ## 174                        0               0.00                     0.00
    ## 175                        0               0.00                     0.00
    ## 176                        0               0.00                     0.00
    ## 177                        0               0.00                     0.00
    ## 178                        0               2.41                     0.04
    ## 179                        0               2.62                     0.03
    ## 180                        0               0.00                     0.00
    ## 181                        0               0.00                     0.00
    ## 182                        0               0.00                     0.00
    ## 183                        0               0.00                     0.00
    ## 184                        0               2.21                     0.19
    ## 185                        0               2.48                     0.11
    ## 186                        0               0.00                     0.00
    ## 187                        0               0.12                     0.24
    ## 188                        0               0.00                     0.00
    ## 189                        0               2.13                     0.19
    ## 190                        0               0.00                     0.25
    ## 191                        0               0.00                     0.00
    ## 192                        0               0.00                     0.00
    ## 193                        0               2.25                     1.00
    ## 194                        0               0.00                     0.00
    ## 195                        0               0.42                     0.97
    ## 196                        0               0.43                     2.03
    ## 197                        0               1.02                     1.85
    ## 198                        0               0.00                     0.00
    ## 199                        0               1.80                     0.50
    ## 200                        0               0.00                     0.00
    ## 201                        0               0.00                     0.00
    ## 202                        0               0.00                     0.00
    ## 203                        0               0.07                     0.42
    ## 204                        0               0.24                     1.25
    ## 205                        0               0.96                     3.46
    ## 206                        0               1.82                     1.49
    ## 207                        0               0.88                     0.37
    ## 208                        0               0.16                     1.23
    ## 209                        0               0.31                     2.05
    ## 210                        0               0.76                     3.24
    ## 211                        0               1.20                     5.12
    ## 212                        0               0.49                     0.82
    ## 213                        0               0.07                     0.35
    ## 214                        0               0.09                     0.80
    ## 215                        0               1.13                     0.42
    ## 216                        0               1.06                     0.92
    ## 217                        0               0.32                     2.03
    ## 218                        0               0.00                     0.00
    ## 219                        0               0.34                     0.73
    ## 220                        0               0.67                     0.22
    ## 221                        0               0.08                     0.66
    ## 222                        0               0.37                     2.31
    ## 223                        0               0.00                     0.57
    ## 224                        0               0.08                     1.88
    ## 225                        0               0.78                     2.16
    ## 226                        0               0.00                     0.00
    ## 227                        0               1.37                     0.79
    ## 228                        0               0.00                     0.00
    ## 229                        0               4.00                     2.45
    ## 230                        0               4.16                     1.98
    ## 231                        0               0.00                     0.00
    ## 232                        0               0.00                     0.00
    ## 233                        0               0.00                     0.00
    ## 234                        0               4.28                     1.66
    ## 235                        0               0.00                     0.00
    ## 236                        0               2.95                     2.16
    ## 237                        0               1.38                     0.63
    ## 238                        0               0.00                     0.00
    ## 239                        0               0.00                     0.00
    ## 240                        0               2.93                     0.57
    ## 241                        0               2.37                     0.93
    ## 242                        0               1.14                     1.00
    ## 243                        0               3.71                     0.75
    ## 244                        0               2.79                     0.64
    ## 245                        0               0.00                     0.00
    ## 246                        0               0.00                     0.00
    ## 247                        0               1.06                     0.41
    ## 248                        0               1.50                     1.20
    ## 249                        0               0.00                     0.00
    ## 250                        0               3.43                     1.66
    ## 251                        0               1.52                     0.54
    ## 252                        0               0.00                     0.00
    ## 253                        0               0.22                     0.47
    ## 254                        0               2.13                     0.89
    ## 255                        0               3.87                     1.61
    ## 256                        0               0.00                     0.00
    ## 257                        0               0.58                     0.40
    ## 258                        0               3.60                     0.38
    ## 259                        0               0.32                     0.22
    ## 260                        0               3.33                     0.31
    ## 261                        0               3.92                     1.60
    ## 262                        0               6.64                     1.28
    ## 263                        0               5.98                     0.83
    ## 264                        0               4.86                     0.72
    ## 265                        0               7.02                     0.64
    ## 266                        0               4.12                     0.34
    ## 267                        0               3.65                     1.66
    ## 268                        0               2.42                     0.79
    ## 269                        0               1.21                     0.36
    ## 270                        0               7.65                     2.15
    ## 271                        0               1.35                     0.67
    ## 272                        0               0.85                     0.65
    ## 273                        0               1.81                     0.40
    ## 274                        0               3.25                     1.17
    ## 275                        0               2.84                     0.61
    ## 276                        0               5.83                     0.79
    ## 277                        0               5.31                     1.44
    ## 278                        0               1.12                     0.35
    ## 279                        0               4.52                     0.15
    ## 280                        0               1.56                     0.25
    ## 281                        0               2.50                     0.47
    ## 282                        0               2.56                     0.75
    ## 283                        0               1.83                     0.30
    ## 284                        0               0.58                     0.85
    ## 285                        0               0.00                     0.00
    ## 286                        0               0.00                     0.00
    ## 287                        0               0.00                     0.00
    ## 288                        0               2.03                     0.48
    ## 289                        0               0.98                     0.40
    ## 290                        0               0.00                     0.00
    ## 291                        0               0.00                     0.00
    ## 292                        0               0.00                     0.00
    ## 293                        0               0.00                     0.00
    ## 294                        0               0.00                     0.00
    ## 295                        0               0.00                     0.00
    ## 296                        0               0.00                     0.00
    ## 297                        0               0.00                     0.00
    ## 298                        0               0.00                     0.00
    ## 299                        0               0.00                     0.00
    ## 300                        0               0.00                     0.00
    ##     lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                  6.06                    0.00                25
    ## 2                  4.71                    0.00                21
    ## 3                  2.83                    0.00                29
    ## 4                  5.04                    0.00                36
    ## 5                  2.51                    0.00                38
    ## 6                  5.03                    0.00                50
    ## 7                  4.24                    0.00                28
    ## 8                  4.65                    0.00                19
    ## 9                  5.36                    0.00                41
    ## 10                 3.28                    0.00                39
    ## 11                 3.94                    0.00                73
    ## 12                 5.54                    0.00                31
    ## 13                 3.79                    0.00                48
    ## 14                 5.58                    0.00                16
    ## 15                 4.27                    0.00                52
    ## 16                 2.92                    0.00                33
    ## 17                 5.92                    0.00                41
    ## 18                 4.88                    0.00                50
    ## 19                 4.88                    0.00                45
    ## 20                 5.81                    0.00                24
    ## 21                 3.13                    0.00                37
    ## 22                 2.73                    0.00                44
    ## 23                 3.74                    0.00                46
    ## 24                 3.26                    0.00                46
    ## 25                 4.55                    0.00                36
    ## 26                 2.31                    0.00                 0
    ## 27                 9.46                    0.00                 9
    ## 28                 2.35                    0.00                 1
    ## 29                 4.88                    0.00                 0
    ## 30                 2.54                    0.00                 0
    ## 31                 2.65                    0.00                 0
    ## 32                 1.45                    0.00                 0
    ## 33                 0.47                    0.00                 0
    ## 34                 0.25                    0.00                 0
    ## 35                 0.68                    0.00                 0
    ## 36                 2.60                    0.00                 0
    ## 37                 1.16                    0.00                 0
    ## 38                 2.20                    0.00                 3
    ## 39                 3.10                    0.00                 0
    ## 40                 2.05                    0.00                 0
    ## 41                 2.37                    0.00                 0
    ## 42                 1.58                    0.00                 0
    ## 43                 0.52                    0.00                 0
    ## 44                 1.50                    0.00                 0
    ## 45                 4.48                    0.00                 0
    ## 46                 1.53                    0.00                 0
    ## 47                 1.81                    0.00                 0
    ## 48                 7.71                    0.00                 0
    ## 49                 2.16                    0.00                 0
    ## 50                 3.73                    0.00                 0
    ## 51                 3.77                    0.00                 0
    ## 52                 3.95                    0.00                 0
    ## 53                 4.71                    0.00                 0
    ## 54                 2.93                    0.00                 0
    ## 55                 2.28                    0.00                 0
    ## 56                 4.35                    0.00                 0
    ## 57                 4.07                    0.00                 0
    ## 58                 7.54                    0.00                 0
    ## 59                 5.08                    0.00                 0
    ## 60                 2.60                    0.00                 0
    ## 61                 3.45                    0.00                 0
    ## 62                 6.60                    0.00                 0
    ## 63                 0.16                    0.00                 0
    ## 64                 5.32                    0.00                 0
    ## 65                 5.51                    0.00                 0
    ## 66                 3.42                    0.00                 0
    ## 67                 3.41                    0.00                19
    ## 68                 3.94                    0.00                 1
    ## 69                 5.43                    0.00                 1
    ## 70                 3.26                    0.00                 1
    ## 71                 3.79                    0.00                11
    ## 72                 4.44                    0.00                 0
    ## 73                 3.38                    0.00                23
    ## 74                 2.92                    0.00                 9
    ## 75                 7.15                    0.00                32
    ## 76                 6.26                    0.00                 0
    ## 77                 4.60                    0.00                15
    ## 78                 3.95                    0.00                 0
    ## 79                 5.32                    0.00                26
    ## 80                 3.60                    0.00                 0
    ## 81                 0.03                    0.00                 0
    ## 82                 2.01                    0.00                44
    ## 83                 2.55                    0.00                31
    ## 84                 3.82                    0.00                 5
    ## 85                 3.26                    0.00                15
    ## 86                 2.97                    0.00                31
    ## 87                 3.92                    0.00                11
    ## 88                 3.82                    0.00                 4
    ## 89                 3.96                    0.00                19
    ## 90                 3.28                    0.00                 2
    ## 91                 4.06                    0.00                 0
    ## 92                 3.06                    0.00                33
    ## 93                 3.85                    0.00                 0
    ## 94                 2.17                    0.00                30
    ## 95                 2.41                    0.00                50
    ## 96                 2.95                    0.00                 7
    ## 97                 4.62                    0.00                 0
    ## 98                 3.22                    0.00                15
    ## 99                 2.71                    0.00                36
    ## 100                3.41                    0.00                43
    ## 101                2.41                    0.00                41
    ## 102                3.02                    0.00                24
    ## 103                4.53                    0.00                47
    ## 104                2.89                    0.00                14
    ## 105                2.71                    0.00                14
    ## 106                2.67                    0.00                29
    ## 107                3.59                    0.00                 0
    ## 108                3.17                    0.00                 9
    ## 109                3.31                    0.00                 1
    ## 110                5.68                    0.00                 4
    ## 111                0.59                    0.00                65
    ## 112                3.15                    0.05                 0
    ## 113                5.58                    0.00                13
    ## 114                4.04                    0.07                38
    ## 115                3.10                    0.01                 0
    ## 116                4.15                    0.00                 0
    ## 117                3.87                    0.04                 0
    ## 118                6.44                    0.00                 1
    ## 119                3.80                    0.00                 0
    ## 120                3.18                    0.00                 0
    ## 121                1.12                    0.00                 1
    ## 122                5.99                    0.00                 0
    ## 123                5.31                    0.00                 0
    ## 124                0.35                    0.00                 0
    ## 125                2.15                    0.00                 1
    ## 126                3.31                    0.00                 0
    ## 127                2.47                    0.00                 1
    ## 128                5.93                    0.00                 0
    ## 129                5.15                    0.00                 8
    ## 130                4.91                    0.00                 6
    ## 131                5.94                    0.00                13
    ## 132                4.12                    0.00                 6
    ## 133                3.51                    0.00                 6
    ## 134                0.84                    0.00                 0
    ## 135                5.71                    0.00                 0
    ## 136                4.83                    0.00                 8
    ## 137                5.46                    0.00                 6
    ## 138                4.08                    0.00                 3
    ## 139                2.46                    0.00                 0
    ## 140                6.25                    0.00                 2
    ## 141                5.70                    0.00                 3
    ## 142                5.18                    0.00                 3
    ## 143                0.01                    0.00                 0
    ## 144                6.73                    0.00                 0
    ## 145                4.45                    0.00                 6
    ## 146                3.58                    0.00                 0
    ## 147                5.14                    0.00                27
    ## 148                5.77                    0.00                20
    ## 149                5.29                    0.00                19
    ## 150                6.98                    0.00                 7
    ## 151                4.92                    0.00                77
    ## 152                5.35                    0.00                58
    ## 153                6.30                    0.00                14
    ## 154                5.27                    0.00                14
    ## 155                5.39                    0.00                11
    ## 156                6.03                    0.00                19
    ## 157                6.94                    0.00                14
    ## 158                6.20                    0.00                12
    ## 159                4.55                    0.00                33
    ## 160                5.80                    0.00                35
    ## 161                5.09                    0.00                12
    ## 162                5.33                    0.00               120
    ## 163                3.23                    0.00               107
    ## 164                6.79                    0.00                 6
    ## 165                6.46                    0.00                13
    ## 166                6.53                    0.00                 8
    ## 167                2.20                    0.00                 0
    ## 168                1.99                    0.00                 0
    ## 169                2.67                    0.00                 0
    ## 170                4.83                    0.00                 0
    ## 171                2.65                    0.00                 0
    ## 172                1.52                    0.00                 0
    ## 173                1.39                    0.00                 0
    ## 174                1.39                    0.00                 0
    ## 175                2.54                    0.00                 0
    ## 176                4.58                    0.00                 0
    ## 177                2.27                    0.00                 0
    ## 178                1.96                    0.00                29
    ## 179                2.38                    0.00                32
    ## 180                3.01                    0.00                 0
    ## 181                3.13                    0.00                 0
    ## 182                4.18                    0.00                 0
    ## 183                3.51                    0.00                 0
    ## 184                2.35                    0.00                27
    ## 185                2.58                    0.00                30
    ## 186                1.96                    0.00                 0
    ## 187                2.18                    0.00                 2
    ## 188                3.03                    0.00                 0
    ## 189                1.25                    0.00                26
    ## 190                4.65                    0.00                 0
    ## 191                3.54                    0.00                 0
    ## 192                2.63                    0.00                 0
    ## 193                2.86                    0.00                34
    ## 194                0.52                    0.00                 0
    ## 195                7.70                    0.00                 6
    ## 196                3.59                    0.00                12
    ## 197                2.31                    0.00                15
    ## 198                2.27                    0.00                 0
    ## 199                2.02                    0.00                66
    ## 200                5.85                    0.00                 0
    ## 201                5.58                    0.00                 0
    ## 202                6.37                    0.00                 0
    ## 203                4.79                    0.00                 1
    ## 204                7.54                    0.00                 3
    ## 205                5.88                    0.00                12
    ## 206                4.07                    0.00                22
    ## 207                4.19                    0.00                10
    ## 208                5.73                    0.00                 2
    ## 209                2.94                    0.00                 4
    ## 210                8.27                    0.00                 9
    ## 211                5.88                    0.00                15
    ## 212                6.11                    0.00                 6
    ## 213                4.54                    0.00                 1
    ## 214                4.78                    0.00                 1
    ## 215                5.77                    0.00                14
    ## 216                6.07                    0.00                12
    ## 217                5.88                    0.00                 4
    ## 218                7.67                    0.00                 0
    ## 219                5.54                    0.00                 4
    ## 220                6.09                    0.00                 8
    ## 221                4.87                    0.00                 1
    ## 222                8.97                    0.00                 5
    ## 223                6.10                    0.00                 0
    ## 224                6.65                    0.00                 1
    ## 225                4.98                    0.00                10
    ## 226                2.23                    0.00                 0
    ## 227                5.41                    0.00                19
    ## 228                3.16                    0.00                 0
    ## 229                4.67                    0.00                61
    ## 230                4.71                    0.00                58
    ## 231                3.77                    0.00                 0
    ## 232                0.43                    0.00                 0
    ## 233                2.43                    0.00                 0
    ## 234                4.18                    0.00                69
    ## 235                1.77                    0.00                 0
    ## 236                2.96                    0.00                47
    ## 237                5.60                    0.00                25
    ## 238                2.68                    0.00                 0
    ## 239                1.18                    0.00                 0
    ## 240                3.69                    0.00                51
    ## 241                4.46                    0.00                40
    ## 242                4.74                    0.00                16
    ## 243                3.17                    0.00                49
    ## 244                4.91                    0.00                46
    ## 245                0.78                    0.00                 0
    ## 246                3.37                    0.00                 0
    ## 247                4.90                    0.00                23
    ## 248                5.68                    0.00                26
    ## 249                2.77                    0.00                 0
    ## 250                4.43                    0.00                44
    ## 251                4.23                    0.00                21
    ## 252                1.22                    0.00                 0
    ## 253                3.30                    0.00                 3
    ## 254                4.56                    0.00                59
    ## 255                5.20                    0.00                61
    ## 256                3.22                    0.00                 0
    ## 257                1.06                    0.00                 8
    ## 258                2.10                    0.00                86
    ## 259                3.25                    0.00                15
    ## 260                2.78                    0.00               118
    ## 261                3.56                    0.00               115
    ## 262                2.73                    0.00               184
    ## 263                2.32                    0.00               200
    ## 264                1.82                    0.00               114
    ## 265                1.76                    0.00               108
    ## 266                1.76                    0.00                87
    ## 267                2.78                    0.00               110
    ## 268                3.30                    0.00                62
    ## 269                4.14                    0.00                24
    ## 270                1.98                    0.00               210
    ## 271                2.76                    0.00                61
    ## 272                2.47                    0.00                38
    ## 273                2.93                    0.00                63
    ## 274                3.01                    0.00                99
    ## 275                2.47                    0.00                97
    ## 276                2.61                    0.00               207
    ## 277                3.24                    0.00               194
    ## 278                4.07                    0.00                37
    ## 279                3.57                    0.00                97
    ## 280                2.08                    0.00                25
    ## 281                2.67                    0.00                45
    ## 282                3.35                    0.00               104
    ## 283                0.89                    0.00                45
    ## 284                9.48                    0.00                 7
    ## 285                5.40                    0.00                 0
    ## 286                3.89                    0.00                 0
    ## 287                8.41                    0.00                 0
    ## 288                5.52                    0.00                26
    ## 289                5.62                    0.00                11
    ## 290                6.20                    0.00                 0
    ## 291                8.68                    0.00                 0
    ## 292                5.76                    0.00                 0
    ## 293                7.11                    0.00                 0
    ## 294                2.60                    0.00                 0
    ## 295                7.24                    0.00                 0
    ## 296                6.73                    0.00                 0
    ## 297                7.40                    0.00                 0
    ## 298                2.68                    0.00                 0
    ## 299                5.54                    0.00                 0
    ## 300                5.53                    0.00                 0
    ##     fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                    13                  328              728     1985
    ## 2                    19                  217              776     1797
    ## 3                    34                  209              726     1745
    ## 4                    10                  221              773     1863
    ## 5                    20                  164              539     1728
    ## 6                    31                  264              775     2035
    ## 7                    12                  205              818     1786
    ## 8                     8                  211              838     1775
    ## 9                    21                  262              732     1949
    ## 10                    5                  238              709     1788
    ## 11                   14                  216              814     2013
    ## 12                   23                  279              833     1970
    ## 13                   28                  189              782     1898
    ## 14                   12                  243              815     1837
    ## 15                   34                  217              712     1947
    ## 16                   35                  246              730     1820
    ## 17                   15                  277              798     2004
    ## 18                   24                  254              816     1990
    ## 19                   24                  250              857     1959
    ## 20                    6                  289              754     1896
    ## 21                   46                  175              833     1821
    ## 22                    8                  203              574     1740
    ## 23                   11                  206              835     1819
    ## 24                   31                  214              746     1859
    ## 25                   23                  251              669     1783
    ## 26                    0                  120             1193     2498
    ## 27                   71                  402              816     3846
    ## 28                    7                  148              682     2580
    ## 29                    0                  295              991     2987
    ## 30                    0                  176              527     1725
    ## 31                    0                  184              218     1763
    ## 32                    7                   75              585     1541
    ## 33                    0                   55              734     2220
    ## 34                    0                   32              986     2151
    ## 35                    0                   51              941     2221
    ## 36                    0                  192             1058     2638
    ## 37                    0                   95             1167     2351
    ## 38                    8                  181              706     1459
    ## 39                    0                  238              663     1521
    ## 40                    0                  197              653     1431
    ## 41                    0                  188              687     1444
    ## 42                    0                  150              728     1373
    ## 43                    0                   60             1053     1214
    ## 44                    0                  141              785     1356
    ## 45                    0                  327              623     1667
    ## 46                    0                  153              749     1370
    ## 47                    0                  162              712     1399
    ## 48                    0                  432              458     1916
    ## 49                    0                  164              704     1401
    ## 50                    0                  260              821     1576
    ## 51                    0                  286              586     1593
    ## 52                    0                  331              626     1649
    ## 53                    0                  352              492     1692
    ## 54                    0                  233              594     1506
    ## 55                    0                  191              716     1447
    ## 56                    0                  355              716     1690
    ## 57                    0                  345              530     1658
    ## 58                    0                  475              479     1926
    ## 59                    0                  383              511     1736
    ## 60                    0                  229              665     1491
    ## 61                    0                  258              610     1555
    ## 62                    0                  401              543     1869
    ## 63                    0                   17             1002     1141
    ## 64                    0                  330              569     1698
    ## 65                    0                  343              330     1364
    ## 66                    0                  242             1129     1804
    ## 67                   32                  195              676     2038
    ## 68                   48                  206              705     2010
    ## 69                   24                  284              720     2133
    ## 70                    7                  249              508     1882
    ## 71                   16                  206              678     1944
    ## 72                    7                  382              648     2346
    ## 73                   26                  208              761     2048
    ## 74                   27                  206              781     1946
    ## 75                   35                  360              591     2629
    ## 76                    0                  360              584     2187
    ## 77                   11                  277              653     2095
    ## 78                    0                  227              732     1861
    ## 79                    9                  295              623     2194
    ## 80                    0                  229              764     1854
    ## 81                    0                    4                2      403
    ## 82                   19                  131              777     1450
    ## 83                   46                  153              754     1495
    ## 84                   23                  214              801     1433
    ## 85                   42                  183              644     1468
    ## 86                   83                  153              663     1625
    ## 87                   58                  205              600     1529
    ## 88                   95                  214              605     1584
    ## 89                   67                  221              738     1638
    ## 90                   98                  164              845     1554
    ## 91                    0                  242              712     1397
    ## 92                   12                  188              731     1481
    ## 93                   92                  252              724     1638
    ## 94                   95                  129              660     1655
    ## 95                    9                  133              781     1570
    ## 96                   95                  170              797     1551
    ## 97                   10                  176              714     1377
    ## 98                    8                  190              804     1407
    ## 99                   32                  150              744     1545
    ## 100                  52                  194              687     1650
    ## 101                  40                  124              691     1501
    ## 102                 143                  176              713     1760
    ## 103                  41                  258              594     1710
    ## 104                  96                  142              852     1628
    ## 105                  88                  178              680     1618
    ## 106                  55                  168              676     1590
    ## 107                  86                  208              703     1574
    ## 108                 116                  171              688     1633
    ## 109                 115                  196              676     1630
    ## 110                  15                  331              712     3654
    ## 111                  21                   55             1222     3051
    ## 112                   0                  174              950     2828
    ## 113                  46                  346              531     3879
    ## 114                  42                  196              916     3429
    ## 115                   0                  177              855     2704
    ## 116                   0                  263              775     3089
    ## 117                   0                  206              774     2926
    ## 118                   9                  339              589     2302
    ## 119                   0                  228              752     1985
    ## 120                   0                  194              724     1884
    ## 121                   9                   58              824     1632
    ## 122                   0                  311              604     2200
    ## 123                  18                  306              671     2220
    ## 124                   0                   34             1265     1792
    ## 125                  19                  176              709     1886
    ## 126                   0                  233              546     1945
    ## 127                   5                  191              692     1880
    ## 128                   8                  390              544     2314
    ## 129                  21                  288              649     2236
    ## 130                  47                  300              680     2324
    ## 131                   8                  359              552     2367
    ## 132                  18                  289              624     2175
    ## 133                  38                  196              695     2092
    ## 134                   0                   67              836     1593
    ## 135                  11                  344              585     2270
    ## 136                  26                  287              669     2235
    ## 137                  11                  314              692     2266
    ## 138                  28                  279              586     2158
    ## 139                   0                  153              603     1792
    ## 140                  14                  374              490     2345
    ## 141                  12                  329              555     2260
    ## 142                  18                  311              574     2232
    ## 143                   0                    2                0      257
    ## 144                   0                  299              837     3066
    ## 145                   9                  253              609     3073
    ## 146                   0                  201              721     2572
    ## 147                   8                  239             1017     3274
    ## 148                  16                  249              704     3015
    ## 149                  42                  228              696     3083
    ## 150                  12                  272              853     3069
    ## 151                  25                  220              945     3544
    ## 152                   5                  215              749     3306
    ## 153                   8                  239              584     2885
    ## 154                  23                  224              673     2929
    ## 155                  48                  241              684     3074
    ## 156                   9                  234              878     2969
    ## 157                  43                  300              537     3283
    ## 158                  15                  241              579     2926
    ## 159                   4                  204              935     3147
    ## 160                  21                  251              632     3162
    ## 161                  39                  199              896     2899
    ## 162                  56                  260              508     4022
    ## 163                  38                  178              576     3934
    ## 164                  19                  258             1020     3013
    ## 165                  14                  267              648     3061
    ## 166                  18                  256              858     2954
    ## 167                   0                  196              787     2113
    ## 168                   0                  194              840     2095
    ## 169                   0                  231              717     2194
    ## 170                   0                  350              711     2496
    ## 171                   0                  225              716     2180
    ## 172                   0                  114             1219     1933
    ## 173                   0                  121              895     1954
    ## 174                   0                  137              841     1974
    ## 175                   0                  215              756     2150
    ## 176                   0                  317              706     2432
    ## 177                   0                  179              916     2070
    ## 178                   1                  180              839     2291
    ## 179                   1                  194              839     2361
    ## 180                   0                  236              762     2203
    ## 181                   0                  226             1106     2196
    ## 182                   0                  290              797     2363
    ## 183                   0                  240              741     2246
    ## 184                   4                  200              667     2336
    ## 185                   2                  233              725     2421
    ## 186                   0                  180              897     2070
    ## 187                   6                  185              734     2120
    ## 188                   0                  229              809     2211
    ## 189                   4                  108              866     2123
    ## 190                   8                  308              733     2423
    ## 191                   0                  266              641     2281
    ## 192                   0                  231              783     2181
    ## 193                  22                  232              622     2499
    ## 194                   0                   58              380     1212
    ## 195                  21                  432              844     2486
    ## 196                  41                  283             1062     2223
    ## 197                  29                  197             1096     1918
    ## 198                   0                  190             1121     1692
    ## 199                  35                  238             1019     2666
    ## 200                   0                  263              718     2947
    ## 201                   0                  258              777     2898
    ## 202                   0                  271              772     2984
    ## 203                   8                  256              944     2896
    ## 204                  24                  335              556     3328
    ## 205                  66                  302              437     3394
    ## 206                  30                  191              890     3013
    ## 207                   8                  179              757     2812
    ## 208                  29                  260              717     3061
    ## 209                  41                  144              901     2729
    ## 210                  66                  408              469     3691
    ## 211                  95                  281              542     3538
    ## 212                  15                  270              730     3064
    ## 213                   8                  216              765     2784
    ## 214                  16                  238              733     2908
    ## 215                   9                  232              738     3033
    ## 216                  19                  267              692     3165
    ## 217                  36                  263              728     3115
    ## 218                   0                  313              729     3145
    ## 219                  15                  251              757     3004
    ## 220                   5                  241              745     3006
    ## 221                  16                  207              682     2859
    ## 222                  46                  439              577     3683
    ## 223                  12                  253              746     2990
    ## 224                  37                  262              701     3172
    ## 225                  41                  235              784     3069
    ## 226                   0                   68              241     1240
    ## 227                  13                  277              767     2026
    ## 228                   0                  226              647     1718
    ## 229                  41                  256              693     2324
    ## 230                  38                  239              689     2254
    ## 231                   0                  288              521     1831
    ## 232                   0                   46              943     1397
    ## 233                   0                  206              622     1683
    ## 234                  28                  249              756     2284
    ## 235                   0                  148              598     1570
    ## 236                  42                  177              801     2066
    ## 237                  16                  270              781     2105
    ## 238                   0                  272              443     1776
    ## 239                   0                  104              582     1507
    ## 240                  11                  201              732     2033
    ## 241                  18                  238              750     2093
    ## 242                  16                  206              745     1922
    ## 243                  13                  165              727     1999
    ## 244                  15                  270              709     2169
    ## 245                   0                   84              506     1463
    ## 246                   0                  237              436     1747
    ## 247                   9                  227              724     1996
    ## 248                  29                  247              812     2116
    ## 249                   0                  224              651     1698
    ## 250                  29                  241              692     2156
    ## 251                   9                  229              761     1916
    ## 252                   0                   96              902     1494
    ## 253                   8                  210              505     1762
    ## 254                  22                  251              667     2272
    ## 255                  40                  265              707     2335
    ## 256                   0                  195              628     1693
    ## 257                   6                   48              222      741
    ## 258                  16                  140              728     3405
    ## 259                  11                  144              776     2551
    ## 260                  30                  176              662     4022
    ## 261                  54                  199              695     4005
    ## 262                  56                  158              472     4274
    ## 263                  37                  159              525     4552
    ## 264                  32                  130              623     3625
    ## 265                  23                  111              733     3501
    ## 266                  16                  113              773     3192
    ## 267                  74                  175              670     4018
    ## 268                  30                  200              823     3329
    ## 269                  24                  223              627     3152
    ## 270                  65                  141              425     4392
    ## 271                  38                  214              743     3374
    ## 272                  32                  181              759     3088
    ## 273                  16                  190              773     3294
    ## 274                  51                  141              692     3580
    ## 275                  36                  165              739     3544
    ## 276                  45                  163              621     4501
    ## 277                  72                  178              499     4546
    ## 278                  20                  235              732     3014
    ## 279                   8                  212              580     3795
    ## 280                   9                  141              631     2755
    ## 281                  21                  143             1153     3004
    ## 282                  37                  194              639     3841
    ## 283                  15                   63              257     1665
    ## 284                  15                  518              502     2828
    ## 285                   0                  312              702     2225
    ## 286                   0                  241              759     2018
    ## 287                   0                  480              425     2606
    ## 288                  10                  349              587     2536
    ## 289                  19                  294              579     4900
    ## 290                   0                  402              413     2409
    ## 291                   0                  512              468     2651
    ## 292                   0                  362              711     2305
    ## 293                   0                  458              417     2576
    ## 294                   0                  141              758     1879
    ## 295                   0                  461              479     2560
    ## 296                   0                  397              525     2361
    ## 297                   0                  487              479     2636
    ## 298                   0                  133              673     1838
    ## 299                   0                  412              456     2469
    ## 300                   0                  318              517     2250
    ##     totalsleeprecords totalminutesasleep totaltimeinbed days_used      usage
    ## 1                   1                327            346        25   high use
    ## 2                   2                384            407        25   high use
    ## 3                   1                412            442        25   high use
    ## 4                   2                340            367        25   high use
    ## 5                   1                700            712        25   high use
    ## 6                   1                304            320        25   high use
    ## 7                   1                360            377        25   high use
    ## 8                   1                325            364        25   high use
    ## 9                   1                361            384        25   high use
    ## 10                  1                430            449        25   high use
    ## 11                  1                277            323        25   high use
    ## 12                  1                245            274        25   high use
    ## 13                  1                366            393        25   high use
    ## 14                  1                341            354        25   high use
    ## 15                  1                404            425        25   high use
    ## 16                  1                369            396        25   high use
    ## 17                  1                277            309        25   high use
    ## 18                  1                273            296        25   high use
    ## 19                  1                247            264        25   high use
    ## 20                  1                334            367        25   high use
    ## 21                  1                331            349        25   high use
    ## 22                  1                594            611        25   high use
    ## 23                  1                338            342        25   high use
    ## 24                  1                383            403        25   high use
    ## 25                  1                285            306        25   high use
    ## 26                  1                119            127         4    low use
    ## 27                  1                124            142         4    low use
    ## 28                  1                796            961         4    low use
    ## 29                  1                137            154         4    low use
    ## 30                  1                644            961         3    low use
    ## 31                  1                722            961         3    low use
    ## 32                  1                590            961         3    low use
    ## 33                  3                750            775         5    low use
    ## 34                  1                398            422         5    low use
    ## 35                  2                475            499         5    low use
    ## 36                  1                296            315         5    low use
    ## 37                  1                166            178         5    low use
    ## 38                  1                503            546        28   high use
    ## 39                  1                531            565        28   high use
    ## 40                  1                545            568        28   high use
    ## 41                  1                523            573        28   high use
    ## 42                  1                524            567        28   high use
    ## 43                  1                437            498        28   high use
    ## 44                  1                498            540        28   high use
    ## 45                  1                461            510        28   high use
    ## 46                  1                477            514        28   high use
    ## 47                  1                520            545        28   high use
    ## 48                  1                522            554        28   high use
    ## 49                  1                555            591        28   high use
    ## 50                  1                506            531        28   high use
    ## 51                  1                508            545        28   high use
    ## 52                  1                513            545        28   high use
    ## 53                  1                490            510        28   high use
    ## 54                  1                573            607        28   high use
    ## 55                  1                527            546        28   high use
    ## 56                  1                511            543        28   high use
    ## 57                  1                538            560        28   high use
    ## 58                  1                468            485        28   high use
    ## 59                  1                524            548        28   high use
    ## 60                  1                511            521        28   high use
    ## 61                  1                541            568        28   high use
    ## 62                  1                531            556        28   high use
    ## 63                  1                357            380        28   high use
    ## 64                  1                523            553        28   high use
    ## 65                  1                456            485        28   high use
    ## 66                  1                 61             69         1    low use
    ## 67                  1                467            531        15 medium use
    ## 68                  1                445            489        15 medium use
    ## 69                  1                452            504        15 medium use
    ## 70                  1                556            602        15 medium use
    ## 71                  1                500            557        15 medium use
    ## 72                  1                465            514        15 medium use
    ## 73                  1                460            484        15 medium use
    ## 74                  1                405            461        15 medium use
    ## 75                  1                374            386        15 medium use
    ## 76                  1                442            459        15 medium use
    ## 77                  1                433            471        15 medium use
    ## 78                  1                436            490        15 medium use
    ## 79                  1                448            499        15 medium use
    ## 80                  1                408            450        15 medium use
    ## 81                  1                411            473        15 medium use
    ## 82                  1                274            469        28   high use
    ## 83                  2                295            456        28   high use
    ## 84                  1                291            397        28   high use
    ## 85                  1                424            556        28   high use
    ## 86                  1                283            510        28   high use
    ## 87                  1                381            566        28   high use
    ## 88                  2                412            522        28   high use
    ## 89                  1                219            395        28   high use
    ## 90                  2                152            305        28   high use
    ## 91                  1                332            512        28   high use
    ## 92                  1                355            476        28   high use
    ## 93                  1                235            372        28   high use
    ## 94                  1                310            526        28   high use
    ## 95                  1                262            467        28   high use
    ## 96                  1                250            371        28   high use
    ## 97                  1                349            540        28   high use
    ## 98                  1                261            423        28   high use
    ## 99                  1                333            478        28   high use
    ## 100                 1                237            382        28   high use
    ## 101                 1                383            626        28   high use
    ## 102                 1                230            384        28   high use
    ## 103                 1                292            500        28   high use
    ## 104                 1                213            336        28   high use
    ## 105                 1                318            480        28   high use
    ## 106                 1                323            512        28   high use
    ## 107                 1                237            443        28   high use
    ## 108                 2                259            456        28   high use
    ## 109                 1                312            452        28   high use
    ## 110                 1                501            541         8    low use
    ## 111                 1                 77             77         8    low use
    ## 112                 1                322            332         8    low use
    ## 113                 1                478            536         8    low use
    ## 114                 1                226            248         8    low use
    ## 115                 1                385            408         8    low use
    ## 116                 1                364            402         8    low use
    ## 117                 1                442            494         8    low use
    ## 118                 1                535            557        26   high use
    ## 119                 1                465            491        26   high use
    ## 120                 1                506            522        26   high use
    ## 121                 1                515            551        26   high use
    ## 122                 2                461            498        26   high use
    ## 123                 1                523            543        26   high use
    ## 124                 1                 59             65        26   high use
    ## 125                 1                533            550        26   high use
    ## 126                 1                692            722        26   high use
    ## 127                 1                467            501        26   high use
    ## 128                 1                488            506        26   high use
    ## 129                 1                505            516        26   high use
    ## 130                 1                286            307        26   high use
    ## 131                 1                497            522        26   high use
    ## 132                 1                523            546        26   high use
    ## 133                 1                490            516        26   high use
    ## 134                 1                484            500        26   high use
    ## 135                 1                478            506        26   high use
    ## 136                 1                474            512        26   high use
    ## 137                 1                450            491        26   high use
    ## 138                 1                507            530        26   high use
    ## 139                 1                602            638        26   high use
    ## 140                 1                535            565        26   high use
    ## 141                 1                487            517        26   high use
    ## 142                 1                529            558        26   high use
    ## 143                 1                302            321        26   high use
    ## 144                 1                499            526        23   high use
    ## 145                 2                426            448        23   high use
    ## 146                 2                619            641        23   high use
    ## 147                 1                 99            104        23   high use
    ## 148                 1                329            338        23   high use
    ## 149                 1                421            451        23   high use
    ## 150                 1                442            458        23   high use
    ## 151                 1                 82             85        23   high use
    ## 152                 1                478            501        23   high use
    ## 153                 3                552            595        23   high use
    ## 154                 1                319            346        23   high use
    ## 155                 1                439            500        23   high use
    ## 156                 1                428            458        23   high use
    ## 157                 2                409            430        23   high use
    ## 158                 1                547            597        23   high use
    ## 159                 2                368            376        23   high use
    ## 160                 1                390            414        23   high use
    ## 161                 1                471            495        23   high use
    ## 162                 1                472            496        23   high use
    ## 163                 2                529            541        23   high use
    ## 164                 1                 62             65        23   high use
    ## 165                 1                354            375        23   high use
    ## 166                 1                469            494        23   high use
    ## 167                 2                429            457        28   high use
    ## 168                 2                370            406        28   high use
    ## 169                 1                441            492        28   high use
    ## 170                 2                337            379        28   high use
    ## 171                 1                462            499        28   high use
    ## 172                 1                 98            107        28   high use
    ## 173                 2                388            424        28   high use
    ## 174                 1                439            462        28   high use
    ## 175                 1                436            469        28   high use
    ## 176                 1                388            417        28   high use
    ## 177                 1                328            345        28   high use
    ## 178                 2                353            391        28   high use
    ## 179                 1                332            374        28   high use
    ## 180                 1                419            442        28   high use
    ## 181                 1                106            108        28   high use
    ## 182                 1                322            353        28   high use
    ## 183                 2                439            459        28   high use
    ## 184                 1                502            542        28   high use
    ## 185                 2                417            450        28   high use
    ## 186                 2                337            363        28   high use
    ## 187                 2                462            513        28   high use
    ## 188                 2                374            402        28   high use
    ## 189                 2                401            436        28   high use
    ## 190                 1                361            391        28   high use
    ## 191                 1                457            533        28   high use
    ## 192                 1                405            426        28   high use
    ## 193                 1                499            530        28   high use
    ## 194                 1                483            501        28   high use
    ## 195                 1                126            137         5    low use
    ## 196                 1                103            121         5    low use
    ## 197                 1                171            179         5    low use
    ## 198                 1                115            129         5    low use
    ## 199                 1                123            134         5    low use
    ## 200                 1                425            439        27   high use
    ## 201                 2                400            430        27   high use
    ## 202                 1                384            415        27   high use
    ## 203                 1                253            257        27   high use
    ## 204                 2                382            406        27   high use
    ## 205                 1                591            612        27   high use
    ## 206                 1                293            312        27   high use
    ## 207                 1                457            487        27   high use
    ## 208                 1                454            468        27   high use
    ## 209                 1                425            434        27   high use
    ## 210                 1                465            475        27   high use
    ## 211                 1                480            506        27   high use
    ## 212                 1                370            380        27   high use
    ## 213                 1                421            429        27   high use
    ## 214                 1                432            449        27   high use
    ## 215                 1                442            461        27   high use
    ## 216                 1                433            447        27   high use
    ## 217                 1                479            501        27   high use
    ## 218                 1                327            373        27   high use
    ## 219                 1                412            434        27   high use
    ## 220                 1                414            428        27   high use
    ## 221                 1                404            449        27   high use
    ## 222                 1                520            543        27   high use
    ## 223                 1                435            458        27   high use
    ## 224                 1                416            431        27   high use
    ## 225                 1                354            366        27   high use
    ## 226                 1                404            442        27   high use
    ## 227                 1                441            464        31   high use
    ## 228                 2                455            488        31   high use
    ## 229                 1                357            418        31   high use
    ## 230                 1                377            409        31   high use
    ## 231                 2                651            686        31   high use
    ## 232                 1                350            402        31   high use
    ## 233                 2                520            541        31   high use
    ## 234                 1                357            410        31   high use
    ## 235                 1                658            678        31   high use
    ## 236                 1                399            431        31   high use
    ## 237                 1                322            353        31   high use
    ## 238                 2                631            725        31   high use
    ## 239                 2                553            640        31   high use
    ## 240                 1                433            468        31   high use
    ## 241                 1                412            453        31   high use
    ## 242                 1                347            391        31   high use
    ## 243                 1                421            457        31   high use
    ## 244                 1                450            495        31   high use
    ## 245                 2                775            843        31   high use
    ## 246                 2                622            686        31   high use
    ## 247                 1                409            471        31   high use
    ## 248                 1                380            429        31   high use
    ## 249                 1                447            470        31   high use
    ## 250                 1                419            464        31   high use
    ## 251                 1                400            434        31   high use
    ## 252                 1                442            470        31   high use
    ## 253                 1                568            608        31   high use
    ## 254                 1                453            494        31   high use
    ## 255                 1                418            443        31   high use
    ## 256                 1                463            486        31   high use
    ## 257                 1                438            475        31   high use
    ## 258                 1                419            438        26   high use
    ## 259                 1                432            458        26   high use
    ## 260                 1                477            497        26   high use
    ## 261                 1                392            413        26   high use
    ## 262                 1                406            445        26   high use
    ## 263                 1                549            583        26   high use
    ## 264                 1                527            553        26   high use
    ## 265                 1                449            465        26   high use
    ## 266                 1                447            480        26   high use
    ## 267                 1                414            437        26   high use
    ## 268                 1                338            366        26   high use
    ## 269                 1                384            402        26   high use
    ## 270                 1                543            615        26   high use
    ## 271                 1                421            461        26   high use
    ## 272                 1                354            377        26   high use
    ## 273                 1                424            452        26   high use
    ## 274                 1                361            372        26   high use
    ## 275                 1                459            485        26   high use
    ## 276                 1                412            433        26   high use
    ## 277                 1                379            398        26   high use
    ## 278                 2                525            553        26   high use
    ## 279                 1                508            543        26   high use
    ## 280                 1                603            634        26   high use
    ## 281                 1                 74             78        26   high use
    ## 282                 1                504            562        26   high use
    ## 283                 1                431            476        26   high use
    ## 284                 1                380            398        18 medium use
    ## 285                 2                336            350        18 medium use
    ## 286                 2                493            510        18 medium use
    ## 287                 1                465            492        18 medium use
    ## 288                 1                474            502        18 medium use
    ## 289                 1                508            550        18 medium use
    ## 290                 1                480            546        18 medium use
    ## 291                 1                492            539        18 medium use
    ## 292                 1                353            367        18 medium use
    ## 293                 1                542            557        18 medium use
    ## 294                 1                393            416        18 medium use
    ## 295                 1                600            636        18 medium use
    ## 296                 1                507            575        18 medium use
    ## 297                 1                392            415        18 medium use
    ## 298                 2                658            698        18 medium use
    ## 299                 2                498            507        18 medium use
    ## 300                 1                555            603        18 medium use
    ##     total_worn minute_worn_percentage               worn
    ## 1         1094             75.9722222 More than half day
    ## 2         1033             71.7361111 More than half day
    ## 3          998             69.3055556 More than half day
    ## 4         1040             72.2222222 More than half day
    ## 5          761             52.8472222 More than half day
    ## 6         1120             77.7777778 More than half day
    ## 7         1063             73.8194444 More than half day
    ## 8         1076             74.7222222 More than half day
    ## 9         1056             73.3333333 More than half day
    ## 10         991             68.8194444 More than half day
    ## 11        1117             77.5694444 More than half day
    ## 12        1166             80.9722222 More than half day
    ## 13        1047             72.7083333 More than half day
    ## 14        1086             75.4166667 More than half day
    ## 15        1015             70.4861111 More than half day
    ## 16        1044             72.5000000 More than half day
    ## 17        1131             78.5416667 More than half day
    ## 18        1144             79.4444444 More than half day
    ## 19        1176             81.6666667 More than half day
    ## 20        1073             74.5138889 More than half day
    ## 21        1091             75.7638889 More than half day
    ## 22         829             57.5694444 More than half day
    ## 23        1098             76.2500000 More than half day
    ## 24        1037             72.0138889 More than half day
    ## 25         979             67.9861111 More than half day
    ## 26        1313             91.1805556 More than half day
    ## 27        1298             90.1388889 More than half day
    ## 28         838             58.1944444 More than half day
    ## 29        1286             89.3055556 More than half day
    ## 30         703             48.8194444 Less than half day
    ## 31         402             27.9166667 Less than half day
    ## 32         667             46.3194444 Less than half day
    ## 33         789             54.7916667 More than half day
    ## 34        1018             70.6944444 More than half day
    ## 35         992             68.8888889 More than half day
    ## 36        1250             86.8055556 More than half day
    ## 37        1262             87.6388889 More than half day
    ## 38         898             62.3611111 More than half day
    ## 39         901             62.5694444 More than half day
    ## 40         850             59.0277778 More than half day
    ## 41         875             60.7638889 More than half day
    ## 42         878             60.9722222 More than half day
    ## 43        1113             77.2916667 More than half day
    ## 44         926             64.3055556 More than half day
    ## 45         950             65.9722222 More than half day
    ## 46         902             62.6388889 More than half day
    ## 47         874             60.6944444 More than half day
    ## 48         890             61.8055556 More than half day
    ## 49         868             60.2777778 More than half day
    ## 50        1081             75.0694444 More than half day
    ## 51         872             60.5555556 More than half day
    ## 52         957             66.4583333 More than half day
    ## 53         844             58.6111111 More than half day
    ## 54         827             57.4305556 More than half day
    ## 55         907             62.9861111 More than half day
    ## 56        1071             74.3750000 More than half day
    ## 57         875             60.7638889 More than half day
    ## 58         954             66.2500000 More than half day
    ## 59         894             62.0833333 More than half day
    ## 60         894             62.0833333 More than half day
    ## 61         868             60.2777778 More than half day
    ## 62         944             65.5555556 More than half day
    ## 63        1019             70.7638889 More than half day
    ## 64         899             62.4305556 More than half day
    ## 65         673             46.7361111 Less than half day
    ## 66        1371             95.2083333 More than half day
    ## 67         922             64.0277778 More than half day
    ## 68         960             66.6666667 More than half day
    ## 69        1029             71.4583333 More than half day
    ## 70         765             53.1250000 More than half day
    ## 71         911             63.2638889 More than half day
    ## 72        1037             72.0138889 More than half day
    ## 73        1018             70.6944444 More than half day
    ## 74        1023             71.0416667 More than half day
    ## 75        1018             70.6944444 More than half day
    ## 76         944             65.5555556 More than half day
    ## 77         956             66.3888889 More than half day
    ## 78         959             66.5972222 More than half day
    ## 79         953             66.1805556 More than half day
    ## 80         993             68.9583333 More than half day
    ## 81           6              0.4166667 Less than half day
    ## 82         971             67.4305556 More than half day
    ## 83         984             68.3333333 More than half day
    ## 84        1043             72.4305556 More than half day
    ## 85         884             61.3888889 More than half day
    ## 86         930             64.5833333 More than half day
    ## 87         874             60.6944444 More than half day
    ## 88         918             63.7500000 More than half day
    ## 89        1045             72.5694444 More than half day
    ## 90        1109             77.0138889 More than half day
    ## 91         954             66.2500000 More than half day
    ## 92         964             66.9444444 More than half day
    ## 93        1068             74.1666667 More than half day
    ## 94         914             63.4722222 More than half day
    ## 95         973             67.5694444 More than half day
    ## 96        1069             74.2361111 More than half day
    ## 97         900             62.5000000 More than half day
    ## 98        1017             70.6250000 More than half day
    ## 99         962             66.8055556 More than half day
    ## 100        976             67.7777778 More than half day
    ## 101        896             62.2222222 More than half day
    ## 102       1056             73.3333333 More than half day
    ## 103        940             65.2777778 More than half day
    ## 104       1104             76.6666667 More than half day
    ## 105        960             66.6666667 More than half day
    ## 106        928             64.4444444 More than half day
    ## 107        997             69.2361111 More than half day
    ## 108        984             68.3333333 More than half day
    ## 109        988             68.6111111 More than half day
    ## 110       1062             73.7500000 More than half day
    ## 111       1363             94.6527778 More than half day
    ## 112       1124             78.0555556 More than half day
    ## 113        936             65.0000000 More than half day
    ## 114       1192             82.7777778 More than half day
    ## 115       1032             71.6666667 More than half day
    ## 116       1038             72.0833333 More than half day
    ## 117        980             68.0555556 More than half day
    ## 118        938             65.1388889 More than half day
    ## 119        980             68.0555556 More than half day
    ## 120        918             63.7500000 More than half day
    ## 121        892             61.9444444 More than half day
    ## 122        915             63.5416667 More than half day
    ## 123        995             69.0972222 More than half day
    ## 124       1299             90.2083333 More than half day
    ## 125        905             62.8472222 More than half day
    ## 126        779             54.0972222 More than half day
    ## 127        889             61.7361111 More than half day
    ## 128        942             65.4166667 More than half day
    ## 129        966             67.0833333 More than half day
    ## 130       1033             71.7361111 More than half day
    ## 131        932             64.7222222 More than half day
    ## 132        937             65.0694444 More than half day
    ## 133        935             64.9305556 More than half day
    ## 134        903             62.7083333 More than half day
    ## 135        940             65.2777778 More than half day
    ## 136        990             68.7500000 More than half day
    ## 137       1023             71.0416667 More than half day
    ## 138        896             62.2222222 More than half day
    ## 139        756             52.5000000 More than half day
    ## 140        880             61.1111111 More than half day
    ## 141        899             62.4305556 More than half day
    ## 142        906             62.9166667 More than half day
    ## 143          2              0.1388889 Less than half day
    ## 144       1136             78.8888889 More than half day
    ## 145        877             60.9027778 More than half day
    ## 146        922             64.0277778 More than half day
    ## 147       1291             89.6527778 More than half day
    ## 148        989             68.6805556 More than half day
    ## 149        985             68.4027778 More than half day
    ## 150       1144             79.4444444 More than half day
    ## 151       1267             87.9861111 More than half day
    ## 152       1027             71.3194444 More than half day
    ## 153        845             58.6805556 More than half day
    ## 154        934             64.8611111 More than half day
    ## 155        984             68.3333333 More than half day
    ## 156       1140             79.1666667 More than half day
    ## 157        894             62.0833333 More than half day
    ## 158        847             58.8194444 More than half day
    ## 159       1176             81.6666667 More than half day
    ## 160        939             65.2083333 More than half day
    ## 161       1146             79.5833333 More than half day
    ## 162        944             65.5555556 More than half day
    ## 163        899             62.4305556 More than half day
    ## 164       1303             90.4861111 More than half day
    ## 165        942             65.4166667 More than half day
    ## 166       1140             79.1666667 More than half day
    ## 167        983             68.2638889 More than half day
    ## 168       1034             71.8055556 More than half day
    ## 169        948             65.8333333 More than half day
    ## 170       1061             73.6805556 More than half day
    ## 171        941             65.3472222 More than half day
    ## 172       1333             92.5694444 More than half day
    ## 173       1016             70.5555556 More than half day
    ## 174        978             67.9166667 More than half day
    ## 175        971             67.4305556 More than half day
    ## 176       1023             71.0416667 More than half day
    ## 177       1095             76.0416667 More than half day
    ## 178       1049             72.8472222 More than half day
    ## 179       1066             74.0277778 More than half day
    ## 180        998             69.3055556 More than half day
    ## 181       1332             92.5000000 More than half day
    ## 182       1087             75.4861111 More than half day
    ## 183        981             68.1250000 More than half day
    ## 184        898             62.3611111 More than half day
    ## 185        990             68.7500000 More than half day
    ## 186       1077             74.7916667 More than half day
    ## 187        927             64.3750000 More than half day
    ## 188       1038             72.0833333 More than half day
    ## 189       1004             69.7222222 More than half day
    ## 190       1049             72.8472222 More than half day
    ## 191        907             62.9861111 More than half day
    ## 192       1014             70.4166667 More than half day
    ## 193        910             63.1944444 More than half day
    ## 194        438             30.4166667 Less than half day
    ## 195       1303             90.4861111 More than half day
    ## 196       1398             97.0833333 More than half day
    ## 197       1337             92.8472222 More than half day
    ## 198       1311             91.0416667 More than half day
    ## 199       1358             94.3055556 More than half day
    ## 200        981             68.1250000 More than half day
    ## 201       1035             71.8750000 More than half day
    ## 202       1043             72.4305556 More than half day
    ## 203       1209             83.9583333 More than half day
    ## 204        918             63.7500000 More than half day
    ## 205        817             56.7361111 More than half day
    ## 206       1133             78.6805556 More than half day
    ## 207        954             66.2500000 More than half day
    ## 208       1008             70.0000000 More than half day
    ## 209       1090             75.6944444 More than half day
    ## 210        952             66.1111111 More than half day
    ## 211        933             64.7916667 More than half day
    ## 212       1021             70.9027778 More than half day
    ## 213        990             68.7500000 More than half day
    ## 214        988             68.6111111 More than half day
    ## 215        993             68.9583333 More than half day
    ## 216        990             68.7500000 More than half day
    ## 217       1031             71.5972222 More than half day
    ## 218       1042             72.3611111 More than half day
    ## 219       1027             71.3194444 More than half day
    ## 220        999             69.3750000 More than half day
    ## 221        906             62.9166667 More than half day
    ## 222       1067             74.0972222 More than half day
    ## 223       1011             70.2083333 More than half day
    ## 224       1001             69.5138889 More than half day
    ## 225       1070             74.3055556 More than half day
    ## 226        309             21.4583333 Less than half day
    ## 227       1076             74.7222222 More than half day
    ## 228        873             60.6250000 More than half day
    ## 229       1051             72.9861111 More than half day
    ## 230       1024             71.1111111 More than half day
    ## 231        809             56.1805556 More than half day
    ## 232        989             68.6805556 More than half day
    ## 233        828             57.5000000 More than half day
    ## 234       1102             76.5277778 More than half day
    ## 235        746             51.8055556 More than half day
    ## 236       1067             74.0972222 More than half day
    ## 237       1092             75.8333333 More than half day
    ## 238        715             49.6527778 Less than half day
    ## 239        686             47.6388889 Less than half day
    ## 240        995             69.0972222 More than half day
    ## 241       1046             72.6388889 More than half day
    ## 242        983             68.2638889 More than half day
    ## 243        954             66.2500000 More than half day
    ## 244       1040             72.2222222 More than half day
    ## 245        590             40.9722222 Less than half day
    ## 246        673             46.7361111 Less than half day
    ## 247        983             68.2638889 More than half day
    ## 248       1114             77.3611111 More than half day
    ## 249        875             60.7638889 More than half day
    ## 250       1006             69.8611111 More than half day
    ## 251       1020             70.8333333 More than half day
    ## 252        998             69.3055556 More than half day
    ## 253        726             50.4166667 More than half day
    ## 254        999             69.3750000 More than half day
    ## 255       1073             74.5138889 More than half day
    ## 256        823             57.1527778 More than half day
    ## 257        284             19.7222222 Less than half day
    ## 258        970             67.3611111 More than half day
    ## 259        946             65.6944444 More than half day
    ## 260        986             68.4722222 More than half day
    ## 261       1063             73.8194444 More than half day
    ## 262        870             60.4166667 More than half day
    ## 263        921             63.9583333 More than half day
    ## 264        899             62.4305556 More than half day
    ## 265        975             67.7083333 More than half day
    ## 266        989             68.6805556 More than half day
    ## 267       1029             71.4583333 More than half day
    ## 268       1115             77.4305556 More than half day
    ## 269        898             62.3611111 More than half day
    ## 270        841             58.4027778 More than half day
    ## 271       1056             73.3333333 More than half day
    ## 272       1010             70.1388889 More than half day
    ## 273       1042             72.3611111 More than half day
    ## 274        983             68.2638889 More than half day
    ## 275       1037             72.0138889 More than half day
    ## 276       1036             71.9444444 More than half day
    ## 277        943             65.4861111 More than half day
    ## 278       1024             71.1111111 More than half day
    ## 279        897             62.2916667 More than half day
    ## 280        806             55.9722222 More than half day
    ## 281       1362             94.5833333 More than half day
    ## 282        974             67.6388889 More than half day
    ## 283        380             26.3888889 Less than half day
    ## 284       1042             72.3611111 More than half day
    ## 285       1014             70.4166667 More than half day
    ## 286       1000             69.4444444 More than half day
    ## 287        905             62.8472222 More than half day
    ## 288        972             67.5000000 More than half day
    ## 289        903             62.7083333 More than half day
    ## 290        815             56.5972222 More than half day
    ## 291        980             68.0555556 More than half day
    ## 292       1073             74.5138889 More than half day
    ## 293        875             60.7638889 More than half day
    ## 294        899             62.4305556 More than half day
    ## 295        940             65.2777778 More than half day
    ## 296        922             64.0277778 More than half day
    ## 297        966             67.0833333 More than half day
    ## 298        806             55.9722222 More than half day
    ## 299        868             60.2777778 More than half day
    ## 300        835             57.9861111 More than half day

Now we will map and see our 3 different types of health tracker device
user: high use, medium use, and low use map with the minute-worn data in
order to find out the usage pattern among these users.

-   The first chart is the overall information of all group of users.
-   The other 3 charts are filtered information of 3 user groups.

``` r
worn_minute_percentage <- minute_worn %>% 
  group_by(worn) %>% 
  summarize(count=sum(n())) %>% 
  mutate(total=sum(count)) %>% 
  group_by(worn) %>% 
  summarize(percent_total=count/total) %>% 
  mutate(labels = percent(percent_total))

worn_minute_highuse <- minute_worn %>% 
  filter(usage == "high use") %>% 
  group_by(worn) %>% 
  summarize(count=sum(n())) %>% 
  mutate(total=sum(count)) %>% 
  group_by(worn) %>% 
  summarize(percent_total=count/total) %>% 
  mutate(labels = percent(percent_total))

worn_minute_mediumuse <- minute_worn %>% 
  filter(usage == "medium use") %>% 
  group_by(worn) %>% 
  summarize(count=sum(n())) %>% 
  mutate(total=sum(count)) %>% 
  group_by(worn) %>% 
  summarize(percent_total=count/total) %>% 
  mutate(labels = percent(percent_total))
worn_minute_lowuse <- minute_worn %>% 
  filter(usage == "low use") %>% 
  group_by(worn) %>% 
  summarize(count=sum(n())) %>% 
  mutate(total=sum(count)) %>% 
  group_by(worn) %>% 
  summarize(percent_total=count/total) %>% 
  mutate(labels = percent(percent_total))

worn_minute_percentage$worn <- factor(worn_minute_percentage$worn, levels = c("All day", "More than half day", "Less than half day"))
worn_minute_highuse$worn <- factor(worn_minute_highuse$worn, levels = c("All day", "More than half day", "Less than half day"))
worn_minute_mediumuse$worn <- factor(worn_minute_mediumuse$worn, levels = c("All day", "More than half day", "Less than half day"))
worn_minute_lowuse$worn <- factor(worn_minute_lowuse$worn, levels = c("All day", "More than half day", "Less than half day"))

head(worn_minute_percentage)
```

    ## # A tibble: 2 × 3
    ##   worn               percent_total labels
    ##   <fct>                      <dbl> <chr> 
    ## 1 Less than half day        0.0488 5%    
    ## 2 More than half day        0.951  95%

``` r
head(worn_minute_highuse)
```

    ## # A tibble: 2 × 3
    ##   worn               percent_total labels
    ##   <fct>                      <dbl> <chr> 
    ## 1 Less than half day        0.0457 5%    
    ## 2 More than half day        0.954  95%

``` r
head(worn_minute_mediumuse)
```

    ## # A tibble: 2 × 3
    ##   worn               percent_total labels
    ##   <fct>                      <dbl> <chr> 
    ## 1 Less than half day        0.0417 4%    
    ## 2 More than half day        0.958  96%

``` r
head(worn_minute_lowuse)
```

    ## # A tibble: 2 × 3
    ##   worn               percent_total labels
    ##   <fct>                      <dbl> <chr> 
    ## 1 Less than half day        0.0882 9%    
    ## 2 More than half day        0.912  91%

And let’s visualize these data

``` r
ggarrange(
  ggplot(worn_minute_percentage,mapping=aes(x="",y=percent_total,fill=worn)) +
    geom_bar(stat="identity",width = 1) +
    coord_polar("y",start = 0) +
    theme_minimal() + 
    labs(title="Time worn per day", subtitle = "All users") +
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
    theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) + 
    geom_text(aes(label = labels),
        position = position_stack(vjust = 0.5), color="white", size = 3.5),
  
  ggarrange(
      ggplot(worn_minute_highuse,mapping=aes(x="",y=percent_total,fill=worn)) +
      geom_bar(stat="identity",width = 1) +
      coord_polar("y",start = 0) +
      theme_minimal() + 
      labs(title="", subtitle = "High use") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
      theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title=element_text(size=8),
          legend.key.height=unit(0.5,'cm'),
          legend.key.width=unit(0.3,'cm'),
          legend.text=element_text(size=8)) + 
      geom_text(aes(label = labels),
          position = position_stack(vjust = 0.5), color="white", size = 3),
    
    ggplot(worn_minute_mediumuse,mapping=aes(x="",y=percent_total,fill=worn)) +
      geom_bar(stat="identity",width = 1) +
      coord_polar("y",start = 0) +
      theme_minimal() + 
      labs(title="", subtitle = "Medium use") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
      theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title=element_text(size=8),
          legend.key.height=unit(0.5,'cm'),
          legend.key.width=unit(0.3,'cm'),
          legend.text=element_text(size=8)) + 
      geom_text(aes(label = labels),
          position = position_stack(vjust = 0.5), color="white", size = 3),
    
    ggplot(worn_minute_lowuse,mapping=aes(x="",y=percent_total,fill=worn)) +
      geom_bar(stat="identity",width = 1) +
      coord_polar("y",start = 0) +
      theme_minimal() + 
      labs(title="", subtitle = "Low use") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
      theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title=element_text(size=8),
          legend.key.height=unit(0.5,'cm'),
          legend.key.width=unit(0.3,'cm'),
          legend.text=element_text(size=8)) + 
      geom_text(aes(label = labels),
          position = position_stack(vjust = 0.5), color="white", size = 3),
  ncol=3),
nrow = 2)
```

![](Challenge2_DataVisualization_files/figure-gfm/Visuals%20of%20worn-minute%20patterns%20distribution%20in%203%20user%20groups-1.png)<!-- -->
These visuals give us a sense that none of the users would wear the
device all day. And once they have had a device like this, they tend to
wear them almost throughout the entire day. The key insight is if they
choose to wear everyday, or on specific days, for example on exercise
days.

### 2.2 How do user segments differ from each other in terms of daily calories burnt, total steps taken or distance traveled, and the total time of sleep?

We want to look at each user type’s amount of calories burnt, total
steps, distance traveled and total time of sleep and compare to each
other

``` r
daily_usage <- daily_usage %>% 
  mutate(date=as.POSIXct(date,format="%Y-%m-%d"))

sum_daily_usage <- daily_usage %>% 
  group_by(usage) %>% 
  summarize(avg_step = mean(totalsteps),avg_distance = mean(totaldistance), avg_calories=mean(calories), avg_sleep=mean(totalminutesasleep))

stats <- sum_daily_usage %>% 
  summarize_all(mean) %>% 
  mutate(usage="average")
```

    ## Warning in mean.default(usage): argument is not numeric or logical: returning NA

``` r
sum_daily_usage <- rbind(sum_daily_usage,stats) 

sum_daily_usage <- sum_daily_usage %>% 
  mutate(usage=factor(usage,levels = c('average','high use','medium use','low use'))) 

head(sum_daily_usage)
```

    ## # A tibble: 4 × 5
    ##   usage      avg_step avg_distance avg_calories avg_sleep
    ##   <fct>         <dbl>        <dbl>        <dbl>     <dbl>
    ## 1 high use      8922.         6.29        2388.      424.
    ## 2 low use       6655.         4.79        2592.      317.
    ## 3 medium use    7052.         4.97        2257.      455.
    ## 4 average       7543.         5.35        2412.      399.

``` r
ggarrange(
  
  ggplot(data=sum_daily_usage, mapping=aes(x=usage,y=avg_step))+
    geom_col(aes(fill=usage))+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")+
    labs(subtitle="Average daily step")+  
    scale_fill_manual(values=c("grey", "dark blue", "grey", "grey")),
  
  ggplot(data=sum_daily_usage, mapping=aes(x=usage,y=avg_distance))+
    geom_col(aes(fill=usage))+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")+
    labs(subtitle="Average daily distance")+  
    scale_fill_manual(values=c("grey", "dark blue", "grey", "grey")),
  
  ggplot(data=sum_daily_usage, mapping=aes(x=usage,y=avg_calories))+
    geom_col(aes(fill=usage))+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")+
    labs(subtitle="Average daily calories")+  
    scale_fill_manual(values=c("grey", "grey", "grey", "dark blue")),
  
  ggplot(data=sum_daily_usage, mapping=aes(x=usage,y=avg_sleep))+
    geom_col(aes(fill=usage))+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")+
    labs(subtitle="Average daily sleep")+  
    scale_fill_manual(values=c("grey", "grey", "grey", "dark blue"))
)
```

![](Challenge2_DataVisualization_files/figure-gfm/Compare%20Daily%20steps%20of%20each%20users-1.png)<!-- -->
From this data, we can notice that low-use users tend to burn more
calories then the others, however, they have the least sleep time among
the 3 groups. This raise a concern about health, which can be mitigated
by being more aware of their health using health tracker devices.

Plus, average sleep time of the people in this group is approximately
6.5 hours per day, which might be lower than the ideal of 8 hours.
Although sleep time for each person might vary and it also depends on
sleep quality, we should have a solution of increasing awareness on
sleep time to improve overall health.

There is one more to look at. That is how people spend time in there day
using time tracked by the devices.

``` r
overall_minute <- minute_worn %>% 
  group_by(usage) %>% 
  summarize(very_active = mean(veryactiveminutes)/1440, fairly_active =mean(fairlyactiveminutes)/1440, lightly_active = mean(lightlyactiveminutes)/1440, sedentary = mean(sedentaryminutes)/1440) %>% 
  mutate(untracked = 1 - very_active - fairly_active - lightly_active - sedentary)

avg_minute <- minute_worn %>% 
  summarize_all(mean) %>%
  mutate(usage="average",very_active = veryactiveminutes/1440, fairly_active = fairlyactiveminutes/1440, lightly_active = lightlyactiveminutes/1440, sedentary = sedentaryminutes/1440) %>% 
  select(usage,very_active, fairly_active, lightly_active, sedentary) %>% 
  mutate(untracked = 1 - very_active - fairly_active - lightly_active - sedentary)
```

    ## Warning in mean.default(usage): argument is not numeric or logical: returning NA

    ## Warning in mean.default(worn): argument is not numeric or logical: returning NA

``` r
head(overall_minute)
```

    ## # A tibble: 3 × 6
    ##   usage      very_active fairly_active lightly_active sedentary untracked
    ##   <chr>            <dbl>         <dbl>          <dbl>     <dbl>     <dbl>
    ## 1 high use       0.0200        0.0138           0.148     0.487     0.331
    ## 2 low use        0.0122        0.00850          0.130     0.625     0.224
    ## 3 medium use     0.00299       0.00571          0.184     0.450     0.357

``` r
head(avg_minute)
```

    ##     usage very_active fairly_active lightly_active sedentary untracked
    ## 1 average  0.01739329    0.01244749       0.150376 0.4945139 0.3252693

``` r
combined_minute <- rbind(overall_minute,avg_minute)

combined_minute <- combined_minute %>% 
  mutate(usage=factor(usage,levels = c('average','high use','medium use','low use'))) 

head(combined_minute)
```

    ## # A tibble: 4 × 6
    ##   usage      very_active fairly_active lightly_active sedentary untracked
    ##   <fct>            <dbl>         <dbl>          <dbl>     <dbl>     <dbl>
    ## 1 high use       0.0200        0.0138           0.148     0.487     0.331
    ## 2 low use        0.0122        0.00850          0.130     0.625     0.224
    ## 3 medium use     0.00299       0.00571          0.184     0.450     0.357
    ## 4 average        0.0174        0.0124           0.150     0.495     0.325

``` r
# turn data from wide to long

combined_minute_long <- combined_minute %>% 
  pivot_longer(cols = c('very_active','fairly_active','lightly_active','sedentary','untracked'), values_to = "percentage", names_to = "type") %>% 
  mutate(type=factor(type,levels = c('untracked','sedentary','lightly_active','fairly_active','very_active'))) 
  


head(combined_minute_long)
```

    ## # A tibble: 6 × 3
    ##   usage    type           percentage
    ##   <fct>    <fct>               <dbl>
    ## 1 high use very_active        0.0200
    ## 2 high use fairly_active      0.0138
    ## 3 high use lightly_active     0.148 
    ## 4 high use sedentary          0.487 
    ## 5 high use untracked          0.331 
    ## 6 low use  very_active        0.0122

``` r
ggarrange(
  ggplot(data=subset(combined_minute_long,usage=='average'),mapping = aes(x=percentage,y=type))+
    geom_col(aes(fill=type))+
    labs(title="Average percentage of time spent during the day", subtitle = "Overall")+
    scale_fill_manual(values=c( "dark blue", "dark blue","grey", "grey", "grey"))+
    theme(axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none"),
  
  ggarrange(
    ggplot(data=subset(combined_minute_long,usage=='high use'),mapping = aes(x=percentage,y=type))+
      geom_col(aes(fill=type))+
      labs(subtitle = "High use")+
      scale_fill_manual(values=c( "dark blue", "dark blue","grey", "grey", "grey"))+
      theme(axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none"),  
    
      ggplot(data=subset(combined_minute_long,usage=='medium use'),mapping = aes(x=percentage,y=type))+
      geom_col(aes(fill=type))+
      labs(subtitle = "Medium use")+
      scale_fill_manual(values=c( "dark blue", "dark blue","grey", "grey", "grey"))+
      theme(axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none"), 
      
      ggplot(data=subset(combined_minute_long,usage=='low use'),mapping = aes(x=percentage,y=type))+
      geom_col(aes(fill=type))+
      labs(subtitle = "Low use")+
      scale_fill_manual(values=c( "dark blue", "dark blue","grey", "grey", "grey"))+
      theme(axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none"),  
    ncol = 3
  ),
nrow=2
)
```

![](Challenge2_DataVisualization_files/figure-gfm/How%20people%20spend%20time%20in%20a%20day-1.png)<!-- -->

Compared to high/medium users, low-use users have more sedentary time.
This might indicate low-use users spend more time doing work at the same
spot and spend most of the calories for brain-work.

Plus, these low-use users have less un-tracked time than the other two
groups, which means they time to spend most time of the day with the
devices. This can be a leverage for Bellabeat to encourage this user
groups to focus more on their health by using the devices more in terms
of day used.

### 2.3 What are the correlations of these metrics within each segment, and how do they differ for each segment?

``` r
high_use <- daily_usage %>% 
  filter(usage=='high use')
medium_use <- daily_usage %>% 
  filter(usage=='medium use')
low_use <- daily_usage %>% 
  filter(usage=='low use')

head(high_use)
```

    ##           id X                date totalsteps totaldistance trackerdistance
    ## 1 1503960366 1 2016-04-11 17:00:00      13162          8.50            8.50
    ## 2 1503960366 2 2016-04-12 17:00:00      10735          6.97            6.97
    ## 3 1503960366 3 2016-04-14 17:00:00       9762          6.28            6.28
    ## 4 1503960366 4 2016-04-15 17:00:00      12669          8.16            8.16
    ## 5 1503960366 5 2016-04-16 17:00:00       9705          6.48            6.48
    ## 6 1503960366 6 2016-04-18 17:00:00      15506          9.88            9.88
    ##   loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                        0               1.88                     0.55
    ## 2                        0               1.57                     0.69
    ## 3                        0               2.14                     1.26
    ## 4                        0               2.71                     0.41
    ## 5                        0               3.19                     0.78
    ## 6                        0               3.53                     1.32
    ##   lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                6.06                       0                25
    ## 2                4.71                       0                21
    ## 3                2.83                       0                29
    ## 4                5.04                       0                36
    ## 5                2.51                       0                38
    ## 6                5.03                       0                50
    ##   fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                  13                  328              728     1985
    ## 2                  19                  217              776     1797
    ## 3                  34                  209              726     1745
    ## 4                  10                  221              773     1863
    ## 5                  20                  164              539     1728
    ## 6                  31                  264              775     2035
    ##   totalsleeprecords totalminutesasleep totaltimeinbed days_used    usage
    ## 1                 1                327            346        25 high use
    ## 2                 2                384            407        25 high use
    ## 3                 1                412            442        25 high use
    ## 4                 2                340            367        25 high use
    ## 5                 1                700            712        25 high use
    ## 6                 1                304            320        25 high use

``` r
head(medium_use)
```

    ##           id  X                date totalsteps totaldistance trackerdistance
    ## 1 2347167796 67 2016-04-12 17:00:00      10352          7.01            7.01
    ## 2 2347167796 68 2016-04-13 17:00:00      10129          6.70            6.70
    ## 3 2347167796 69 2016-04-14 17:00:00      10465          6.92            6.92
    ## 4 2347167796 70 2016-04-16 17:00:00       5472          3.62            3.62
    ## 5 2347167796 71 2016-04-17 17:00:00       8247          5.45            5.45
    ## 6 2347167796 72 2016-04-18 17:00:00       6711          4.44            4.44
    ##   loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                        0               1.66                     1.94
    ## 2                        0               0.02                     2.74
    ## 3                        0               0.07                     1.42
    ## 4                        0               0.08                     0.28
    ## 5                        0               0.79                     0.86
    ## 6                        0               0.00                     0.00
    ##   lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                3.41                       0                19
    ## 2                3.94                       0                 1
    ## 3                5.43                       0                 1
    ## 4                3.26                       0                 1
    ## 5                3.79                       0                11
    ## 6                4.44                       0                 0
    ##   fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                  32                  195              676     2038
    ## 2                  48                  206              705     2010
    ## 3                  24                  284              720     2133
    ## 4                   7                  249              508     1882
    ## 5                  16                  206              678     1944
    ## 6                   7                  382              648     2346
    ##   totalsleeprecords totalminutesasleep totaltimeinbed days_used      usage
    ## 1                 1                467            531        15 medium use
    ## 2                 1                445            489        15 medium use
    ## 3                 1                452            504        15 medium use
    ## 4                 1                556            602        15 medium use
    ## 5                 1                500            557        15 medium use
    ## 6                 1                465            514        15 medium use

``` r
head(low_use)
```

    ##           id  X                date totalsteps totaldistance trackerdistance
    ## 1 1644430081 26 2016-04-28 17:00:00       3176          2.31            2.31
    ## 2 1644430081 27 2016-04-29 17:00:00      18213         13.24           13.24
    ## 3 1644430081 28 2016-05-01 17:00:00       3758          2.73            2.73
    ## 4 1644430081 29 2016-05-07 17:00:00       6724          4.89            4.89
    ## 5 1844505072 30 2016-04-14 17:00:00       3844          2.54            2.54
    ## 6 1844505072 31 2016-04-29 17:00:00       4014          2.67            2.67
    ##   loggedactivitiesdistance veryactivedistance moderatelyactivedistance
    ## 1                        0               0.00                     0.00
    ## 2                        0               0.63                     3.14
    ## 3                        0               0.07                     0.31
    ## 4                        0               0.00                     0.00
    ## 5                        0               0.00                     0.00
    ## 6                        0               0.00                     0.00
    ##   lightactivedistance sedentaryactivedistance veryactiveminutes
    ## 1                2.31                       0                 0
    ## 2                9.46                       0                 9
    ## 3                2.35                       0                 1
    ## 4                4.88                       0                 0
    ## 5                2.54                       0                 0
    ## 6                2.65                       0                 0
    ##   fairlyactiveminutes lightlyactiveminutes sedentaryminutes calories
    ## 1                   0                  120             1193     2498
    ## 2                  71                  402              816     3846
    ## 3                   7                  148              682     2580
    ## 4                   0                  295              991     2987
    ## 5                   0                  176              527     1725
    ## 6                   0                  184              218     1763
    ##   totalsleeprecords totalminutesasleep totaltimeinbed days_used   usage
    ## 1                 1                119            127         4 low use
    ## 2                 1                124            142         4 low use
    ## 3                 1                796            961         4 low use
    ## 4                 1                137            154         4 low use
    ## 5                 1                644            961         3 low use
    ## 6                 1                722            961         3 low use

``` r
ggarrange(
  ggscatter(high_use, x = "totalsteps", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Calories Burnt"),
  
  ggscatter(high_use, x = "totalsteps", y = "totalminutesasleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Sleep Time")
)
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20for%20high%20use-1.png)<!-- -->

``` r
ggarrange(
  ggscatter(medium_use, x = "totalsteps", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Calories Burnt"),
  
  ggscatter(medium_use, x = "totalsteps", y = "totalminutesasleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Sleep Time")
)
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20for%20medium%20use-1.png)<!-- -->

``` r
ggarrange(
  ggscatter(low_use, x = "totalsteps", y = "calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Calories Burnt"),
  
  ggscatter(low_use, x = "totalsteps", y = "totalminutesasleep", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily Steps", ylab = "Daily Sleep Time")
)
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](Challenge2_DataVisualization_files/figure-gfm/Correlation%20for%20low%20use-1.png)<!-- -->

These graphs show same pattern with our previous conclusions, which are
that steps taken correlates strongly with calories burnt, and barely
correlates with the sleep time.

## 3. How could these trends help influence Bellabeat’s marketing strategy?

### 3.1 Key findings

-   Metrics-wise:
    -   Hourly/daily steps taken and hourly/daily calories burnt highly
        correlate, which can have huge impact on health benefits.
    -   Daily steps taken does not correlate much with the daily sleep
        time.
    -   People tend to have more step counts at 12 PM to 2 PM which is
        lunch time and refreshment for afternoon shift, and 5 PM - 7 PM
        which is after-work exercise sessions.
-   Customer segments: high-use users accounted for 50% of the sample,
    following by low-use users with 38% and medium-use users with 12%.
    -   All user groups have the tendency to wear the devices throughout
        the day (accounted for \~ 90-95% of the users).
    -   Low-use users have a key pain point of having more calories
        burnt and significant low sleep time with average steps and high
        sedentary time on devices. On the bright side, they have low
        un-tracked time
    -   High-use users tend to take more steps compared to the other
        groups, which is recommended for weight loss or better heart
        rate tendency or work efficiency.
    -   All groups have a low sleep time if compared to standard sleep
        time of 8 hours.

### 3.2 What key selling points/propositions can Bellabeat use to address each customer’s need/behavior?

Positioned as **a tech-driven wellness company for women**, Bellabeat
has its own key selling points of making its tracker devices more
fashionable way, such as necklace, jewelry, or clip. These forms of
devices make Bellabeat more accessible for women who wants to be healthy
and stylish at the same time.

### 3.3 What are the overall marketing and growth strategies for Bellabeat based on those key selling points/propositions and the findings we have?

There are several verticals we should focus on to develop our overall
strategy:

-   Propositions (Key selling points): a tech-driven wellness company
    for women with selling points of feminism and fashionable designed
    tracker.

-   Promotion focuses on campaigns that increase the usage frequency for
    low/medium users and retain that of high users.

-   Place (Promotion channels): focuses on partnership with
    health-related association/websites or fashionable brands to
    increase the connection between having a great health (both physical
    and mental) and keeping track of it.

*Note: As data does not show sales channel figures, this part will focus
more on promotion channels where potential users can be aware of the
product and existing users can interact/engage with our products more
actively and efficiently.*

-   Price (which relates to device price and membership fee): focuses on
    building a fee-structure that focused on the key products. For
    example, the cost of tracker devices can be low as being the access
    for users to get to the membership to keep track of their health. It
    could be a bundle price scheme.

-   Product (products’ ecosystem): focuses on products that are related
    to the analysis data, such as Leaf, Time, the membership package,
    and the Bellabeat app.
