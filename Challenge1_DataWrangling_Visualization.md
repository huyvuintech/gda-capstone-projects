Google Data Analytics Certificate’s Capstone Project 1 - Cyclistic
bike-share analysis
================
Quang Huy Vu
Data from Sep 2021 to Aug 2022

## Introduction:

This documentation is made to record all steps taken during the data
cleaning process for the first Google Data Analytics Certificate’s
capstone project.

The outline for this documentation would be:

1.  Set up the packages to manipulate raw data.

2.  Import and combine csv raw data files into one in R.

3.  Check data for errors and transform data for further analyses.

4.  Conduct descriptive analyses.

5.  Export clean data to one completed csv file.

## 1. Set up the packages to manipulate raw data

First, set up the packages for data wrangling and cleaning:

-   tidyverse: wrangle data.

-   lubridate: wrangle date attributes.

-   skimr: enable summary function for prescriptive analytics of each
    individual column.

-   janitor: check data compatibility and combine csv files.

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

## 2. Import and combine csv raw data files into one in R

Before importing csv files, R requires to access the working directory
that contains the files in the local computer.

File’s working directory: “D:Data Analytics CertificateProject\_data”

*Note: Change  to / to match R syntax.*

``` r
setwd("D:/Learning/Google Data Analytics Certificate/Capstone Project/Project 1/total_data")
getwd()
```

    ## [1] "D:/Learning/Google Data Analytics Certificate/Capstone Project/Project 1/total_data"

The next step is to import csv files to R and process it.

``` r
trip_202208 <- read.csv("202208-divvy-tripdata.csv")
trip_202207 <- read.csv("202207-divvy-tripdata.csv")
trip_202206 <- read.csv("202206-divvy-tripdata.csv")
trip_202205 <- read.csv("202205-divvy-tripdata.csv")
trip_202204 <- read.csv("202204-divvy-tripdata.csv")
trip_202203 <- read.csv("202203-divvy-tripdata.csv")
trip_202202 <- read.csv("202202-divvy-tripdata.csv")
trip_202201 <- read.csv("202201-divvy-tripdata.csv")
trip_202112 <- read.csv("202112-divvy-tripdata.csv")
trip_202111 <- read.csv("202111-divvy-tripdata.csv")
trip_202110 <- read.csv("202110-divvy-tripdata.csv")
trip_202109 <- read.csv("202109-divvy-tripdata.csv")
```

Let’s review some of the imported data frames.

``` r
head(trip_202208)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 550CF7EFEAE0C618 electric_bike 2022-08-07 21:34:15 2022-08-07 21:41:46
    ## 2 DAD198F405F9C5F5 electric_bike 2022-08-08 14:39:21 2022-08-08 14:53:23
    ## 3 E6F2BC47B65CB7FD electric_bike 2022-08-08 15:29:50 2022-08-08 15:40:34
    ## 4 F597830181C2E13C electric_bike 2022-08-08 02:43:50 2022-08-08 02:58:53
    ## 5 0CE689BB4E313E8D electric_bike 2022-08-07 20:24:06 2022-08-07 20:29:58
    ## 6 BFA7E7CC69860C20 electric_bike 2022-08-08 13:06:08 2022-08-08 13:19:09
    ##   start_station_name start_station_id end_station_name end_station_id start_lat
    ## 1                                                                         41.93
    ## 2                                                                         41.89
    ## 3                                                                         41.97
    ## 4                                                                         41.94
    ## 5                                                                         41.85
    ## 6                                                                         41.79
    ##   start_lng end_lat end_lng member_casual
    ## 1    -87.69   41.94  -87.72        casual
    ## 2    -87.64   41.92  -87.64        casual
    ## 3    -87.69   41.97  -87.66        casual
    ## 4    -87.65   41.97  -87.69        casual
    ## 5    -87.65   41.84  -87.66        casual
    ## 6    -87.72   41.82  -87.69        casual

``` r
head(trip_202207)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 954144C2F67B1932  classic_bike 2022-07-05 08:12:47 2022-07-05 08:24:32
    ## 2 292E027607D218B6  classic_bike 2022-07-26 12:53:38 2022-07-26 12:55:31
    ## 3 57765852588AD6E0  classic_bike 2022-07-03 13:58:49 2022-07-03 14:06:32
    ## 4 B5B6BE44314590E6  classic_bike 2022-07-31 17:44:21 2022-07-31 18:42:50
    ## 5 A4C331F2A00E79E0  classic_bike 2022-07-13 19:49:06 2022-07-13 20:15:24
    ## 6 579D73BE2ED880B3 electric_bike 2022-07-01 17:04:35 2022-07-01 17:13:18
    ##            start_station_name start_station_id               end_station_name
    ## 1  Ashland Ave & Blackhawk St            13224       Kingsbury St & Kinzie St
    ## 2  Buckingham Fountain (Temp)            15541          Michigan Ave & 8th St
    ## 3  Buckingham Fountain (Temp)            15541          Michigan Ave & 8th St
    ## 4  Buckingham Fountain (Temp)            15541         Woodlawn Ave & 55th St
    ## 5      Wabash Ave & Grand Ave     TA1307000117 Sheffield Ave & Wellington Ave
    ## 6 Desplaines St & Randolph St            15535      Clinton St & Roosevelt Rd
    ##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
    ## 1   KA1503000043  41.90707 -87.66725 41.88918 -87.63851        member
    ## 2            623  41.86962 -87.62398 41.87277 -87.62398        casual
    ## 3            623  41.86962 -87.62398 41.87277 -87.62398        casual
    ## 4   TA1307000164  41.86962 -87.62398 41.79526 -87.59647        casual
    ## 5   TA1307000052  41.89147 -87.62676 41.93625 -87.65266        member
    ## 6         WL-008  41.88461 -87.64456 41.86712 -87.64109        member

``` r
head(trip_202206)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 600CFD130D0FD2A4 electric_bike 2022-06-30 17:27:53 2022-06-30 17:35:15
    ## 2 F5E6B5C1682C6464 electric_bike 2022-06-30 18:39:52 2022-06-30 18:47:28
    ## 3 B6EB6D27BAD771D2 electric_bike 2022-06-30 11:49:25 2022-06-30 12:02:54
    ## 4 C9C320375DE1D5C6 electric_bike 2022-06-30 11:15:25 2022-06-30 11:19:43
    ## 5 56C055851023BE98 electric_bike 2022-06-29 23:36:50 2022-06-29 23:45:17
    ## 6 B664188E8163D045 electric_bike 2022-06-30 16:42:10 2022-06-30 16:58:22
    ##   start_station_name start_station_id end_station_name end_station_id start_lat
    ## 1                                                                         41.89
    ## 2                                                                         41.91
    ## 3                                                                         41.91
    ## 4                                                                         41.80
    ## 5                                                                         41.91
    ## 6                                                                         42.03
    ##   start_lng end_lat end_lng member_casual
    ## 1    -87.62   41.91  -87.62        casual
    ## 2    -87.62   41.93  -87.63        casual
    ## 3    -87.65   41.89  -87.61        casual
    ## 4    -87.66   41.80  -87.65        casual
    ## 5    -87.63   41.93  -87.64        casual
    ## 6    -87.71   42.06  -87.73        casual

``` r
head(trip_202205)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 EC2DE40644C6B0F4  classic_bike 2022-05-23 23:06:58 2022-05-23 23:40:19
    ## 2 1C31AD03897EE385  classic_bike 2022-05-11 08:53:28 2022-05-11 09:31:22
    ## 3 1542FBEC830415CF  classic_bike 2022-05-26 18:36:28 2022-05-26 18:58:18
    ## 4 6FF59852924528F8  classic_bike 2022-05-10 07:30:07 2022-05-10 07:38:49
    ## 5 483C52CAAE12E3AC  classic_bike 2022-05-10 17:31:56 2022-05-10 17:36:57
    ## 6 C0A3AA5A614DCE01  classic_bike 2022-05-04 14:48:55 2022-05-04 14:56:04
    ##                  start_station_name start_station_id
    ## 1            Wabash Ave & Grand Ave     TA1307000117
    ## 2 DuSable Lake Shore Dr & Monroe St            13300
    ## 3           Clinton St & Madison St     TA1305000032
    ## 4           Clinton St & Madison St     TA1305000032
    ## 5           Clinton St & Madison St     TA1305000032
    ## 6           Carpenter St & Huron St            13196
    ##                end_station_name end_station_id start_lat start_lng  end_lat
    ## 1        Halsted St & Roscoe St   TA1309000025  41.89147 -87.62676 41.94367
    ## 2   Field Blvd & South Water St          15534  41.88096 -87.61674 41.88635
    ## 3       Wood St & Milwaukee Ave          13221  41.88224 -87.64107 41.90765
    ## 4        Clark St & Randolph St   TA1305000030  41.88224 -87.64107 41.88458
    ## 5           Morgan St & Lake St   TA1306000015  41.88224 -87.64107 41.88578
    ## 6 Sangamon St & Washington Blvd          13409  41.89456 -87.65345 41.88316
    ##     end_lng member_casual
    ## 1 -87.64895        member
    ## 2 -87.61752        member
    ## 3 -87.67255        member
    ## 4 -87.63189        member
    ## 5 -87.65102        member
    ## 6 -87.65110        member

``` r
head(trip_202204)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 3564070EEFD12711 electric_bike 2022-04-06 17:42:48 2022-04-06 17:54:36
    ## 2 0B820C7FCF22F489  classic_bike 2022-04-24 19:23:07 2022-04-24 19:43:17
    ## 3 89EEEE32293F07FF  classic_bike 2022-04-20 19:29:08 2022-04-20 19:35:16
    ## 4 84D4751AEB31888D  classic_bike 2022-04-22 21:14:06 2022-04-22 21:23:29
    ## 5 5664BCF0D1DE7A8B electric_bike 2022-04-16 15:56:30 2022-04-16 16:02:11
    ## 6 AA9EB7BD2E1FC128  classic_bike 2022-04-21 16:52:33 2022-04-21 16:56:51
    ##            start_station_name start_station_id         end_station_name
    ## 1      Paulina St & Howard St              515  University Library (NU)
    ## 2   Wentworth Ave & Cermak Rd            13075    Green St & Madison St
    ## 3        Halsted St & Polk St     TA1307000121    Green St & Madison St
    ## 4   Wentworth Ave & Cermak Rd            13075 Delano Ct & Roosevelt Rd
    ## 5        Halsted St & Polk St     TA1307000121  Clinton St & Madison St
    ## 6 Desplaines St & Randolph St            15535      Canal St & Adams St
    ##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
    ## 1            605  42.01913 -87.67353 42.05294 -87.67345        member
    ## 2   TA1307000120  41.85308 -87.63193 41.88189 -87.64879        member
    ## 3   TA1307000120  41.87184 -87.64664 41.88189 -87.64879        member
    ## 4   KA1706005007  41.85308 -87.63193 41.86749 -87.63219        casual
    ## 5   TA1305000032  41.87181 -87.64657 41.88224 -87.64107        member
    ## 6          13011  41.88462 -87.64457 41.87926 -87.63990        member

``` r
head(trip_202203)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 47EC0A7F82E65D52  classic_bike 2022-03-21 13:45:01 2022-03-21 13:51:18
    ## 2 8494861979B0F477 electric_bike 2022-03-16 09:37:16 2022-03-16 09:43:34
    ## 3 EFE527AF80B66109  classic_bike 2022-03-23 19:52:02 2022-03-23 19:54:48
    ## 4 9F446FD9DEE3F389  classic_bike 2022-03-01 19:12:26 2022-03-01 19:22:14
    ## 5 431128AD9AFFEDC0  classic_bike 2022-03-21 18:37:01 2022-03-21 19:19:11
    ## 6 9AA8A13AF7A85325  classic_bike 2022-03-07 17:10:22 2022-03-07 17:15:04
    ##                   start_station_name start_station_id
    ## 1             Wabash Ave & Wacker Pl     TA1307000131
    ## 2              Michigan Ave & Oak St            13042
    ## 3              Broadway & Berwyn Ave            13109
    ## 4             Wabash Ave & Wacker Pl     TA1307000131
    ## 5 DuSable Lake Shore Dr & North Blvd           LF-005
    ## 6          Bissell St & Armitage Ave            13059
    ##                       end_station_name end_station_id start_lat start_lng
    ## 1             Kingsbury St & Kinzie St   KA1503000043  41.88688 -87.62603
    ## 2 Orleans St & Chestnut St (NEXT Apts)            620  41.90100 -87.62375
    ## 3                 Broadway & Ridge Ave          15578  41.97835 -87.65975
    ## 4           Franklin St & Jackson Blvd   TA1305000025  41.88688 -87.62603
    ## 5             Loomis St & Jackson Blvd          13206  41.91172 -87.62680
    ## 6         Southport Ave & Clybourn Ave   TA1309000030  41.91802 -87.65218
    ##    end_lat   end_lng member_casual
    ## 1 41.88918 -87.63851        member
    ## 2 41.89820 -87.63754        member
    ## 3 41.98404 -87.66027        member
    ## 4 41.87771 -87.63532        member
    ## 5 41.87794 -87.66201        member
    ## 6 41.92077 -87.66371        member

``` r
head(trip_202202)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 E1E065E7ED285C02  classic_bike 2022-02-19 18:08:41 2022-02-19 18:23:56
    ## 2 1602DCDC5B30FFE3  classic_bike 2022-02-20 17:41:30 2022-02-20 17:45:56
    ## 3 BE7DD2AF4B55C4AF  classic_bike 2022-02-25 18:55:56 2022-02-25 19:09:34
    ## 4 A1789BDF844412BE  classic_bike 2022-02-14 11:57:03 2022-02-14 12:04:00
    ## 5 07DE78092C62F7B3  classic_bike 2022-02-16 05:36:06 2022-02-16 05:39:00
    ## 6 9A2F204F04AB7E24  classic_bike 2022-02-07 09:51:57 2022-02-07 10:07:53
    ##             start_station_name start_station_id               end_station_name
    ## 1       State St & Randolph St     TA1305000029         Clark St & Lincoln Ave
    ## 2  Halsted St & Wrightwood Ave     TA1309000061 Southport Ave & Wrightwood Ave
    ## 3       State St & Randolph St     TA1305000029            Canal St & Adams St
    ## 4 Southport Ave & Waveland Ave            13235         Broadway & Sheridan Rd
    ## 5       State St & Randolph St     TA1305000029          Franklin St & Lake St
    ## 6       St. Clair St & Erie St            13016        Franklin St & Monroe St
    ##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
    ## 1          13179  41.88462 -87.62783 41.91569 -87.63460        member
    ## 2   TA1307000113  41.92914 -87.64908 41.92877 -87.66391        member
    ## 3          13011  41.88462 -87.62783 41.87926 -87.63990        member
    ## 4          13323  41.94815 -87.66394 41.95283 -87.64999        member
    ## 5   TA1307000111  41.88462 -87.62783 41.88584 -87.63550        member
    ## 6   TA1309000007  41.89435 -87.62280 41.88032 -87.63519        member

``` r
head(trip_202201)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 C2F7DD78E82EC875 electric_bike 2022-01-13 11:59:47 2022-01-13 12:02:44
    ## 2 A6CF8980A652D272 electric_bike 2022-01-10 08:41:56 2022-01-10 08:46:17
    ## 3 BD0F91DFF741C66D  classic_bike 2022-01-25 04:53:40 2022-01-25 04:58:01
    ## 4 CBB80ED419105406  classic_bike 2022-01-04 00:18:04 2022-01-04 00:33:00
    ## 5 DDC963BFDDA51EEA  classic_bike 2022-01-20 01:31:10 2022-01-20 01:37:12
    ## 6 A39C6F6CC0586C0B  classic_bike 2022-01-11 18:48:09 2022-01-11 18:51:31
    ##              start_station_name start_station_id              end_station_name
    ## 1      Glenwood Ave & Touhy Ave              525          Clark St & Touhy Ave
    ## 2      Glenwood Ave & Touhy Ave              525          Clark St & Touhy Ave
    ## 3 Sheffield Ave & Fullerton Ave     TA1306000016 Greenview Ave & Fullerton Ave
    ## 4      Clark St & Bryn Mawr Ave     KA1504000151     Paulina St & Montrose Ave
    ## 5   Michigan Ave & Jackson Blvd     TA1309000002        State St & Randolph St
    ## 6         Wood St & Chicago Ave              637       Honore St & Division St
    ##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
    ## 1         RP-007  42.01280 -87.66591 42.01256 -87.67437        casual
    ## 2         RP-007  42.01276 -87.66597 42.01256 -87.67437        casual
    ## 3   TA1307000001  41.92560 -87.65371 41.92533 -87.66580        member
    ## 4   TA1309000021  41.98359 -87.66915 41.96151 -87.67139        casual
    ## 5   TA1305000029  41.87785 -87.62408 41.88462 -87.62783        member
    ## 6   TA1305000034  41.89563 -87.67207 41.90312 -87.67394        member

``` r
head(trip_202112)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 46F8167220E4431F electric_bike 2021-12-07 15:06:07 2021-12-07 15:13:42
    ## 2 73A77762838B32FD electric_bike 2021-12-11 03:43:29 2021-12-11 04:10:23
    ## 3 4CF42452054F59C5 electric_bike 2021-12-15 23:10:28 2021-12-15 23:23:14
    ## 4 3278BA87BF698339  classic_bike 2021-12-26 16:16:10 2021-12-26 16:30:53
    ## 5 6FF54232576A3B73 electric_bike 2021-12-30 11:31:05 2021-12-30 11:51:21
    ## 6 93E8D79490E3AB11  classic_bike 2021-12-01 18:28:36 2021-12-01 18:38:03
    ##             start_station_name start_station_id           end_station_name
    ## 1     Laflin St & Cullerton St            13307        Morgan St & Polk St
    ## 2        LaSalle Dr & Huron St     KP1705001026 Clarendon Ave & Leland Ave
    ## 3 Halsted St & North Branch St     KA1504000117       Broadway & Barry Ave
    ## 4 Halsted St & North Branch St     KA1504000117      LaSalle Dr & Huron St
    ## 5     Leavitt St & Chicago Ave            18058     Clark St & Drummond Pl
    ## 6         Wabash Ave & 16th St           SL-012         Wells St & Polk St
    ##   end_station_id start_lat start_lng  end_lat   end_lng member_casual
    ## 1   TA1307000130  41.85483 -87.66366 41.87197 -87.65097        member
    ## 2   TA1307000119  41.89441 -87.63233 41.96797 -87.65000        casual
    ## 3          13137  41.89936 -87.64852 41.93758 -87.64410        member
    ## 4   KP1705001026  41.89939 -87.64854 41.89488 -87.63233        member
    ## 5   TA1307000142  41.89558 -87.68202 41.93125 -87.64434        member
    ## 6         SL-011  41.86038 -87.62581 41.87260 -87.63350        member

``` r
head(trip_202111)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 7C00A93E10556E47 electric_bike 2021-11-27 13:27:38 2021-11-27 13:46:38
    ## 2 90854840DFD508BA electric_bike 2021-11-27 13:38:25 2021-11-27 13:56:10
    ## 3 0A7D10CDD144061C electric_bike 2021-11-26 22:03:34 2021-11-26 22:05:56
    ## 4 2F3BE33085BCFF02 electric_bike 2021-11-27 09:56:49 2021-11-27 10:01:50
    ## 5 D67B4781A19928D4 electric_bike 2021-11-26 19:09:28 2021-11-26 19:30:41
    ## 6 02F85C2C3C5F7D46 electric_bike 2021-11-26 18:34:07 2021-11-26 18:52:49
    ##      start_station_name start_station_id end_station_name end_station_id
    ## 1                                                                       
    ## 2                                                                       
    ## 3                                                                       
    ## 4                                                                       
    ## 5                                                                       
    ## 6 Michigan Ave & Oak St            13042                                
    ##   start_lat start_lng end_lat end_lng member_casual
    ## 1  41.93000 -87.72000   41.96  -87.73        casual
    ## 2  41.96000 -87.70000   41.92  -87.70        casual
    ## 3  41.96000 -87.70000   41.96  -87.70        casual
    ## 4  41.94000 -87.79000   41.93  -87.79        casual
    ## 5  41.90000 -87.63000   41.88  -87.62        casual
    ## 6  41.90086 -87.62379   41.90  -87.63        casual

``` r
head(trip_202110)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 620BC6107255BF4C electric_bike 2021-10-22 12:46:42 2021-10-22 12:49:50
    ## 2 4471C70731AB2E45 electric_bike 2021-10-21 09:12:37 2021-10-21 09:14:14
    ## 3 26CA69D43D15EE14 electric_bike 2021-10-16 16:28:39 2021-10-16 16:36:26
    ## 4 362947F0437E1514 electric_bike 2021-10-16 16:17:48 2021-10-16 16:19:03
    ## 5 BB731DE2F2EC51C5 electric_bike 2021-10-20 23:17:54 2021-10-20 23:26:10
    ## 6 7176307BBC097313 electric_bike 2021-10-21 16:57:37 2021-10-21 17:11:58
    ##         start_station_name start_station_id end_station_name end_station_id
    ## 1 Kingsbury St & Kinzie St     KA1503000043                                
    ## 2                                                                          
    ## 3                                                                          
    ## 4                                                                          
    ## 5                                                                          
    ## 6                                                                          
    ##   start_lat start_lng end_lat end_lng member_casual
    ## 1  41.88919  -87.6385   41.89  -87.63        member
    ## 2  41.93000  -87.7000   41.93  -87.71        member
    ## 3  41.92000  -87.7000   41.94  -87.72        member
    ## 4  41.92000  -87.6900   41.92  -87.69        member
    ## 5  41.89000  -87.7100   41.89  -87.69        member
    ## 6  41.89000  -87.7100   41.93  -87.70        member

``` r
head(trip_202109)
```

    ##            ride_id rideable_type          started_at            ended_at
    ## 1 9DC7B962304CBFD8 electric_bike 2021-09-28 16:07:10 2021-09-28 16:09:54
    ## 2 F930E2C6872D6B32 electric_bike 2021-09-28 14:24:51 2021-09-28 14:40:05
    ## 3 6EF72137900BB910 electric_bike 2021-09-28 00:20:16 2021-09-28 00:23:57
    ## 4 78D1DE133B3DBF55 electric_bike 2021-09-28 14:51:17 2021-09-28 15:00:06
    ## 5 E03D4ACDCAEF6E00 electric_bike 2021-09-28 09:53:12 2021-09-28 10:03:44
    ## 6 346DE323A2677DC0 electric_bike 2021-09-28 01:53:18 2021-09-28 02:00:02
    ##   start_station_name start_station_id end_station_name end_station_id start_lat
    ## 1                                                                         41.89
    ## 2                                                                         41.94
    ## 3                                                                         41.81
    ## 4                                                                         41.80
    ## 5                                                                         41.88
    ## 6                                                                         41.87
    ##   start_lng end_lat end_lng member_casual
    ## 1    -87.68   41.89  -87.67        casual
    ## 2    -87.64   41.98  -87.67        casual
    ## 3    -87.72   41.80  -87.72        casual
    ## 4    -87.72   41.81  -87.72        casual
    ## 5    -87.74   41.88  -87.71        casual
    ## 6    -87.75   41.88  -87.74        casual

After that, we need to check the compatibility among files before
combining these together.

There are compatibility bullet points to check:

-   The number of columns.

-   The headers of the columns.

*Note: The order of columns might not be exactly the same for every data
files, yet the headers’ names must be EXACTLY the same.*

We will first check to see if all data frames had the same number of
columns.

``` r
compare_df_cols(trip_202208,trip_202207,trip_202206,trip_202205,trip_202204,trip_202203,trip_202202,trip_202201,trip_202112,trip_202111,trip_202110,trip_202109)
```

    ##           column_name trip_202208 trip_202207 trip_202206 trip_202205
    ## 1             end_lat     numeric     numeric     numeric     numeric
    ## 2             end_lng     numeric     numeric     numeric     numeric
    ## 3      end_station_id   character   character   character   character
    ## 4    end_station_name   character   character   character   character
    ## 5            ended_at   character   character   character   character
    ## 6       member_casual   character   character   character   character
    ## 7             ride_id   character   character   character   character
    ## 8       rideable_type   character   character   character   character
    ## 9           start_lat     numeric     numeric     numeric     numeric
    ## 10          start_lng     numeric     numeric     numeric     numeric
    ## 11   start_station_id   character   character   character   character
    ## 12 start_station_name   character   character   character   character
    ## 13         started_at   character   character   character   character
    ##    trip_202204 trip_202203 trip_202202 trip_202201 trip_202112 trip_202111
    ## 1      numeric     numeric     numeric     numeric     numeric     numeric
    ## 2      numeric     numeric     numeric     numeric     numeric     numeric
    ## 3    character   character   character   character   character   character
    ## 4    character   character   character   character   character   character
    ## 5    character   character   character   character   character   character
    ## 6    character   character   character   character   character   character
    ## 7    character   character   character   character   character   character
    ## 8    character   character   character   character   character   character
    ## 9      numeric     numeric     numeric     numeric     numeric     numeric
    ## 10     numeric     numeric     numeric     numeric     numeric     numeric
    ## 11   character   character   character   character   character   character
    ## 12   character   character   character   character   character   character
    ## 13   character   character   character   character   character   character
    ##    trip_202110 trip_202109
    ## 1      numeric     numeric
    ## 2      numeric     numeric
    ## 3    character   character
    ## 4    character   character
    ## 5    character   character
    ## 6    character   character
    ## 7    character   character
    ## 8    character   character
    ## 9      numeric     numeric
    ## 10     numeric     numeric
    ## 11   character   character
    ## 12   character   character
    ## 13   character   character

Seems like everything is matched. Yet, we want to make sure there is no
human mistakes during the processing stage. We would try to test these
files to return only mismatched columns and define missing columns as
mismatched.

``` r
compare_df_cols(trip_202208,trip_202207,trip_202206,trip_202205,trip_202204,trip_202203,trip_202202,trip_202201,trip_202112,trip_202111,trip_202110,trip_202109, return = "mismatch", bind_method = "rbind")
```

    ##  [1] column_name trip_202208 trip_202207 trip_202206 trip_202205 trip_202204
    ##  [7] trip_202203 trip_202202 trip_202201 trip_202112 trip_202111 trip_202110
    ## [13] trip_202109
    ## <0 rows> (or 0-length row.names)

It’s a match! There are no mismatched columns presented. Now we would
proceed to combine these files together.

``` r
total <- bind_rows(trip_202208,trip_202207,trip_202206,trip_202205,trip_202204,trip_202203,trip_202202,trip_202201,trip_202112,trip_202111,trip_202110,trip_202109, id = NULL)
```

Now all data frames are combined into one data frame named **total**. We
can try to review the data frame to see if there is any issue with the
combining process.

``` r
glimpse(total)
```

    ## Rows: 5,883,043
    ## Columns: 13
    ## $ ride_id            <chr> "550CF7EFEAE0C618", "DAD198F405F9C5F5", "E6F2BC47B6…
    ## $ rideable_type      <chr> "electric_bike", "electric_bike", "electric_bike", …
    ## $ started_at         <chr> "2022-08-07 21:34:15", "2022-08-08 14:39:21", "2022…
    ## $ ended_at           <chr> "2022-08-07 21:41:46", "2022-08-08 14:53:23", "2022…
    ## $ start_station_name <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_station_id   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_name   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_id     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_lat          <dbl> 41.93, 41.89, 41.97, 41.94, 41.85, 41.79, 41.89, 41…
    ## $ start_lng          <dbl> -87.69, -87.64, -87.69, -87.65, -87.65, -87.72, -87…
    ## $ end_lat            <dbl> 41.94, 41.92, 41.97, 41.97, 41.84, 41.82, 41.89, 41…
    ## $ end_lng            <dbl> -87.72, -87.64, -87.66, -87.69, -87.66, -87.69, -87…
    ## $ member_casual      <chr> "casual", "casual", "casual", "casual", "casual", "…

Now we could say the combination is a success. There are a total of
nearly 6 million rows, which are the total rows of 12 data frames
combined.

Next, we would have to change some of the columns’ names as they might
be confusing to convey the data within.

``` r
total <- total %>% 
  rename(ride_id = ride_id, 
         ride_type = rideable_type, 
         start_time = started_at, 
         end_time = ended_at,
         user_type = member_casual)
```

Now, we try to review the new column name.

``` r
colnames(total)
```

    ##  [1] "ride_id"            "ride_type"          "start_time"        
    ##  [4] "end_time"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "user_type"

## 3. Check data for errors and transform data for further analyses

The next big step is to find errors lying in the data. There is two
columns where we could check if the data is valid. The end time of a
specific ride should be bigger than the start time.

Now we could create a column name **ride_length** and the value of valid
observations should be larger than 0.

``` r
total <- total %>% 
  mutate(ride_length = difftime(total$end_time,total$start_time))
glimpse(total)
```

    ## Rows: 5,883,043
    ## Columns: 14
    ## $ ride_id            <chr> "550CF7EFEAE0C618", "DAD198F405F9C5F5", "E6F2BC47B6…
    ## $ ride_type          <chr> "electric_bike", "electric_bike", "electric_bike", …
    ## $ start_time         <chr> "2022-08-07 21:34:15", "2022-08-08 14:39:21", "2022…
    ## $ end_time           <chr> "2022-08-07 21:41:46", "2022-08-08 14:53:23", "2022…
    ## $ start_station_name <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_station_id   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_name   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_id     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_lat          <dbl> 41.93, 41.89, 41.97, 41.94, 41.85, 41.79, 41.89, 41…
    ## $ start_lng          <dbl> -87.69, -87.64, -87.69, -87.65, -87.65, -87.72, -87…
    ## $ end_lat            <dbl> 41.94, 41.92, 41.97, 41.97, 41.84, 41.82, 41.89, 41…
    ## $ end_lng            <dbl> -87.72, -87.64, -87.66, -87.69, -87.66, -87.69, -87…
    ## $ user_type          <chr> "casual", "casual", "casual", "casual", "casual", "…
    ## $ ride_length        <drtn> 451 secs, 842 secs, 644 secs, 903 secs, 352 secs, …

The data type of ride_length column (total number seconds) is the
combination of numeric values and character. We would want to check and
change the data type of this column to numeric.

``` r
is.factor(total$ride_length)
```

    ## [1] FALSE

``` r
total$ride_length <- as.numeric(total$ride_length)
is.numeric(total$ride_length)
```

    ## [1] TRUE

Then, we would remove rows with ride_length values less than 0.

``` r
total2 <- total[!(total$ride_length<0),]
glimpse(total2)
```

    ## Rows: 5,882,908
    ## Columns: 14
    ## $ ride_id            <chr> "550CF7EFEAE0C618", "DAD198F405F9C5F5", "E6F2BC47B6…
    ## $ ride_type          <chr> "electric_bike", "electric_bike", "electric_bike", …
    ## $ start_time         <chr> "2022-08-07 21:34:15", "2022-08-08 14:39:21", "2022…
    ## $ end_time           <chr> "2022-08-07 21:41:46", "2022-08-08 14:53:23", "2022…
    ## $ start_station_name <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_station_id   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_name   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_id     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_lat          <dbl> 41.93, 41.89, 41.97, 41.94, 41.85, 41.79, 41.89, 41…
    ## $ start_lng          <dbl> -87.69, -87.64, -87.69, -87.65, -87.65, -87.72, -87…
    ## $ end_lat            <dbl> 41.94, 41.92, 41.97, 41.97, 41.84, 41.82, 41.89, 41…
    ## $ end_lng            <dbl> -87.72, -87.64, -87.66, -87.69, -87.66, -87.69, -87…
    ## $ user_type          <chr> "casual", "casual", "casual", "casual", "casual", "…
    ## $ ride_length        <dbl> 451, 842, 644, 903, 352, 781, 536, 1077, 683, 669, …

Previously, we had 5,883,043 values in the total data frame. The new
data frame created after we remove ride_length values less than 0
records 5,882,908 observations.

Next we would want to separate ride starting time to specific date of
month, weekday, month, and hour to find out patterns in the trip data on
ride level.

``` r
total2 <- total2 %>% 
  mutate(start_date = as.Date(total2$start_time),
         day = format(as.Date(total2$start_time), "%d"),
         month = format(as.Date(total2$start_time),"%m"),
         year = format(as.Date(total2$start_time),"%Y"),
         weekday = format(as.Date(total2$start_time),"%A"),
         hour = hour(total2$start_time)
         )
```

Then, we recheck our new data frame. 6 new columns are created to help
with further analyses which can be found in the next documentation of
the analytics phase.

``` r
glimpse(total2)
```

    ## Rows: 5,882,908
    ## Columns: 20
    ## $ ride_id            <chr> "550CF7EFEAE0C618", "DAD198F405F9C5F5", "E6F2BC47B6…
    ## $ ride_type          <chr> "electric_bike", "electric_bike", "electric_bike", …
    ## $ start_time         <chr> "2022-08-07 21:34:15", "2022-08-08 14:39:21", "2022…
    ## $ end_time           <chr> "2022-08-07 21:41:46", "2022-08-08 14:53:23", "2022…
    ## $ start_station_name <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_station_id   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_name   <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ end_station_id     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
    ## $ start_lat          <dbl> 41.93, 41.89, 41.97, 41.94, 41.85, 41.79, 41.89, 41…
    ## $ start_lng          <dbl> -87.69, -87.64, -87.69, -87.65, -87.65, -87.72, -87…
    ## $ end_lat            <dbl> 41.94, 41.92, 41.97, 41.97, 41.84, 41.82, 41.89, 41…
    ## $ end_lng            <dbl> -87.72, -87.64, -87.66, -87.69, -87.66, -87.69, -87…
    ## $ user_type          <chr> "casual", "casual", "casual", "casual", "casual", "…
    ## $ ride_length        <dbl> 451, 842, 644, 903, 352, 781, 536, 1077, 683, 669, …
    ## $ start_date         <date> 2022-08-07, 2022-08-08, 2022-08-08, 2022-08-08, 20…
    ## $ day                <chr> "07", "08", "08", "08", "07", "08", "08", "07", "07…
    ## $ month              <chr> "08", "08", "08", "08", "08", "08", "08", "08", "08…
    ## $ year               <chr> "2022", "2022", "2022", "2022", "2022", "2022", "20…
    ## $ weekday            <chr> "Sunday", "Monday", "Monday", "Monday", "Sunday", "…
    ## $ hour               <int> 21, 14, 15, 2, 20, 13, 14, 20, 21, 23, 20, 11, 22, …

## 4. Conduct descriptive analyses

Let’s do some descriptive analyses on the ride_length value.

``` r
summary(total2$ride_length)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     363     643    1185    1160 2442301

Due to the lack of information in the data, we would want to assume that
there are only local users (users who are originated or live in a long
duration in the area), with the consideration of most casual users are
visitors from other cities/states. In this case, we would want to look
at casual users who are less likely to use the service frequently than
the annual users.

There are several aspects we want to look at:

-   Compare descriptive values of ride_length in two groups of members:
    casual and annual member. This gives a sense about how differently
    member types use the ride.

-   Compare ride_length of casual users and annual users in terms of
    week day of the ride. This gives a sense about which weekday
    different member types use the ride and for what purpose.

-   Compare ride_length and number of rides of casual users and annual
    users in terms of month. This gives a sense about which month would
    be the most popular for different user types to use bike-share
    services.

-   Start time & total of rides during the day. This determines the
    differences in the bike-share service usage of the two user types
    and form the next steps for our solutions.

The first point is to compare the descriptive values of ride_length of
the different user types.

``` r
aggregate(total2$ride_length ~ total2$user_type,FUN = mean)
```

    ##   total2$user_type total2$ride_length
    ## 1           casual          1757.9776
    ## 2           member           771.3461

``` r
aggregate(total2$ride_length ~ total2$user_type,FUN = min)
```

    ##   total2$user_type total2$ride_length
    ## 1           casual                  0
    ## 2           member                  0

``` r
aggregate(total2$ride_length ~ total2$user_type,FUN = max)
```

    ##   total2$user_type total2$ride_length
    ## 1           casual            2442301
    ## 2           member              89998

``` r
aggregate(total2$ride_length ~ total2$user_type,FUN = median)
```

    ##   total2$user_type total2$ride_length
    ## 1           casual                835
    ## 2           member                538

Through the data, we can recognize that casual riders seems to have
larger time of ride compared to member riders do.

Next, we want to compare average the ride time of casual users and
annual users in terms of weekday of the ride to better understand
context.

``` r
aggregate(total2$ride_length ~ total2$user_type + total2$weekday, FUN = mean)
```

    ##    total2$user_type total2$weekday total2$ride_length
    ## 1            casual         Friday          1673.3191
    ## 2            member         Friday           755.6748
    ## 3            casual         Monday          1790.5689
    ## 4            member         Monday           747.0105
    ## 5            casual       Saturday          1922.4448
    ## 6            member       Saturday           858.1010
    ## 7            casual         Sunday          2051.6129
    ## 8            member         Sunday           865.7635
    ## 9            casual       Thursday          1561.6087
    ## 10           member       Thursday           742.4759
    ## 11           casual        Tuesday          1554.8980
    ## 12           member        Tuesday           730.6850
    ## 13           casual      Wednesday          1502.4496
    ## 14           member      Wednesday           731.7847

We noticed that the weekday is not in ondered. We would want to sort the
data by weekday to get a better view.

``` r
total2$weekday <- ordered(total2$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(total2$ride_length ~ total2$user_type + total2$weekday, FUN = mean)
```

    ##    total2$user_type total2$weekday total2$ride_length
    ## 1            casual         Sunday          2051.6129
    ## 2            member         Sunday           865.7635
    ## 3            casual         Monday          1790.5689
    ## 4            member         Monday           747.0105
    ## 5            casual        Tuesday          1554.8980
    ## 6            member        Tuesday           730.6850
    ## 7            casual      Wednesday          1502.4496
    ## 8            member      Wednesday           731.7847
    ## 9            casual       Thursday          1561.6087
    ## 10           member       Thursday           742.4759
    ## 11           casual         Friday          1673.3191
    ## 12           member         Friday           755.6748
    ## 13           casual       Saturday          1922.4448
    ## 14           member       Saturday           858.1010

This returns the same result throughout all weekdays.

Now let’s visualize this to get a better view throughout the whole year.

``` r
total2 %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_ride_time = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(mapping = aes(x = weekday, y = average_ride_time, fill = user_type)) +
  geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Visualize%20average%20ride%20time%20of%20user%20types%20by%20weekday-1.png)<!-- -->

And a visualization of total number of rides of users types by weekday.

``` r
total2 %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_ride_time = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(mapping = aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/r%20Visualize%20number%20of%20rides%20of%20user%20types%20by%20weekday-1.png)<!-- -->

Through this we got some insights:

-   During weekday, casual members have significant higher ride time yet
    lower number of riders compared to annual members do. This can be
    interpreted as more rides are made by annual members who save lots
    of time commuting to work during weekdays, while casual members tend
    to hang out more with rides throughout the week as they spend more
    time with the service.

-   During the weekend, both casual and annual members spend time to
    hang out with the service, which results in more ride time. In terms
    of number of rides, less annual members tend to use the service,
    while casual members tend to use more compared to the weekdays,
    moving along with the rise in ride time.

Up next, we would want to look at the monthly trends of ride time and
number of rides. In this case, we would look at a monthly view, ordered
from January to December.

*Noted that based on the imported data, trip data from January to August
is from 2022 and trip data from September to December is from 2021.*

Let’s create some visualization.

``` r
total2 %>%
  group_by(user_type, month) %>% 
  summarise(number_of_rides = n()
            ,average_ride_time = mean(ride_length)) %>% 
  arrange(user_type, month)  %>% 
  ggplot(mapping = aes(x = month, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Visualization%20of%20number%20of%20rides%20by%20month%20of%20user%20types-1.png)<!-- -->

``` r
total2 %>%
  group_by(user_type, month) %>% 
  summarise(number_of_rides = n()
            ,average_ride_time = mean(ride_length)) %>% 
  arrange(user_type, month)  %>% 
  ggplot(mapping = aes(x = month, y = average_ride_time, fill = user_type)) +
  geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Visualization%20of%20ride%20time%20by%20month%20of%20user%20types-1.png)<!-- -->

``` r
total2 %>%
  group_by(user_type, month) %>% 
  summarise(number_of_rides = n()
            ,total_ride_time = sum(ride_length)) %>% 
  arrange(user_type, month)  %>% 
  ggplot(mapping = aes(x = month, y = total_ride_time, fill = user_type)) +
  geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Visualization%20of%20total%20ride%20time%20by%20month%20of%20user%20types-1.png)<!-- -->

Insights:

-   In terms of number of rides and total ride time, the figures
    determine peak season is from May (05) to October (10). This could
    be explained as the increase in temperature which results in more
    rides made during summer season.

![Chicago’s monthly average
temperature](Chicago's%20monthly%20average%20temperature.png)

Lastly, we would want to look at the popular start time in terms of
total of rides during the day. Let’s visualize.

``` r
total2 %>% 
  group_by(user_type, hour) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(hour) %>% 
  ggplot(mapping = aes(x=hour,y=number_of_rides, fill=user_type)) + geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'user_type'. You can override using the
    ## `.groups` argument.

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Popular%20start%20time%20in%20terms%20of%20total%20of%20rides%20during%20the%20day-1.png)<!-- -->
Insights:

-   Casual users’ rides start time peaks from 12 PM to 7PM which
    represents the time they hang out (12 PM) and the time they leave
    for dinner or home (7 PM).

-   Annual users, on the other hand, mostly start at 7 AM - 8 AM (the
    time they leave for work) and 4PM - 6PM (the time they leave from
    work).

Furthermore, there are more things about casual users to look into the
most popular start and end stations for casual users. We want to check
if these end stations are entertainment areas or business areas to
customize our solutions to convert casual users to annual users.

``` r
total2 %>% 
   filter(user_type == "casual",start_station_name != "") %>% 
   group_by(start_station_name) %>% 
   summarize(number_of_rides = n()) %>% 
   filter(rank(desc(number_of_rides))<=10) %>% 
   arrange(-number_of_rides) %>% 
   mutate(start_station_name=factor(start_station_name,levels=start_station_name)) %>% 
  ggplot(mapping = aes(x=start_station_name, y=number_of_rides)) + geom_col(position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust=1,vjust = 1))
```

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Most%20popular%20start%20station%20for%20casual%20users-1.png)<!-- -->

``` r
total2 %>% 
   filter(user_type == "casual",end_station_name != "") %>% 
   group_by(end_station_name) %>% 
   summarize(number_of_rides = n()) %>% 
   filter(rank(desc(number_of_rides))<=10) %>% 
   arrange(-number_of_rides) %>% 
   mutate(end_station_name=factor(end_station_name,levels=end_station_name)) %>% 
  ggplot(mapping = aes(x=end_station_name, y=number_of_rides)) + geom_col(position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust=1,vjust = 1))
```

![](Challenge1_DataWrangling_Visualization_files/figure-gfm/Most%20popular%20end%20station%20for%20casual%20users-1.png)<!-- -->

The visuals show that most popular start and end station are located
near popular visitors’ site in Chicago, which includes museums, parks,
harbors, aquariums, and lots of shopping places around these areas. It
means most casual users will come to these entertainment sites for
leisure.

## 5. Export clean data to one completed csv file

The last step is to export the final cleaned data to a csv file using
write.csv() function and save as **new_total.csv**.

``` r
write.csv(total2,"new_total.csv")
```
