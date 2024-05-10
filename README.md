# Rainfall analysis

Rainfall analysis inspired by https://github.com/engjen/rain_pdx


---


Load packages


```r
library(magrittr)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ tidyr::extract()   masks magrittr::extract()
## ✖ dplyr::filter()    masks stats::filter()
## ✖ dplyr::lag()       masks stats::lag()
## ✖ purrr::set_names() masks magrittr::set_names()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(ggthemes)
library(hrbrthemes)
```

Read rain gauge data from City of Portland Bureau of Environmental Services by way of USGS.

> Vernon School Rain Gage - 2044 NE. Killingsworth St.  
>  
> PROVISIONAL, UNCORRECTED RAW DATA FROM THE CITY OF PORTLAND HYDRA NETWORK.  
> Data are the number of tips of the rain gage bucket.  
> Each tip is 0.01 inches of rainfall.  
>  [`-`, missing data]  
> Dates and times are PACIFIC STANDARD TIME.  

Assume most recent day is incomplete and exclude.
Convert *tips* to inches.


```r
url <- "https://or.water.usgs.gov/non-usgs/bes/vernon.rain"
f <- tempfile()
download.file(url, f)
colNames <- read_lines(f, skip = 9, n_max = 1) %>% str_trim() %>% str_split("\\s+") %>% unlist()
L <- read_lines(f, skip = 11) %>% str_split("\\s+")
date <- sapply(L, function(x) x[1]) %>% as.Date(format = "%d-%b-%Y")
total <- sapply(L, function(x) x[2]) %>% as.numeric()
```

```
## Warning in sapply(L, function(x) x[2]) %>% as.numeric(): NAs introduced by
## coercion
```

```r
hourly <-
  sapply(L, function(x) x[3:26], simplify = FALSE) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  mutate_if(is.character, as.numeric)
```

```
## Warning: There were 24 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `X1 = .Primitive("as.double")(X1)`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 23 remaining warnings.
```

```r
names(hourly) <- paste0("x", colNames[3:26])
df <-
  tibble(date, hourly) %>%
  filter(date < max(date)) %>%
  pivot_longer(starts_with("x")) %>%
  mutate(hour = gsub("x", "", name) %>% as.integer()) %>%
  select(-name) %>%
  mutate(datetime = sprintf("%02d:00", hour) %>% paste(date, .) %>% as.POSIXlt(format = "%Y-%m-%d %H:%M")) %>%
  mutate(inches = value * 0.01) %>%
  select(-value)
head(df) %>% knitr::kable()
```



|date       | hour|datetime            | inches|
|:----------|----:|:-------------------|------:|
|2024-05-09 |    0|2024-05-09 00:00:00 |      0|
|2024-05-09 |    1|2024-05-09 01:00:00 |      0|
|2024-05-09 |    2|2024-05-09 02:00:00 |      0|
|2024-05-09 |    3|2024-05-09 03:00:00 |      0|
|2024-05-09 |    4|2024-05-09 04:00:00 |      0|
|2024-05-09 |    5|2024-05-09 05:00:00 |      0|

## Monthly comparison

Plot the most recent $k$ years.
Assume current month is incomplete and exclude.


```r
k <- 4
currentYear <- year(max(date))
minYear <- currentYear - k
previousMonth <- month(today() %>% floor_date(unit = "month") - 1)
spin <- -((previousMonth - 1/2) / 12) * (2 * pi)
G <-
  df %>%
  filter(max(date) - k * 365.25 <= date) %>%
  filter(!(year(date) == year(today()) & month(date) == month(today()))) %>%
  mutate(year = year(date) %>% factor(),
         isCurrentYear = (year(date) == currentYear),
         monthday = sprintf("%d-%d-%d", currentYear, month(date), day(date)) %>% as.Date()) %>%
  mutate(unit = floor_date(monthday, unit = "month")) %>%
  group_by(year, isCurrentYear, unit) %>%
  summarize(inches = sum(inches, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = unit, y = inches)) +
    labs(title = "Monthly rainfall (inches)") +
    geom_col(aes(fill = year, alpha = isCurrentYear), position = "dodge", show.legend = c(fill = TRUE, alpha = FALSE)) +
    scale_fill_viridis_d("Year", direction = -1) +
    scale_x_date("", date_breaks = "month", date_labels = "%b", expand = expansion(add = 120 * pi / 180)) +
    scale_y_continuous("", transform = "sqrt") +
    scale_alpha_manual(values = c(2/3, 1)) +
    coord_polar(start = spin) +
    theme_ipsum_ps() +
    theme(panel.grid.minor.x = element_blank(),
          plot.title.position = "plot")
```

```
## `summarise()` has grouped output by 'year', 'isCurrentYear'. You can override
## using the `.groups` argument.
```

```r
ggsave("rainfall.png", dpi = 300, units = "in", height = 4, width = 6)
```

![rainfall.png](rainfall.png)

Create `ts` object


```r
min <- min(df$date, na.rm = TRUE)
max <- max(df$date, na.rm = TRUE)
timeSeries <-
  df %>%
  group_by(date) %>%
  summarize(inches = sum(inches)) %>%
  ungroup() %>%
  ts(start = min, end = max)
```


---


Session information


```r
sessionInfo()
```

```
## R version 4.4.0 (2024-04-24 ucrt)
## Platform: x86_64-w64-mingw32/x64
## Running under: Windows 10 x64 (build 19044)
## 
## Matrix products: default
## 
## 
## locale:
## [1] LC_COLLATE=English_United States.utf8 
## [2] LC_CTYPE=English_United States.utf8   
## [3] LC_MONETARY=English_United States.utf8
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.utf8    
## 
## time zone: America/Los_Angeles
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] hrbrthemes_0.8.7 ggthemes_5.1.0   lubridate_1.9.3  forcats_1.0.0   
##  [5] stringr_1.5.1    dplyr_1.1.4      purrr_1.0.2      readr_2.1.5     
##  [9] tidyr_1.3.1      tibble_3.2.1     ggplot2_3.5.1    tidyverse_2.0.0 
## [13] magrittr_2.0.3  
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.5            xfun_0.43               tzdb_0.4.0             
##  [4] vctrs_0.6.5             tools_4.4.0             generics_0.1.3         
##  [7] curl_5.2.1              parallel_4.4.0          fansi_1.0.6            
## [10] pkgconfig_2.0.3         lifecycle_1.0.4         farver_2.1.1           
## [13] compiler_4.4.0          textshaping_0.3.7       munsell_0.5.1          
## [16] httpuv_1.6.15           fontquiver_0.2.1        fontLiberation_0.1.0   
## [19] htmltools_0.5.8.1       Rttf2pt1_1.3.12         pillar_1.9.0           
## [22] later_1.3.2             crayon_1.5.2            extrafontdb_1.0        
## [25] gfonts_0.2.0            mime_0.12               fontBitstreamVera_0.1.1
## [28] tidyselect_1.2.1        digest_0.6.35           stringi_1.8.3          
## [31] labeling_0.4.3          extrafont_0.19          fastmap_1.1.1          
## [34] grid_4.4.0              colorspace_2.1-0        cli_3.6.2              
## [37] crul_1.4.2              utf8_1.2.4              withr_3.0.0            
## [40] gdtools_0.3.7           scales_1.3.0            promises_1.3.0         
## [43] bit64_4.0.5             timechange_0.3.0        bit_4.0.5              
## [46] ragg_1.3.0              hms_1.1.3               shiny_1.8.1.1          
## [49] evaluate_0.23           knitr_1.46              viridisLite_0.4.2      
## [52] rlang_1.1.3             Rcpp_1.0.12             xtable_1.8-4           
## [55] glue_1.7.0              httpcode_0.3.0          vroom_1.6.5            
## [58] jsonlite_1.8.8          R6_2.5.1                systemfonts_1.0.6
```
