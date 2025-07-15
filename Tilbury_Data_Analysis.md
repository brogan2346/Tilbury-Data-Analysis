Tilbury_Data_Analysis
================
Brogan Neufeld
2025-07-15

Installing Relavent Packages and Importing Relavent Data

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
donor_data_RAW <- read.csv("Raw_Data\\Donor_Core_Data_RAW.csv")
nursery_data_RAW <- read.csv("Raw_Data\\Nursery_Data_RAW.csv")
october_data_RAW <- read.csv("Raw_Data\\October_Data_RAW.csv")
TEV_data_RAW <- read.csv("Raw_Data\\TEV_Data_RAW.csv")
willow_data_RAW <- read.csv("Raw_Data\\Willow_Data_RAW.csv")
```

Cleaning up data Only keeping variables / columns of intrest

``` r
donor_data <- donor_data_RAW[, 1:10] %>%
  filter(!(Plot.ID == ""))
nursery_data <- nursery_data_RAW[, 1:7] %>%
  filter(!(Plot.ID == ""))
october_data <- october_data_RAW[, 1:6] %>% 
  filter(!(Plot.ID == ""))
TEV_data <- TEV_data_RAW[, 1:4] %>%
  filter(!(Plot.ID == ""))
willow_data <- willow_data_RAW[, 1:7] %>%
  filter(!(Cluster.. == ""))
```

Creating a Palette of colours to be used for figures that are coloured
according to date

``` r
#Gets unique date values, sorted
date_levels <- sort(unique(willow_data$Date))

# generates gradient from red to yellow with the same # of colours as dates
date_color_palette <- colorRampPalette(c("darkblue", "lightblue"))(length(date_levels))

#name the colors so ggplot can map them to dates
names(date_color_palette) <- date_levels
```

willow data plots

``` r
#Living Tissue vs. Cluster#

willow_growth_cluster <- willow_data %>%
  ggplot(aes(x=Cluster.., y=Shoot.Growth.Average.Length..cm.)) +
  geom_point(aes(color = factor(Date))) +
  geom_smooth(aes(group = Date, color = factor(Date)), se = FALSE) +
  scale_color_manual(values = date_color_palette,
                     breaks = setdiff(names(date_color_palette),
                                  as.character(c("4/17/2025", "4/4/2025")))) +
  labs(color = "Date", x = "Distance From Shore (m)", 
       y = "Max Shoot Growth (cm)",
       title = "Willow Stake Growth vs. Distance from Shore") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.89, 0.75))
```

    ## Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    ## 3.5.0.
    ## ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
willow_growth_cluster
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 48 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave("Willow stake growth vs cluster #.jpg", plot = willow_growth_cluster, 
       width = 10, height = 6.5)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 48 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).
    ## Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_point()`).
