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
  filter(!(Plot.ID == "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
nursery_data <- nursery_data_RAW[, 1:7] %>%
  filter(!(Plot.ID == "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
october_data <- october_data_RAW[, 1:6] %>% 
  filter(!(Plot.ID == "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
TEV_data <- TEV_data_RAW[, 1:4] %>%
  filter(!(Plot.ID == "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
willow_data <- willow_data_RAW[, 1:7] %>%
  filter(!(Cluster.. == "")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
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

Getting Willow Stake Elevations and adding them to the willow data frame

``` r
Willow_Elevation <- read.csv("Raw_Data\\Willow_Elevations.csv")
Willow_Elevation$Name <- sub(".*Willow_stake", "", Willow_Elevation$Name)
Willow_Elevation$Name <- as.numeric(Willow_Elevation$Name)
Willow_Elevation <- Willow_Elevation %>% 
  rename("Cluster.." = Name)
willow_data <- left_join(willow_data, Willow_Elevation[, c("Cluster..",
                        "Elevation")], by = "Cluster..")
```

Living Tissue vs. Cluster#

``` r
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
        legend.position = c(0.89, 0.75)) +
  geom_point(aes(y = Elevation * 100), color = "red") +
  geom_line(aes(y = Elevation *100), color = "red", se = FALSE) +
  scale_y_continuous(
    name = "Shoot Growth (cm)",
    sec.axis = sec_axis(~ . / 100, name = "Elevation (m)"))
```

    ## Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    ## 3.5.0.
    ## ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning in geom_line(aes(y = Elevation * 100), color = "red", se = FALSE):
    ## Ignoring unknown parameters: `se`

``` r
willow_growth_cluster
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 48 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#ggsave("Willow stake growth vs cluster #.jpg", plot = willow_growth_cluster, 
       #width = 10, height = 6.5)
```

Mortality plots

``` r
willow_ave <- willow_data %>%
  group_by(Date) %>%
  summarise(ave_mort = mean(Alive.Tissue, na.rm = TRUE),
            ave_bud = mean(Growth.Present, na.rm = TRUE))
          

mort_distance <- willow_ave %>%
  ggplot(aes(x = Date, y = ave_mort/3*100)) +
  geom_point() +
  labs(y = "Total Survivorship (%)",
       title = "Stake Survivorship over Time") +
  geom_point(aes(y = ave_bud/3*100), color = "red") +
  ylim(0, 100)
mort_distance
```

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

TEV site analysis

``` r
TEV_data$Difference <- TEV_data$Inside.Average.Plant.Height..cm. -     TEV_data$Outside.Average.Plant.Height..cm.

TEV_ave <- TEV_data %>%
  group_by(Date) %>%
  summarise(ave_diff = mean(Difference, na.rm = TRUE),
            sd_diff = sd(Difference, na.rm = TRUE))
```

TEV site plot

``` r
TEV_plot <- TEV_ave %>%
  ggplot(aes(x = Date, y = ave_diff)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  ylim(-10,10) +
  geom_errorbar(aes(ymin = ave_diff - sd_diff, ymax = ave_diff + sd_diff),
                width = 0.2) +
  labs(y = "Difference in height (cm)",
       title = "Spikerush (Eleocharis palustris) Height Difference over Time ") +
  theme_classic()
TEV_plot
```

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
