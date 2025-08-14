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
diameter_data_RAW <- read.csv("Raw_Data\\Diameter_Data_RAW.csv")
bird_RAW <- read.csv("Raw_Data\\Bird_data_RAW.csv")
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

diameter_data <- diameter_data_RAW %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(growth_cm = as.numeric(growth_cm))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `growth_cm = as.numeric(growth_cm)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
bird_data <- bird_RAW %>%
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

diameter plots

``` r
growth_diameter_plot <- diameter_data %>%
  ggplot(aes(x = diameter_cm, y = growth_cm)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)
growth_diameter_plot
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 25 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
growth_cluster_plot <- diameter_data %>%
  ggplot(aes(x = cluster_., y = growth_cm, size = diameter_cm)) +
  geom_point()
growth_cluster_plot
```

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

diameter fun?

``` r
diameter_number_data <- diameter_data %>%
  filter(!is.na(growth_cm)) %>%
  group_by(cluster_.) %>%
  mutate( max_value = max(growth_cm, na.rm = TRUE),
          normalized = (growth_cm / max_value) * 100,
          source = "Measured") %>%
  mutate(normalized = ifelse(is.nan(normalized), 0, normalized)) %>%
  ungroup()

diameter_na_data <- diameter_data %>%
  filter(is.na(growth_cm)) %>%
  mutate(normalized = 0, source = "Missing")

data_combined <- bind_rows(diameter_number_data, diameter_na_data)

normalized_plot <- data_combined %>%
  ggplot(aes(x = cluster_., y = normalized, size = diameter_cm, color = source)) +
  geom_point() +
  scale_color_manual(values = c("Measured" = "black", "Missing" = "red"))
normalized_plot
```

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

TEV site analysis

``` r
TEV_data$Difference <- TEV_data$Inside.Average.Plant.Height..cm. -     TEV_data$Outside.Average.Plant.Height..cm.

TEV_ave <- TEV_data %>%
  group_by(Date) %>%
  summarise(ave_diff = mean(Difference, na.rm = TRUE),
            sd_diff = sd(Difference, na.rm = TRUE))

# comparison analysis

TEV_sub <- TEV_data %>%
  filter(Date == as.Date("2025-07-14"))

#T-Test paired
t.test(TEV_sub$Inside.Average.Plant.Height..cm., 
       TEV_sub$Outside.Average.Plant.Height..cm., paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  TEV_sub$Inside.Average.Plant.Height..cm. and TEV_sub$Outside.Average.Plant.Height..cm.
    ## t = 2.6186, df = 2, p-value = 0.1201
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.572411 10.572411
    ## sample estimates:
    ## mean difference 
    ##               4

``` r
#Wilcoxon test
wilcox.test(TEV_sub$Inside.Average.Plant.Height..cm., 
       TEV_sub$Outside.Average.Plant.Height..cm., paired = TRUE)
```

    ## 
    ##  Wilcoxon signed rank exact test
    ## 
    ## data:  TEV_sub$Inside.Average.Plant.Height..cm. and TEV_sub$Outside.Average.Plant.Height..cm.
    ## V = 6, p-value = 0.25
    ## alternative hypothesis: true location shift is not equal to 0

TEV site plot

``` r
TEV_plot <- TEV_ave %>%
  ggplot(aes(x = Date, y = ave_diff)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  ylim(-10,10) +
  geom_errorbar(aes(ymin = ave_diff - sd_diff, ymax = ave_diff + sd_diff),
                width = 0.2) +
  labs(y = "Spikerush Height Difference (cm)",
       title = expression("Common Spikerush (" 
                          * italic("Eleocharis palustris") *
                          ") Growth Difference over Time ")) +
  annotate("text", x = as.Date("2025-06-04"), y = 0.75, 
           label = "Increased growth inside exclosure zone ⮝", size = 3, color ="blue") +
  annotate("text", x = as.Date("2025-06-04"), y = -0.5, 
           label = "Increased growth outside exclosure zone ⮟", size = 3, color = "red") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
TEV_plot
```

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#ggsave("TEV_sites_growth_plot.jpg", plot = TEV_plot, width = 7, height = 4)
```

Nursery Plug data manipulation

``` r
nursery_ave <- nursery_data %>%
  group_by(Plot.ID, Exclosure, Date) %>%
  summarise(mean_height = mean(Average.Plant.Height..cm.))
```

    ## `summarise()` has grouped output by 'Plot.ID', 'Exclosure'. You can override
    ## using the `.groups` argument.

``` r
nursery_ave <- nursery_ave %>%
  group_by(Date, Plot.ID) %>%
  summarise(
    yes_val = mean_height[Exclosure == "Yes"],
    no_val = mean_height[Exclosure == "No"],
    diff = yes_val - no_val
  )
```

    ## `summarise()` has grouped output by 'Date'. You can override using the
    ## `.groups` argument.

Nursery Plug data visualization

``` r
totalnum_high_plot <- nursery_data %>%
  filter(Density == "High") %>%
  ggplot(aes(x = Date, y = X..of.Plugs/25*100, color = Exclosure)) +
  geom_point(position = position_jitter(width = 1, height = 0))+
  geom_smooth(se = FALSE) +
  ylim(0, 100) +
  labs( y = "%plugs remaining (High Density Plots)",
        title = "High Density Plug Survivorship Over Time")
totalnum_high_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 4.9396e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 4.9396e-17

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
totalnum_low_plot <- nursery_data %>%
  filter(Density == "Low") %>%
  filter(Plot.ID != "TE5") %>%
  ggplot(aes(x = Date, y = X..of.Plugs/9*100, color = Exclosure)) +
  geom_point(position = position_jitter(width = 1, height = 0))+
  geom_smooth(se = FALSE) +
  ylim(0, 100) +
  labs( y = "%plugs remaining (Low Density Plots)",
        title = "Low Density Plug Survivorship Over Time")
totalnum_low_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 4.9396e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 4.9396e-17

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
height_ave_plot <- nursery_ave %>%
  ggplot(aes(x = Date, y = yes_val)) +
  geom_point(aes(color = "yes_val")) +
  geom_point(aes(y = no_val, color = "no_val")) +
  ylim(0,25) +
  geom_smooth(aes(y = yes_val), color = "red", se = FALSE) +
  geom_smooth(aes(y = no_val), color = "blue", se = FALSE) + 
  scale_color_manual(values = c("yes_val" = "red", "no_val" = "blue"),
                     labels = c("yes_val" = "Exclosed", "no_val" = "Unprotected")) +
  labs(color = "Treatment", y = "Average Plant Height (cm)",
       title = "Nursery Plant Growth over Time ") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_classic()
height_ave_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

Donor Plug data manipulation

``` r
donor_ave <- donor_data %>%
  mutate(Max.CARELYN.Height..cm. = ifelse
         (Max.CARELYN.Height..cm. == 0, NA, Max.CARELYN.Height..cm.)) %>%
  mutate(Max.SCHOTAB.Height..cm. = ifelse
         (Max.SCHOTAB.Height..cm. == 0, NA, Max.SCHOTAB.Height..cm.)) %>%
  mutate(Max.ELEOPAL.Height..cm. = ifelse
         (Max.ELEOPAL.Height..cm. == 0, NA, Max.ELEOPAL.Height..cm.))

donor_ave <- donor_data %>%
  group_by(Date, Exclosure) %>%
  summarise(SCHOTAB_ave = mean(Max.SCHOTAB.Height..cm., na.rm = TRUE),
            ELEOPAL_ave = mean(Max.ELEOPAL.Height..cm., na.rm = TRUE),
            CARELYN_ave = mean(Max.CARELYN.Height..cm., na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'Date'. You can override using the
    ## `.groups` argument.

``` r
donor_ave <- donor_ave %>%
  ungroup() %>% 
  mutate(SCHOTAB_mort = c(5,5,4,5,4,4,4,4,0,0)/5*100,
         ELEOPAL_mort = c(10,10,8,10,6,10,6,10,6,8)/10*100,
         CARELYN_mort = c(5,5,2,5,3,4,3,5,2,5)/5*100)
```

Donor Plug data visualization

``` r
SCHOTAB_height_plot <- donor_data %>%
  ggplot(aes(x = Date, y = Max.SCHOTAB.Height..cm.)) +
  geom_point(aes(color = Exclosure)) +
  geom_smooth(aes(color = Exclosure), se = FALSE)
SCHOTAB_height_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ELEOPAL_height_plot <- donor_data %>%
  ggplot(aes(x = Date, y = Max.ELEOPAL.Height..cm.)) +
  geom_point(aes(color = Exclosure)) +
  geom_smooth(aes(color = Exclosure), se = FALSE)
ELEOPAL_height_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
CARELYN_height_plot <- donor_data %>%
  ggplot(aes(x = Date, y = Max.CARELYN.Height..cm.)) +
  geom_point(aes(color = Exclosure)) +
  geom_smooth(aes(color = Exclosure), se = FALSE)
CARELYN_height_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
total_mort_plot <- donor_data %>%
  ggplot(aes(x = Date, y = X..of.Plugs/4*100, color = Exclosure)) +
  geom_point() +
  geom_smooth(aes(color = Exclosure), se = FALSE) +
  geom_jitter(width = 2, height = 2) +
  ylim(0,100)
total_mort_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20257

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 26

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 5.5227e-17

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
CARELYN_mort <- donor_ave %>%
  ggplot(aes(x = Date, y = CARELYN_mort)) +
  geom_point(aes(color = Exclosure))+
  geom_smooth(aes(color = Exclosure), se = FALSE)+
  ylim(0,110)
CARELYN_mort
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->

``` r
ELEOPAL_mort <- donor_ave %>%
  ggplot(aes(x = Date, y = ELEOPAL_mort)) +
  geom_point(aes(color = Exclosure))+
  geom_smooth(aes(color = Exclosure), se = FALSE)+
  ylim(0,100)
ELEOPAL_mort
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

    ## Warning: Removed 9 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->

``` r
SCHOTAB_mort <- donor_ave %>%
  ggplot(aes(x = Date, y = SCHOTAB_mort)) +
  geom_point(aes(color = Exclosure))+
  geom_smooth(aes(color = Exclosure), se = FALSE)+
  ylim(0,100)
SCHOTAB_mort
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20231

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 39.4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1714

    ## Warning: Removed 27 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->

Bird Data Management

``` r
bird_data <- bird_data %>%
  group_by(Date, Species) %>%
  summarise(total = n()) %>%
  filter (Species != "No Birds")
```

    ## `summarise()` has grouped output by 'Date'. You can override using the
    ## `.groups` argument.

goose plot

``` r
# Step 1: Create a dataset of only Canada Goose data
goose_data <- bird_data %>%
  filter(Species == "Canada Goose")

# Step 2: Fit the smoother manually (uses the same loess method as geom_smooth())
loess_model <- loess(total ~ as.numeric(Date), data = goose_data)
```

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20195

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 62.44

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 699.07

``` r
# Step 3: Create smooth prediction data
smooth_df <- data.frame(Date = seq(min(goose_data$Date), max(goose_data$Date), length.out = 200)) %>%
  mutate(
    total = predict(loess_model, newdata = data.frame(Date = as.numeric(Date)))
  )

# Step 4: Plot with shaded area
goose_plot <- ggplot(goose_data, aes(x = Date, y = total)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "blue") +  # Trendline
  geom_ribbon(data = smooth_df, aes(x = Date, ymin = 0, ymax = total),
              inherit.aes = FALSE, fill = "lightblue", alpha = 0.4) +
  ylim(0, 100) +
  theme_minimal()

goose_plot
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 20195

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 62.44

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 699.07

![](Tilbury_Data_Analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
