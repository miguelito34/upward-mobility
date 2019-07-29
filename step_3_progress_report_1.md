Progress report 1
================
Michael
2019-01-30

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
load("C:/Users/Michael Spencer/Desktop/ENGR 150 DATA/c01/c01 workspace.RData")
```

## Summary of Progress

In this report, I have explored simple outcomes such as adult household
income and incarceration rates at the national level by both parental
income and race. Additionally, I have explored how education levels and
completion rates vary by parental income and race. After exploring
simple national trends to get a sense of them, I began to dive deeper,
looking into trends at the commuting zone level and eventually at the
city level. Though the breadth of trends I looked at wasn’t huge, it did
give me a sense of some key insights which I’ll explore further into
progress report 2. In particular, when exploring simple outcomes such as
adulthood income rate and incarceration rates within specific cities
such as Chicago or Los Angeles, it was striking how much variation there
was for census tracts with the same median parental income. This is a
finding I hope to explore more as we go on.

## Key Findings and Illustrations

#### Exploring National Outcomes

Though the findings below are by no means new, they’re neverthless
important for understanding our data and its various dimensions. The
plots below demonstrate how incarceration rates and adult household
income varies for different races and those who come from higher income
backgrounds. Many of the trends were to be expected - wealthier children
grew into wealthier adults and went to jail less frequently. An aspect
to keep in mind while exploring this data further will be how race
affects outcomes within specific locations. As we saw here, race played
a huge role in outcomes as white people always had the most preferable
outcomes in each scenario, with the occasional exception of
Asian-Americans. Tailing behing them were racial minorities such as
Blacks and Hispanics.

``` r
national_percentile_outcomes %>% 
  ggplot(aes(x = par_pctile)) +
  geom_point(aes(y = kfr_white_pooled, color = "white_ex"), shape = 15, size = 1.5) +
  geom_line(aes(y = s_kfr_white_pooled, color = "white_ex"), size = 1) +
  geom_point(aes(y = kfr_hisp_pooled, color = "hisp_ex"), shape = 16) +
  geom_line(aes(y = s_kfr_hisp_pooled, color = "hisp_ex"), size = 1) +
  geom_point(aes(y = kfr_black_pooled, color = "blk_ex"), shape = 17) +
  geom_line(aes(y = s_kfr_black_pooled, color = "blk_ex"), size = 1) + 
  geom_point(aes(y = kfr_asian_pooled, color = "as_ex"), shape = 17) +
  geom_line(aes(y = s_kfr_asian_pooled, color = "as_ex"), size = 1) +
  scale_y_continuous(
    breaks = seq(.3, .7, by = .1),
    minor_breaks = NULL,
    labels = seq(30, 70, by = 10)
  ) +
  scale_color_manual(
    name = "Race",
    values = c(white_ex = "darkblue", hisp_ex = "orange", blk_ex = "maroon", as_ex = "darkgreen"),
    labels = c(blk_ex = "Black", as_ex = "Asian", white_ex = "White", hisp_ex = "Hispanic")
  ) +
  labs(
    title = "Household Income Rank by Race and Ethnicity",
    subtitle = "Across all races, children growing up in higher income households tend to have\nhigher income households as adults.",
    x = "Parent Household Income Rank",
    y = "Children's Mean Household income Rank (Ages 31-37)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  ) +
  guides(color = guide_legend(reverse = TRUE))
```

![](step_3_progress_report_1_files/figure-gfm/adulthood%20household%20income%20rank%20vs%20parental%20household%20income%20rank%20by%20race-1.png)<!-- -->

``` r
national_percentile_outcomes %>% 
  ggplot(aes(x = par_pctile)) +
  geom_point(aes(y = jail_white_male, color = "white_ex"), shape = 15, size = 1.5) +
  geom_line(aes(y = s_jail_white_male, color = "white_ex"), size = 1) +
  geom_point(aes(y = jail_hisp_male, color = "hisp_ex"), shape = 16) +
  geom_line(aes(y = s_jail_hisp_male, color = "hisp_ex"), size = 1) +
  geom_point(aes(y = jail_black_male, color = "blk_ex"), shape = 17) +
  geom_line(aes(y = s_jail_black_male, color = "blk_ex"), size = 1) +
  scale_y_continuous(
    breaks = seq(0, .2, by = .05),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    name = "Race",
    values = c(white_ex = "darkblue", hisp_ex = "orange", blk_ex = "maroon"),
    labels = c("Black", "Hispanic", "White")
  ) +
  labs(
    title = "Male Incarceration Rates by Race and Ethnicity",
    subtitle = "Across all races, children growing up in higher income households tend to have\nlower incarceration rates as adults.",
    x = "Parent Household Income Rank",
    y = "% of Men Incarcerated on April 1, 2010 (Ages 27-32)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](step_3_progress_report_1_files/figure-gfm/adulthood%20incarceration%20rates%20vs%20parental%20household%20income%20rank%20by%20race-1.png)<!-- -->

``` r
national_percentile_outcomes %>% 
  ggplot(aes(x = par_pctile)) +
  geom_point(aes(y = hs_white_pooled, color = "white_ex"), shape = 15, size = 1.5) +
  geom_line(aes(y = s_hs_white_pooled, color = "white_ex"), size = 1) +
  geom_point(aes(y = hs_hisp_pooled, color = "hisp_ex"), shape = 16) +
  geom_line(aes(y = s_hs_hisp_pooled, color = "hisp_ex"), size = 1) +
  geom_point(aes(y = hs_black_pooled, color = "blk_ex"), shape = 17) +
  geom_smooth(aes(y = hs_black_pooled, color = "blk_ex"), se = FALSE, size = 1) +
  geom_point(aes(y = hs_asian_pooled, color = "as_ex"), shape = 17) +
  geom_smooth(aes(y = hs_asian_pooled, color = "as_ex"), se = FALSE, size = 1) +
  scale_y_continuous(
    breaks = seq(.5, 1, by = .1),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    name = "Race",
    values = c(white_ex = "darkblue", hisp_ex = "orange", blk_ex = "maroon", as_ex = "darkgreen"),
    labels = c(blk_ex = "Black", as_ex = "Asian", white_ex = "White", hisp_ex = "Hispanic")
  ) +
  labs(
    title = "National High School Completion Rates by Race and Ethnicity",
    subtitle = "Across all races, children growing up in higher income households tend to have\nhigher high school completion rates.",
    x = "Parent Household Income Rank",
    y = "Children's High School Completion Rates"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](step_3_progress_report_1_files/figure-gfm/HS%20completion%20rates%20vs%20parental%20household%20income-1.png)<!-- -->

``` r
national_percentile_outcomes %>% 
  ggplot(aes(x = par_pctile)) +
  geom_point(aes(y = coll_white_pooled, color = "white_ex"), shape = 15, size = 1.5) +
  geom_line(aes(y = s_coll_white_pooled, color = "white_ex"), size = 1) +
  geom_point(aes(y = coll_hisp_pooled, color = "hisp_ex"), shape = 16) +
  geom_line(aes(y = s_coll_hisp_pooled, color = "hisp_ex"), size = 1) +
  geom_point(aes(y = coll_black_pooled, color = "blk_ex"), shape = 17) +
  geom_line(aes(y = s_coll_black_pooled, color = "blk_ex"), size = 1) +
  scale_y_continuous(
    breaks = seq(0, 1, by = .1),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    name = "Race",
    values = c(white_ex = "darkblue", hisp_ex = "orange", blk_ex = "maroon", as_ex = "darkgreen"),
    labels = c(blk_ex = "Black", as_ex = "Asian", white_ex = "White", hisp_ex = "Hispanic")
  ) +
  labs(
    title = "National College Completion Rates by Race and Ethnicity",
    subtitle = "Across all races, children growing up in higher income households tend to have\nhigher college completion rates.",
    x = "Parent Household Income Rank",
    y = "Children's College Completion Rates"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

![](step_3_progress_report_1_files/figure-gfm/College%20completion%20rates%20vs%20parental%20household%20income-1.png)<!-- -->

## Exploring Commuting Zone Trends

The following plot demonstrates a key notion that outcomes can vary
greatly from place to place. In thinking about the one of the biggest
findings of the paper, that where you grow up matters, this plot is
important. It helps us to identify potential cities and anomalies to
explore deeper so that we may gaib more insight into this data.

``` r
cz_join_simple <-
  cz_cov %>% 
  left_join(cz_outcomes_simple, by = c("cz", "czname"))
```

``` r
cz_join_simple %>% 
  arrange(desc(popdensity2010)) %>% 
  top_n(30) %>% 
  ggplot(aes(job_growth_1990_2010, kfr_pooled_pooled_p25)) +
  geom_hline(
    aes(yintercept = median(kfr_pooled_pooled_p25)), 
    linetype = 2, 
    color = "red", 
    alpha = .7
  ) +
  geom_vline(
    aes(xintercept = median(job_growth_1990_2010)), 
    linetype = 2, 
    color = "red", 
    alpha = .7
  ) +
  geom_point(size = 2, color = "darkblue") +
  geom_text(
    aes(label = czname), 
    size = 2.5, 
    nudge_x = 5.5,
    color = "darkblue"
  ) +
  coord_cartesian(xlim = c(0, 80), ylim = c(.375, .46)) +
  scale_y_continuous(
    breaks = seq(.38, .46, by = .02),
    minor_breaks = NULL,
    labels = c("38", "40", "42", "44", "46")
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, by = 10),
    minor_breaks = NULL,
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")
  ) +
  labs(
    title = "Upward Mobility vs. Job Growth in the Largest 50 Commuting Zones\nby Population Density",
    x = "Job Growth Rate from 1990 to 2010 (%)",
    y = "Children's Mean Household Income Rank\nGiven Parents at the 25th Income Percentile"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

    ## Selecting by hisp_female_count

![](step_3_progress_report_1_files/figure-gfm/Upward%20mobility%20vs%20job%20growth%20in%20the%20largest%2050%20CZs-1.png)<!-- -->

## Exploring Individual Cities

At face value, the following plots show what one would expect, that
outcomes improve as housing costs (and presumably the area)
increases/improves. The examples below use Chicago and Pheonix as
starters, but several other plots were created on large metropolitan
areas, all showing similar trends. These plots are interesting in that
they demonstrate two things:

  - For a given housing cost, a wide range of outcomes can be had, from
    great to horrible.

  - In some cases, good outcomes come at a high cost.

Both of these points illustrate a problem that people face in accessing
mobility and opportunity which is a key aspect of this paper, and
something we will explore further.

``` r
major_cities_fips <- tibble(
  city = c("Chicago", "Los Angeles", "San Francisco", "Oakland", "Phoenix", "Louisville"),
  state = c(17, 06, 06, 06, 04, 21),
  county = c(031, 037, 075, 001, 013, 111),
  FIPS = c(17031, 06037, 06075, 06001, 04013, 21111)
)

major_cities_fips
```

    ## # A tibble: 6 x 4
    ##   city          state county  FIPS
    ##   <chr>         <dbl>  <dbl> <dbl>
    ## 1 Chicago          17     31 17031
    ## 2 Los Angeles       6     37  6037
    ## 3 San Francisco     6     75  6075
    ## 4 Oakland           6      1  6001
    ## 5 Phoenix           4     13  4013
    ## 6 Louisville       21    111 21111

``` r
major_cities_fips %>% 
  filter(city == "Chicago") %>% 
  left_join(ct_outcomes_simple, by = c("state", "county")) %>% 
  left_join(ct_cov, by = c("state", "county", "tract")) %>% 
  select(rent_twobed2015, kfr_pooled_pooled_p25) %>% 
  filter(!is.na(rent_twobed2015), !is.na(kfr_pooled_pooled_p25)) %>% 
  ggplot(aes(rent_twobed2015, kfr_pooled_pooled_p25)) +
  geom_point(size = 1, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  coord_cartesian(
    xlim = c(0, 2000),
    ylim = c(.2, .7)
  ) +
  scale_y_continuous(
    breaks = seq(.2, .7, by = .1),
    minor_breaks = NULL,
    labels = c("20", "30", "40", "50", "60", "70")
  ) +
  scale_x_continuous(
    breaks = seq(0, 2000, by = 500),
    minor_breaks = NULL,
    labels = c("$0", "$500", "$1,000", "$1,500", "$2,000")
  ) +
  labs(
    title = "Children's Mean Income Rank in Adulthood vs. Median Rents in Chicago",
    x = "Median Two-Bedroom Rent in 1990 (2015 Dollars)",
    y = "Children's Mean Household Income Rank\nGiven Parents at the 25th Income Percentile"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

![](step_3_progress_report_1_files/figure-gfm/adulthood%20income%20based%20on%20median%20rent%20in%20Chicago-1.png)<!-- -->

``` r
major_cities_fips %>% 
  filter(city == "Chicago") %>% 
  left_join(ct_outcomes_simple, by = c("state", "county")) %>% 
  left_join(ct_cov, by = c("state", "county", "tract")) %>% 
  select(rent_twobed2015, jail_pooled_pooled_p25) %>% 
  filter(!is.na(rent_twobed2015), !is.na(jail_pooled_pooled_p25), jail_pooled_pooled_p25 >= 0) %>% 
  ggplot(aes(rent_twobed2015, jail_pooled_pooled_p25)) +
  geom_boxplot(aes(group = cut_width(rent_twobed2015, 200)), varwidth = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  coord_cartesian(
    xlim = c(0, 2500),
    ylim = c(0, .15)
  ) +
  scale_y_continuous(
    breaks = seq(0, .15, by = .03),
    minor_breaks = NULL,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 2500, by = 500),
    minor_breaks = NULL,
    labels = c("$0", "$500", "$1,000", "$1,500", "$2,000", "$2,500")
  ) +
  labs(
    title = "Adulthood Incarceration Rates vs. Median Rents in Chicago",
    x = "Median Two-Bedroom Rent in 1990 (2015 Dollars)",
    y = "% of Adults Incarcerated on April 1, 2010 (Ages 27-32)\nGiven Parents at the 25th Income Percentile"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

![](step_3_progress_report_1_files/figure-gfm/adulthood%20incarceration%20rates%20based%20on%20median%20rent%20in%20Chicago-1.png)<!-- -->

``` r
major_cities_fips %>% 
  filter(city == "Phoenix") %>% 
  left_join(ct_outcomes_simple, by = c("state", "county")) %>% 
  left_join(ct_cov, by = c("state", "county", "tract")) %>% 
  select(rent_twobed2015, kfr_pooled_pooled_p25) %>% 
  filter(!is.na(rent_twobed2015), !is.na(kfr_pooled_pooled_p25)) %>% 
  ggplot(aes(rent_twobed2015, kfr_pooled_pooled_p25)) +
  geom_point(size = 1, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  coord_cartesian(
    xlim = c(250, 1500),
    ylim = c(.2, .7)
  ) +
  scale_y_continuous(
    breaks = seq(.2, .7, by = .1),
    minor_breaks = NULL,
    labels = c("20", "30", "40", "50", "60", "70")
  ) +
  scale_x_continuous(
    breaks = seq(250, 1500, by = 250),
    minor_breaks = NULL,
    labels = c("$250", "$500", "$750", "$1,000", "$1,250", "$1,500")
  ) +
  labs(
    title = "Children's Mean Income Rank in Adulthood vs. Median Rents in Phoenix",
    x = "Median Two-Bedroom Rent in 1990 (2015 Dollars)",
    y = "Children's Mean Household Income Rank\nGiven Parents at the 25th Percentile"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "gray95")
  )
```

![](step_3_progress_report_1_files/figure-gfm/adulthood%20income%20based%20on%20median%20rent%20in%20Phoenix-1.png)<!-- -->

## Issues

**Logistical**

  - My data total around 3GB and is a bit cumbersome to move around. I’m
    not entirely sure how to go about solving that. Reading it in every
    time I rerun my script is painstakingly slow, so for now I save it
    and load it when need be.

**Technical**

  - To build out some of the maps this paper has, I’ll need to get
    better with geom\_sf. While I hope to be able to do this
    semi-confidently for the next report, I’m still nervous that I won’t
    be able to reproduce maps detailed enough for what I hope to
    achieve.

  - I’ve found a workaround solution for this, but I don’t have a great
    way of looking at data for specific cities. My workaround currently
    entails looking up a given city’s county or commuting zone and then
    looking up the related fips which are present in my data.

  - I’m a bit overwhelmed by all of the data available to me and I’m not
    entirely sure which direction I should head in. Going forward I want
    to make a list of questions to answer with my final report and
    hopefully gain some guidance.

  - Many of the key findings of this report make use of coefficients and
    relationships between factors and outcomes. This is what the final
    findings are based off of, particularly when determining if moving
    to different locations has a noticeable impact on outcomes. In order
    to replicate this effect, I would need to learn how to create models
    somewhat good enough to complete these tasks. It may also be
    possible to gain this information from the paper itself with some
    extra readings.

## Next Steps

  - Key next steps include:
    
      - Making several static maps showing how different outcomes change
        based on region and demographic factors. Seeing this variation
        will be a huge part of the final report, and understanding what
        it means will be even more important.
    
      - Further exploring outcome variation within key cities and
        visualizing how much of a difference moving from one census
        tract to another can make in potential outcomes.
    
      - At this point, I would also like to dive deeper into several
        other outcomes and factors ranging from poverty rates to
        education rates. For example, how do poverty rates vary between
        neighboring census tracts and how do educatino completion rates
        vary? How do they vary from big cities to rural areas?
    
      - Potentially use models to determine the affect that different
        factors have on various outcomes. If not possible, I will try to
        explore this through other means as it is essential to some of
        the bigger findings of the paper.
    
      - Begin to flesh out my challenge ideas. Hopefully, I can build a
        function to allow users to create a handful of maps based on
        where they live or where they’re interested in. More to come on
        this.
