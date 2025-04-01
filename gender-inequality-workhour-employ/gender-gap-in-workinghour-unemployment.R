library(haven)
acs1721_secol <- read_dta("17_21acs_v2.dta")
View(acs1721_secol)

library(tidyverse)

### Graph 1: Gaps between Men and Women's Working Hours

wrhr_slim <- acs1721_secol |>
  filter(is.na(acs_mean_workhr_f) == FALSE) |> 
  filter(is.na(acs_mean_workhr_m) == FALSE) |>
  select(state, county,region, statefips, countyfips, year, acs_mean_workhr_m, acs_mean_workhr_f)

wrhr_yrrg_long <- wrhr_slim |>
  pivot_longer(cols = c(acs_mean_workhr_f, acs_mean_workhr_m),
               names_to = "gender_lab",
               values_to = "median_workhr") 

wrhr_yrrg_long <- wrhr_yrrg_long |>
  filter(median_workhr > 0)

wrhr_yrrg_long$year = factor(wrhr_yrrg_long$year)

ggplot(data = wrhr_yrrg_long) +
  geom_boxplot(mapping = aes(x = year,
                             y = median_workhr,
                             fill = gender_lab),
               outlier.shape = 19,
               outlier.size = 1,
               outlier.alpha = 0.3)+
  facet_wrap(~region) +
  scale_fill_manual(values = c("acs_mean_workhr_m" = "moccasin",
                               "acs_mean_workhr_f" = "mediumseagreen"),
                    labels = c("acs_mean_workhr_m" = "Men's Mean \n Working Hours",
                               "acs_mean_workhr_f" = "Women's Mean \n Working Hours")) +
  theme(legend.title = element_blank()) +
  labs(title = "Notable Gaps between Men and Women's Working Hours",
       subtitle = paste("Northeast has the smallest in-region disparities",
                        "while West has the largest"),
       caption = "Data: 2017 - 2021 ACS 5 years estimate",
       x = "Year",
       y = "Median Working Hours \n at County Level (per week)")

### Graph 2: Women's Employment Percentage VS Total Employment Percentage (at county level)

emp_slim <- acs1721_secol |>
  filter(is.na(acs_pct_employed) == FALSE) |> 
  filter(is.na(acs_female_employed) == FALSE) |>
  filter(acs_female_employed > 0) |>
  filter(acs_pct_employed > 0) |>
  select(state, county,region, statefips, countyfips, year, 
         acs_pct_employed, acs_female_employed)

emp_slim_yr <- emp_slim |>
  group_by(year, region) |>
  summarize("mean_tol" = mean(acs_pct_employed),
            "mean_fem" = mean(acs_female_employed))

emp_yrrg_long <- emp_slim_yr |>
  pivot_longer(cols = c(mean_tol, mean_fem),
               names_to = "gender_lab",
               values_to = "mean_emprate")

emp_yrrg_long|>
  ggplot(mapping = aes(x = year,
                       y = mean_emprate,
                       linetype = gender_lab))+
  geom_line(mapping = aes(color = region))+
  scale_linetype_manual(labels = c("mean_fem" = "female",
                                   "mean_tol" = "total"),
                        values = c("mean_fem" = 1,
                                   "mean_tol" = 2))+
  labs(title = paste("Women's employment percentage generally lacks behind",
                     "and the trend is quite stable"),
       subtitle = paste("Despite dotable difference of overall employment",
                        "situation among different regions"),
       caption = "Data: 2017 - 2021 ACS 5 years estimate",
       x = "Year",
       y = "Mean Employment Rate \n at County Level")

