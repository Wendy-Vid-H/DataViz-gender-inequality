library(tidyverse)

library(readxl)
sdoh_20 <- read_excel("SDOH_2020_COUNTY_1_0.xlsx", 
                      sheet = "Data")

### Visualization for "Median Income for Man and Women in 2020"

sdoh_20_a <- sdoh_20 |>
  select(COUNTY, STATE, REGION, ACS_MEDIAN_HH_INC, ACS_MEDIAN_INC_F,
         ACS_MEDIAN_INC_M, ACS_MEDIAN_NONVET_INC)

sdoh_20_a |>
  filter(is.na(ACS_MEDIAN_INC_F) == FALSE) |>
  filter(is.na(ACS_MEDIAN_INC_M) == FALSE) |>
  filter(is.na(REGION) == FALSE) |>
  ggplot() +
  geom_density(mapping = aes(x = ACS_MEDIAN_INC_F,
                             fill = "female",
                             color = "female"),
               bw = 1000, alpha = 0.4) +
  geom_density(mapping = aes(x = ACS_MEDIAN_INC_M,
                             fill = "male",
                             color = "male"),
               bw = 1000, alpha = 0.4) +
  scale_color_manual(values = c("female" = "seagreen",
                                "male" = "darkgoldenrod1"),
                     labels = ) +
  scale_fill_manual(values = c("female" = "green",
                               "male" = "yellow"),
                    name = "Median Income") +
  guides(color = "none")+
  facet_wrap(~REGION) +
  labs(title ="Median Income for Man and Women in 2020",
       subtitle = "Men generally earns more and dominate the top percentiles",
       caption = "Sources: Social Determinants of Health Database",
       x = "Median Income (at county level)")

### Visualization for "Counties with Extreme Inequality among Different Genders' Income"
library(tidyverse)

library(readxl)
sdoh_20 <- read_excel("SDOH_2020_COUNTY_1_0.xlsx", 
                      sheet = "Data")

sdoh_20_b <- sdoh_20 |>
  select(COUNTY, STATE, REGION, AHRF_USDA_RUCC_2013,
         ACS_MEDIAN_INC_F, ACS_MEDIAN_INC_M) |>
  filter(is.na(ACS_MEDIAN_INC_F) == FALSE) |>
  filter(is.na(ACS_MEDIAN_INC_M) == FALSE) |>
  filter(is.na(AHRF_USDA_RUCC_2013) == FALSE) |>
  mutate(mfwrate = ACS_MEDIAN_INC_M/ACS_MEDIAN_INC_F) |>
  mutate(mfwind = if_else(
    mfwrate < 2, 1,0
  )) |>
  filter(is.na(AHRF_USDA_RUCC_2013) == FALSE) |>
  mutate(urbanlev = case_when(
    AHRF_USDA_RUCC_2013 %in% 1:3 ~ "Urban",
    AHRF_USDA_RUCC_2013 %in% 4:6 ~ "Suburban",
    AHRF_USDA_RUCC_2013 %in% 7:9 ~ "Rural"
  )) 


sdoh_20_b2 <- sdoh_20_b |>
  select(REGION, mfwind, urbanlev) |>
  filter(is.na(REGION)== FALSE) |>
  filter(mfwind == 0) |>
  select(REGION, urbanlev) |>
  group_by(REGION, urbanlev) |>
  summarise(n = n()) |>
  mutate(ratio = n / sum(n)) 

sdoh_20_b2 |>
  ggplot(mapping = aes(x = REGION,
                       y = n,
                       fill = urbanlev)) +
  geom_col() +
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = c("Urban" = "khaki2",
                               "Suburban" = "darkturquoise",
                               "Rural" = "darkgreen"),
                    name = "Urbanization Level",
                    labels = c( "Urban" = "Urban \n (RUCC~[1,3])",
                                "Suburban" = "Suburban \n (RUCC~[4,6])",
                                "Rural" = "Rural \n (RUCC~[7,9])")) +
  labs(title = "Counties with Extreme Inequality among Different Genders' Income",
       subtitle = "Base line: Median Income for men is larger than 2*Meidan Income for women",
       caption = "Sources: Social Determinants of Health Database \n  RUCC: Rural Urban Continuum Codes",
       x = "Census Region",
       y = "Numbers of Counties with \n Extreme Inequality")

