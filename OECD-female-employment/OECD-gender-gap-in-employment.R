library(tidyverse)
library(ggridges)

### Graph 1: Women's Employment in OECD countries: from 2014 to 22=023

library(readxl)
oecd_edu_t <- read_excel("Women_Men_Education_total.xlsx")
View(oecd_edu_t)

oecd_edu_t |>
  filter(is.na(OBS_VALUE) == FALSE) |>
  filter(is.na(TIME_PERIOD) == FALSE) |>
  filter(TIME_PERIOD >= 2014) |>
  filter(SEX == "F") |>
  ggplot(mapping = aes(x = OBS_VALUE,
                       y = as.factor(TIME_PERIOD))) +
  geom_density_ridges(alpha = 0.4,
                      scale = 1.5,
                      fill = "lightcoral",
                      color = "mediumpurple") +
  scale_x_continuous(breaks = seq(0, 40, by = 5))+
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())+
  geom_vline(xintercept = 3.5, color = "blue", linetype = 1)+
  labs(title = "Women's Unemployment in OECD Countries Shows an Overall Trend of Decline and Concentration",
       subtitle = "Comparison: The unemployment rate for women in the US in 2023 is around 3.5%",
       x = "Unemployment Rate (%)",
       y = "Year",
       caption = "Data Source: OECD")

### Graph 2: Education's effect on Gender Inequality in Employment  

library(tidyverse)

library(readxl)
oecd_edudiv <- read_excel("WF_edu_spe.xlsx")
View(oecd_edudiv)

oecd_edudiv |>
  filter(is.na(OBS_VALUE) == FALSE) |>
  filter(is.na(TIME_PERIOD) == FALSE) |>
  filter(REF_AREA != "G20") |>
  ggplot() +
  geom_boxplot(mapping = aes( x = as.factor(TIME_PERIOD),
                              y = OBS_VALUE,
                              fill = SEX),
               outlier.size = 1,
               outlier.alpha = 0.3) +
  scale_fill_manual(values = c("F" = "thistle1",
                               "M" = "springgreen"),
                    labels = c("F" = "female",
                               "M" = "male"))+
  ylim(0, 30) +
  facet_wrap(~Edu_atlevel) +
  labs(title = "Education Reduces Gender Inequality in Employment",
       subtitle = "Disparity between men and women is the largest among people with below upper secondary education",
       x = "Year",
       y = "Unemployment Rate (%)",
       caption = "Data Source: OECD Data")



