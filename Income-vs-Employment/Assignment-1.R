library(tidyverse)
library(readxl)

X2020_county_UR <- read_excel("2020_county_UR.xlsx")

ggplot(data = X2020_county_UR,
       mapping = aes(x = SAIPE_MEDIAN_HH_INCOME,
                     y = ACS_PCT_EMPLOYED)) +
  geom_point(color = "seagreen", alpha = 0.2, shape = 16) +
  geom_smooth(color = "slateblue",
              se = FALSE) +
  labs(title = "Household Income Versus Employment Rate in US counties",
       subtitle = "Employment rate rises with median household income, fast at first, then slows down",
       caption = "Data Sources: Agency for Healthcare Research and Quality",
       x = "Estimated median household income",
       y = "Percentage of civilian labor force that is employed")
  

