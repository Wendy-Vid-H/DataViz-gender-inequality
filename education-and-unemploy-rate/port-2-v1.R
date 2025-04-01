library(tidyverse)

library(haven)
ur_bac <- read_dta("ur-bac.dta")
View(ur_bac)

ur_bac |>
  mutate(popm = totalpopulation/1000000) |>
  ggplot(mapping = aes(x = pct_bachelor,
                       y = unemploymentrate, label = state)) +
  geom_point(mapping = aes(size = popm), color = "forestgreen") +
  geom_smooth(
    se = FALSE, method = "lm",
    alpha = 0.8, size = 0.7, color = "deepskyblue"
  ) +
  geom_smooth(
    se = FALSE, alpha = 0.8, 
    size = 0.7, color = "darkslateblue") +
  geom_text(size = 2) +
  scale_size_continuous(name = "Population \n (in Millions)")+
  guides(color = "none") +
  labs(title = "Education Level Versus Unemployment Rate",
       subtitle = expression(italic("Overall High Education level does not reduce unemployment rate")),
       x = "Bachelor Degree Percentage",
       y = "Unemployment rate",
       caption = "Source: 2020 ACS")

ur_bac |>
  mutate(popm = totalpopulation/1000000) |>
  filter(state != "District of Columbia") |>
  ggplot(mapping = aes(x = pct_bachelor,
                       y = unemploymentrate, label = state)) +
  geom_point(mapping = aes(size = popm), color = "forestgreen") +
  geom_smooth(
    se = FALSE, method = "lm",
    alpha = 0.8, size = 0.7, color = "deepskyblue"
  ) +
  geom_smooth(
    se = FALSE, alpha = 0.8, 
    size = 0.7, color = "darkslateblue") +
  geom_text(size = 2) +
  scale_size_continuous(name = "Population \n (in Millions)")+
  guides(color = "none") +
  labs(title = "Education Level Versus Unemployment Rate",
       subtitle = expression(italic("Overall High Education level does not reduce unemployment rate")),
       x = "Bachelor Degree Percentage",
       y = "Unemployment rate",
       caption = "Source: 2020 ACS")



