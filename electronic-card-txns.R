# Analysis of Electronic Card Transactions data

# Data source: Stats NZ

# *****************************************************************************
# Setup ----

library(conflicted)
library(tidyverse)
library(lubridate)
library(here)
library(my.r.functions)
library(janitor)
library(scales)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ----

ect_dat <- read_csv(file = here("data/ECT383901_20200413_061543_27.csv"), 
                    skip = 2, 
                    na = "..") %>%
  pivot_longer(cols = -X1, 
               names_to = "category", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  separate(col = X1, into = c("year", "month"), sep = "M", convert = TRUE)

# *****************************************************************************


# *****************************************************************************
# Compare March values for 2020 vs average for the past 3 years ----

march_ect_comparison <- ect_dat %>% 
  filter(month == 3, year > 2016) %>%
  mutate(grouping = ifelse(year == 2020, "y2020", "avg_y2017_2019")) %>%
  group_by(category, grouping) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = grouping, values_from = value) %>%
  mutate(delta = y2020 - avg_y2017_2019) %>%
  mutate(delta_pct = 100 * delta / avg_y2017_2019) %>%
  select(category, delta, delta_pct) %>%
  pivot_longer(cols = c(delta, delta_pct), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(sign = ifelse(value > 0, "pos", "neg")) %>%
  mutate(value_label = case_when(
    (measure == "delta") & (value > 0) ~ paste0("+", dollar(value, accuracy = 1), "m"), 
    (measure == "delta") & (value < 0) ~ paste0(dollar(value, accuracy = 1), "m"), 
    (measure == "delta_pct") & (value > 0) ~ paste0("+", round(x = value, digits = 0), "%"), 
    (measure == "delta_pct") & (value < 0) ~ paste0(round(x = value, digits = 0), "%")
  )) %>%
  mutate(vjust = ifelse(value > 0, -0.3, 1.3)) %>%
  mutate(measure = factor(x = measure, 
                          levels = c("delta", "delta_pct"), 
                          labels = c("Million dollars (2020 vs 2017-2019 average)", 
                                     "Percent (2020 vs 2017-2019 average)"), 
                          ordered = TRUE))

better_y_axis_breaks <- function(x) {
  if (max(x) < 100) {
    br <- seq(-100, 100, 10)
  } else {
    br <- seq(-1000, 1000, 100)
  }
  return(br)
}

march_ect_comparison_chart <- march_ect_comparison %>%
  ggplot(mapping = aes(x = str_wrap(string = category, width = 12), 
                       y = value,
                       label = value_label, 
                       colour = sign, 
                       fill = sign)) + 
  geom_hline(yintercept = 0, size = 0.25, colour = grey(0.75)) + 
  geom_col() + 
  my_geom_text(mapping = aes(vjust = vjust)) + 
  facet_wrap(facets = vars(measure), ncol = 1, scales = "free") + 
  scale_colour_manual(values = c("pos" = "dodgerblue4", 
                                 "neg" = "firebrick4"), 
                      aesthetics = c("colour", "fill"), 
                      guide = "none") +
  scale_y_continuous(breaks = better_y_axis_breaks)

output_chart(chart = march_ect_comparison_chart, 
             path = here("outputs"), 
             orientation = "square", 
             axis.ticks.x = element_blank(), 
             xlab = "", 
             ylab = "", 
             panel.spacing.y = unit(0.1, "cm"), 
             plot.margin = margin(0, 24, 0, 0))

 # *****************************************************************************