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

# Electronic card transactions from Stats NZ Infoshare
ect_dat <- read_csv(file = here("data/ECT383901_20200710_121103_86.csv"), 
                    skip = 2, 
                    na = "..") %>%
  pivot_longer(cols = -X1, 
               names_to = "category", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  separate(col = X1, into = c("year", "month"), sep = "M", convert = TRUE)

# *****************************************************************************


# *****************************************************************************
# Utility functions ---

# Customised y-axis break function because I don't like ggplot default
better_y_axis_breaks <- function(x) {
  if (max(x) < 100) {
    br <- seq(-100, 100, 10)
  } else {
    br <- seq(-1000, 1000, 100)
  }
  return(br)
}

# *****************************************************************************


# *****************************************************************************
# Monthls sales value comparison ----
# Compare monthly values for 2020 vs average for tge same months in 2017 to 2019

ect_value_comparison <- ect_dat %>%
  filter(year > 2016) %>%
  group_by(category) %>%
  add_count(month, name = "month_n") %>%
  ungroup() %>%
  filter(month_n == max(month_n)) %>%
  mutate(grouping = ifelse(year == 2020, "y2020", "avg_y2017_2019")) %>%
  group_by(category, grouping, month) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = grouping, values_from = value) %>%
  mutate(delta = y2020 - avg_y2017_2019) %>%
  mutate(delta_pct = 100 * delta / avg_y2017_2019) %>%
  select(category, month, delta, delta_pct) %>%
  pivot_longer(cols = c(delta, delta_pct), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(sign = ifelse(value > 0, "pos", "neg")) %>%
  mutate(value_label = case_when(
    (measure == "delta") & (value > 0) ~ paste0("+", round(value)), 
    (measure == "delta") & (value < 0) ~ paste0(round(value)), 
    (measure == "delta_pct") & (value > 0) ~ paste0("+", round(x = value, digits = 0), "%"), 
    (measure == "delta_pct") & (value < 0) ~ paste0(round(x = value, digits = 0), "%")
  )) %>%
  mutate(vjust = ifelse(value > 0, -0.3, 1.3)) %>%
  mutate(measure = factor(x = measure, 
                          levels = c("delta", "delta_pct"), 
                          labels = c("Million dollars\n(2020 vs 2017-2019 average)", 
                                     "Percent\n(2020 vs 2017-2019 average average)"), 
                          ordered = TRUE)) %>%
  mutate(month_label = fct_reorder(.f = month.abb[month], .x = month)) %>%
  arrange(category, measure, month)

ect_value_comparison_chart <- ect_value_comparison %>%
  ggplot(mapping = aes(x = month_label, 
                       y = value,
                       label = value_label, 
                       colour = sign, 
                       fill = sign)) + 
  geom_hline(yintercept = 0, size = 0.25, colour = grey(0.75)) + 
  geom_col(width = 0.75, 
           size = 0) + 
  my_geom_text(mapping = aes(vjust = vjust), 
               rel_size = 0.45) + 
  facet_grid(rows = vars(measure), 
             cols = vars(category), 
             scales = "free", 
             labeller = labeller(category = label_wrap(16))) + 
  scale_colour_manual(values = c("pos" = "cornflowerblue", 
                                 "neg" = "darkgoldenrod3"), 
                      aesthetics = c("colour", "fill"), 
                      guide = "none") +
  scale_y_continuous(breaks = better_y_axis_breaks, 
                     labels = comma, 
                     expand = expansion(0.05, 0)) + 
  xlab("") + 
  ylab("")

output_chart(chart = ect_value_comparison_chart, 
             path = here("outputs"), 
             width = 16, 
             height = 8, 
             units = "cm", 
             xlab = "", 
             ylab = "", 
             base_size = 4, 
             panel.spacing.y = unit(4, "pt"), 
             panel.spacing.x = unit(4, "pt"), 
             plot.margin = margin(4, 0, 4, 4, "pt"), 
             axis.ticks.x = element_blank(), 
             plot.title = element_blank(), 
             axis.title.x = element_blank(), 
             axis.title.y = element_blank())

# *****************************************************************************

