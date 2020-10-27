# Load the libraries

library(tidyverse)
library(scales)
library(fpp3)
theme_set(theme_light())

# Load tha data
kids <- read_rds("kids.RDS")


# Converting into tsibble

kids_ts <- kids %>% 
    as_tsibble(key = c(variable, state),
               index = year)

# Visualising random time plots
kids_ts %>%
    filter(variable == "addCC") %>%
    autoplot(1000 *inf_adj_perchild, show.legend = F, size = 1) +
    facet_wrap( ~ state) +
    theme_tufte() +
    geom_vline(xintercept = 2009, lty = 2, color = "gray60") +
    scale_y_continuous(labels = dollar_format(), breaks = c(0:2)) +
    labs(title = "Government Spendings on Elementary & Primary Education",
         x = "",
         y = "Amount in $ (Inf adj per child)")

kids_ts %>% 
    filter(variable == "PK12ed",
           state %in% sample(unique(state), 5)) %>% 
    model(naive = NAIVE(inf_adj_perchild),
          drift = RW(inf_adj_perchild ~ drift()),
          mean = MEAN(inf_adj_perchild)) %>% 
    forecast(h = 4) %>% 
    autoplot(kids_ts, level = NULL)
