# Load the libraries

library(tidyverse)
library(scales)
library(ggthemes)
theme_set(theme_light())

# Load tha data
kids <- read_rds("kids.RDS")


# Visualization
kids %>% 
    filter(variable == "PK12ed",
           state %in% unique(state)) %>% 
    mutate(state = fct_reorder(state, inf_adj_perchild, max, .desc = T)) %>% 
    ggplot(aes(year, inf_adj_perchild, color = state)) +
    geom_line(size = 1, show.legend = F) +
    geom_vline(xintercept = 2009, color = "gray60", lty = 2) +
    facet_wrap(~state) +
    scale_y_continuous(labels = dollar_format()) +
    theme_tufte() +
    labs(title = "Government Spendings on Elementary & Primary Education",
         x = "",
         y = "Amount in $ (Inf adj per child)")

# Lets create a function for this plot

plot_change_faceted <- function(tbl) {
    tbl %>% 
        group_by(state, variable) %>%
        mutate(change = inf_adj_perchild / first(inf_adj_perchild) - 1) %>%
        ungroup() %>%
        plot_faceted(change)
}

plot_faceted <- function(tbl, y_axis) {
    tbl %>% 
        mutate(state = fct_reorder(state, {{y_axis}}, max, .desc = T)) %>% 
        ggplot(aes(year, {{y_axis}}, color = state)) +
        geom_line(size = 1, show.legend = F) +
        geom_hline(yintercept = 0,
                   color = "gray60",
                   lty = 4) +
        facet_wrap(~ state)
}

# Plot using plot_faceted    
kids %>% 
    filter(variable == "PK12ed") %>% 
    plot_faceted(inf_adj_perchild * 1000) +
    geom_vline(xintercept = 2009,
               color = "red",
               lty = 2) +
    scale_y_continuous(labels = dollar_format(), breaks = c(0:2)) +
    theme_tufte() +
    labs(title = "Government Spendings on Higher Education",
         x = "",
         y = "Increase in inflation adjusted spendings per child relative to 1997($)")

# Plot using plot_change_faceted
kids %>% 
    filter(variable == "highered") %>% 
    plot_change_faceted() +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0:2)) +
    theme_tufte() +
    labs(title = "Government Spendings on Higher Education",
         x = "",
         y = "Increase in inflation adjusted spendings per child relative to 1997($)")
    

