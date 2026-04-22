# ----------------------------------------------------------------------

# Date: 4/13/2026
# Creator: Jordan Sibley 
# Purpose: Create a plot to show monarch butterfly survivorship

# ----------------------------------------------------------------------

# -------------------------------------------------------------------------


# Set up ----
# libraries 
library(tidyverse)
library(readxl)
library(here)
library(showtext)

# font
font_add_google(name = "Lato", family = "Lato")
showtext_auto(enable = TRUE)

# Data ----

# Read in 'milkweed_tidydata' sheet from milkweed_tidydata 
milkweed_data <- read_excel(here("WCB-monarch", "data", "milkweed_tidydata.xlsx"), sheet = "milkweed_tidydata", na = "NA")


# Create annotation column
milkweed_data <- milkweed_data |> 
  mutate(annotation = case_when(site_name == "Joske Grove" & type == "Plants" & origin == "Planted" & count_monitoring == 86 ~ "52% of planted\nsurvived", 
                                site_name == "Joske Grove" & type == "Plants" & origin == "Planted" & count_monitoring == 54 ~ "68% of planted\nsurvived",
                                site_name == "San Carlos Way" & type == "Plants" & origin == "Planted" & count_monitoring == 43 ~ "25% of planted\nsurvived",
                                site_name == "San Carlos Way" & type == "Plants" & origin == "Planted" & count_monitoring == 212 ~ "424% of planted\nsurvived",
                                site_name == "Bowman Bridge" & type == "Plants" & origin == "Planted" & count_monitoring == 21 ~ "6% of planted\nsurvived", 
                                site_name == "Bowman Bridge" & type == "Plants" & origin == "Planted" & count_monitoring == 51 ~ "19% of planted\nsurvived", 
                                TRUE ~ NA))
# Alter monitoring_year column so years are below 
milkweed_data <- milkweed_data |> 
  mutate(monitoring_year2 = case_when(monitoring_year == "Baseline average (2022 & 2023)" ~ "Baseline average\n(2022 & 2023)",
                                      monitoring_year == "Post-planting Year 1 (2024)" ~ "Post-planting Year 1\n(2024)",
                                      monitoring_year == "Post-planting Year 2 (2025)" ~ "Post-planting Year 2\n(2025)")) |> 
  select(c(-monitoring_year)) |> 
  rename("monitoring_year" = "monitoring_year2")

# Graphing ----


# Joske Grove stacked bar plot
joske_plot <- milkweed_data |>
  filter(site_name == "Joske Grove") |> 
  ggplot( aes(x = monitoring_year, y = count_monitoring, fill = origin)) +
  geom_col() +
  
  facet_wrap(~ type, ncol = 2, nrow = 1) + 
  scale_fill_manual(values = c("Wild" = "#D89FAE", "Planted" = "#6C3A5C")) +
  
  labs(title = "Narrow-leaf Milkweed Monitoring Counts at Joske Grove", 
       x = "Monitoring Period",
       y = "Count",
       fill = "Plant Origin") +
  
  # % annotations 
  geom_text( aes(x = monitoring_year, 
                 y = count_monitoring,
                 label = annotation),
             position = position_stack(vjust = 1),
             vjust = -0.5,
             size = 3.5,
             family = "Lato") +

  theme_light() +
  theme(
    # text
    axis.title.x = element_text(family = "Lato", size = 12), 
    axis.title.y = element_text(family = "Lato", size = 12),
    plot.title = element_text(family = "Lato", size = 18),
    axis.text = element_text(family = "Lato", size = 10),
    
    # legend
    legend.title = element_text(family = "Lato", size = 10), 
    legend.position = c(0.07, 0.85),
    legend.background = element_rect(fill = "grey85"),
    legend.text = element_text(family = "Lato", size = 8),
    
    # facet title box
    strip.background = element_rect(
      fill = "#668401",
      color = "#668401"),
    strip.text = element_text(
      family = "Lato",
      color = "#F4F5F3",
      face = "bold", 
      size = 16)
    
  )
    
joske_plot  

# San carlos way plants and stems plot
sancarlos_plot <- milkweed_data |>
  filter(site == "San Carlos Way") |> 
  ggplot( aes(x = status, y = count)) +
  geom_col(fill = "#D89FAE") +
 
   # survivorship labels
  geom_text(aes(label = survivorship),
            vjust = -0.5,
            size = 3.5,
            family = "Lato") +
  
  facet_wrap(~ type, ncol = 2, nrow = 1) + 
  
  labs(title = "Narrow-leaf Milkweed Monitoring Counts at San Carlos Way", 
       x = "Monitoring Period",
       y = "Count") +
  
  theme_light() +
  theme(
    axis.title.x = element_text(family = "Lato", size = 12), 
    axis.title.y = element_text(family = "Lato", size = 12),
    plot.title = element_text(family = "Lato", size = 18),
    axis.text = element_text(family = "Lato", size = 10),
    
    
    
    # facet title box
    strip.background = element_rect(
      fill = "#668401",
      color = "#668401"),
    strip.text = element_text(
      family = "Lato",
      color = "#F4F5F3",
      face = "bold", 
      size = 16)
  )

sancarlos_plot  

# Bowman Bridge plants and stems plot 
bowman_plot <- milkweed_data |>
  filter(site == "Bowman Bridge") |> 
  ggplot( aes(x = status, y = count)) +
  geom_col(fill = "#D89FAE") +
  
  # survivorship labels
  geom_text(aes(label = survivorship),
            vjust = -0.5,
            size = 3.5,
            family = "Lato") +
  
  facet_wrap(~ type, ncol = 2, nrow = 1) + 
  
  labs(title = "Narrow-leaf Milkweed Survivorship at Bowman Bridge", 
       x = "Monitoring Period",
       y = "Count") +
  
  theme_light() +
  theme(
    axis.title.x = element_text(family = "Lato", size = 12), 
    axis.title.y = element_text(family = "Lato", size = 12),
    plot.title = element_text(family = "Lato", size = 18),
    axis.text = element_text(family = "Lato", size = 10),
    
    # facet title box
    strip.background = element_rect(
      fill = "#668401",
      color = "#668401"),
    strip.text = element_text(
      family = "Lato",
      color = "#F4F5F3",
      face = "bold", 
      size = 16)
  )

bowman_plot  


# Export ----
# save as svg files
# ggsave(
#   filename = here::here("WCB-monarch", "plot-exports", "joske_plot.svg"),
#   plot = joske_plot,
#   device = svg,
#   width = 9.5,
#   height = 5
# )
# 
# ggsave(
#   filename = here::here("WCB-monarch", "plot-exports", "sancarlos_plot.svg"),
#   plot = sancarlos_plot,
#   device = svg,
#   width = 9.5,
#   height = 5
# )
# 
# ggsave(
#   filename = here::here("WCB-monarch", "plot-exports", "bowman_plot.svg"),
#   plot = bowman_plot,
#   device = svg,
#   width = 9.5,
#   height = 5
# )