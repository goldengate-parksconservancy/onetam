# ----------------------------------------------------------------------

# Date: 4/13/2026
# Creator: Jordan Sibley 
# Purpose: Create a plot to show monarch butterfly survivorship

# ----------------------------------------------------------------------

# -------------------------------------------------------------------------


# Set up ----
# libraries 
library(tidyverse)
library(here)
library(showtext)

# font
font_add_google(name = "Lato", family = "Lato")
showtext_auto(enable = TRUE)

# Data ----
# create dataset based on monarch annual summary tables
milkweed_data <- data.frame(
  site = c("Joske Grove", "Joske Grove", "Joske Grove",
           "Joske Grove", "Joske Grove", "Joske Grove",
           
           "San Carlos Way", "San Carlos Way", "San Carlos Way",
           "San Carlos Way", "San Carlos Way", "San Carlos Way",
           
           "Bowman Bridge", "Bowman Bridge", "Bowman Bridge",
           "Bowman Bridge", "Bowman Bridge", "Bowman Bridge"),
  
  status = c("Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)",
             "Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)",
             
             "Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)",
             "Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)",
             
             "Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)",
             "Baseline average\n(2022 & 2023)", "Post-planting Year 1\n(2024)", "Post-planting Year 2\n(2025)"),
  
  type = c("Plants", "Plants", "Plants",
           "Stems", "Stems", "Stems",
           
           "Plants", "Plants", "Plants",
           "Stems", "Stems", "Stems",
           
           "Plants", "Plants", "Plants",
           "Stems", "Stems", "Stems"), 
  count = c(1172, 689, 962,
            1682, 1494, 1831, 
            
            186, 229, 398,
            239, 354, 792,
            
            0, 21, 51,
            0, 35, 79),
  survivorship = c(NA, "52%", "77%",
                   NA, "81%", "104%",
                   
                   NA, "64%", "169%",
                   NA, "86%", "274%",
                   
                   NA, "6%", "19%",
                   NA, "10%", "29%")
)



# Graphing ----
# Joske Grove plants & stems
joske_plot <- milkweed_data |>
  filter(site == "Joske Grove") |> 
  ggplot( aes(x = status, y = count)) +
  geom_col(fill = "#D89FAE") +
  
  
 # survivorship labels
  geom_text(aes(label = survivorship),
    vjust = -0.5,
    size = 3.5,
    family = "Lato") +
  
  facet_wrap(~ type, ncol = 2, nrow = 1) + 
  
  labs(title = "Narrow-leaf Milkweed Survivorship at Joske Grove", 
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
  
  labs(title = "Narrow-leaf Milkweed Survivorship at San Carlos Way", 
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