## Oats_National_Acres_Harvested
### Slider+GIF
This code uses data found on the USDA NASS Quick Stats website (note that the data table needs to be formatted to match the example below to be compatible with this code) 


<img width="241" height="121" alt="image" src="https://github.com/user-attachments/assets/d2bd1680-0564-40ac-bd87-6acb798ae1a7" />


`````
#install packages as needed
library('usmap')
library('ggplot2')
library('dplyr')
library('data.table')
library('gganimate')
library('gifski')
library('plotly')
library('htmlwidgets')
library('stringr')

setwd("C:/Users/Ellie/OneDrive/Desktop/Oats")

#This step reads in the raw NASS output and reformats 
oat_data <- (read.csv("OATS_Acres_Harvested_By_State.csv.csv"))
oat_data <- oat_data %>%
  select(Year, State, `Acres.Harvested`) %>%
  rename(year = Year, state = State, acres = `Acres.Harvested`) %>%
  filter(state != "OTHER STATES") %>%
  mutate(
    state = str_to_title(state),   # converts "IOWA" -> "Iowa"
    year  = as.integer(year),
    acres = as.numeric(acres)
  )

# ── 5. Build the Plotly interactive slider map ──────────────────────────────────
oat_data_clean <- oat_data %>%
  mutate(
    state_clean = trimws(state),
    state_code  = state.abb[match(state_clean, state.name)]
  ) %>%
  filter(!is.na(state_code))  # removes any rows that didn't match a real state

#this sets the upper and lower boundaries for the heat map
min_acres <- min(oat_data_clean$acres, na.rm = TRUE)
max_acres <- max(oat_data_clean$acres, na.rm = TRUE)

interactive_map <- plot_geo(oat_data_clean, locationmode = "USA-states") %>%
  add_trace(
    z         = ~log10(acres +1), 
    locations = ~state_code,
    color     = ~log10(acres +1),
    colors    = c("#b22222", "#ffcc00", "#006d2c"),
    frame     = ~year,
    zmin      = log10(min_acres +1),
    zmax      = log10(max_acres +1),
    colorbar  = list(title = "Harvested Acres",
                     tickvals = log10(c(1000, 5000, 10000, 50000, 100000, 500000, 1000000) + 1), 
                     ticktext = c("1K", "5K", "10K", "50K", "100K", "500K", "1M"),
                     tickformat = "")
  ) %>%
  layout(
    title = list(
      text = "<b>U.S. Oat Harvested Acres by Year</b>",
      font = list(size = 22)
    ),
    geo = list(
      scope      = "usa",
      projection = list(type = "albers usa")
    )
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "YEAR: ",
      font   = list(size = 22, color = "#d95f02", weight = "bold")
    ),
    font = list(size = 16, color = "#333333")
  )

# Display it in RStudio's viewer pane
interactive_map

# Save it as a standalone HTML file you can open in any browser
#We had issues getting the HTML file to work for users who were not running the whole code on their machine 
saveWidget(interactive_map, file = "oat_harvested_acres_slider.html", selfcontained = TRUE)
htmltools::save_html(interactive_map, file = "oat_harvested_acres_slider.html")

# ── 6. OPTIONAL: GIF version ─────────────────────────────────────────────────── 
# Only run this if you want a GIF — it takes several minutes to render
oat_data <- oat_data %>%
  filter(!is.na(year), year > 0)

all_states <- data.frame(state = state.name)
all_years  <- data.frame(year = unique(oat_data$year))

oat_data_full <- merge(all_states, all_years) %>%
  left_join(oat_data, by = c("state", "year")) %>%
  mutate(acres = ifelse(is.na(acres), 1, acres))

animated_map <- plot_usmap(data = oat_data_full, values = "acres", color = "gray50") +
  scale_fill_gradientn(
    name   = "Harvested Acres",
    colours    = c("#b22222", "#ffcc00", "#006d2c"),
    trans = "log10",
    breaks = c(1000, 5000, 10000, 50000, 100000, 500000, 1000000),
    labels = c("1K", "5K", "10K", "50K", "100K", "500K", "1M"),
    limits = c (1,10000000),
    oob = scales::squish,
    na.value = "white",
    guide  = guide_colorbar(barwidth = 21, barheight = 2.0, title.position = "right")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text            = element_text(size = 12),
    plot.title      = element_text(face = "bold", size = 16)
  ) +
  labs(
    title    = "U.S. Oat Harvested Acres in Year: {current_frame}",
    subtitle = "Data pulled from historical records"
  ) +
  transition_manual(year)

animate(animated_map, renderer = gifski_renderer("oat_harvested_acres.gif"), width = 800, height = 600, fps = 3)
`````

## Here are snapshots of what the Slider map and GIF look like 
### Slider
Hovering over the states on the map will show individual states' values for that year
<img width="1915" height="508" alt="Screenshot 2026-07-01 133113" src="https://github.com/user-attachments/assets/8940540c-242c-42e2-8807-a026214b4a89" />


### GIF
<img width="783" height="582" alt="Screenshot 2026-07-01 133008" src="https://github.com/user-attachments/assets/90e6f452-e3f9-4a44-a1c3-61a2e0ec9a2d" />
