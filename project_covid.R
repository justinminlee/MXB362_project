# Load required libraries
library(ggplot2)
library(gganimate)
library(dplyr)
library(viridis)

# Load your dataset (adjust the path as necessary)
covid_data <- read.csv("covid_with_latlong.csv")

# Convert Date to Date type
covid_data$Date <- as.Date(covid_data$Date, format = "%d/%m/%Y")

# Aggregate cases by country and date
covid_summary <- covid_data %>%
  group_by(Entity, Code, Date, Latitude, Longitude) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
  ungroup()

# Create the base plot using ggplot2
base_plot <- ggplot(covid_summary, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") +
  geom_point(aes(size = Cases, color = Cases), alpha = 0.7) +
  scale_size(range = c(1, 10), guide = "none") +
  scale_color_viridis_c(option = "C", trans = "log") +  # Log scale for color
  labs(title = 'COVID-19 Cases by Country: {frame_time}', 
       x = 'Longitude', 
       y = 'Latitude') +
  theme_minimal() +
  theme(legend.position = "right")

# Animate the plot with gganimate
animated_plot <- base_plot +
  transition_time(Date) +               # Time-based animation on Date
  ease_aes('linear') +                  # Linear transition between dates
  labs(subtitle = "Date: {frame_time}")  # Subtitle showing the current date

# Display the animated plot
animate(animated_plot, nframes = 300, fps = 10, width = 900, height = 600)
