library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(plotly)

# Data
data <- global_air_quality_2014_2025
names(data) <- make.names(names(data))

pm25_country <- data %>%
  group_by(Country) %>%
  summarise(PM2.5 = mean(PM2.5..ug.m3., na.rm = TRUE))

# Wereldkaart
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge
map_data <- world %>%
  left_join(pm25_country, by = c("name" = "Country"))

# Alleen landen met data
map_data <- map_data %>% filter(!is.na(PM2.5))

# Centroid per land
centroids <- st_centroid(map_data)

coords <- st_coordinates(centroids)
centroids$lon <- coords[,1]
centroids$lat <- coords[,2]

# Plotly puntenkaart
fig <- plot_ly(
  data = centroids,
  lon = ~lon,
  lat = ~lat,
  type = "scattergeo",
  mode = "markers",
  marker = list(
    size = 6,
    color = ~PM2.5,
    colorscale = "Viridis",
    reversescale = FALSE,
    colorbar = list(title = "PM2.5")
  ),
  text = ~paste("Land:", name, "<br>PM2.5:", PM2.5),
  hoverinfo = "text"
) %>%
  layout(
    title = "Interactieve Wereldkaart PM2.5 (gemiddelde 2014–2025)",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(230, 230, 230)",
      showcountries = TRUE,
      countrycolor = "rgb(200, 200, 200)"
    )
  )

fig
