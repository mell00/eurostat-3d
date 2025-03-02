library(eurostat)
library(sf)
library(rgl)
library(dplyr)
library(viridis)

# RStudio uses the Plots tab
options(rgl.useNULL = FALSE)

# Define EU Regions
western_europe <- c("FR", "DE", "BE", "NL", "LU", "AT", "CH", "IE", "UK")
northern_europe <- c("SE", "NO", "DK", "FI", "IS")
southern_europe <- c("ES", "PT", "IT", "GR", "MT")
eastern_europe <- c("PL", "CZ", "SK", "HU", "RO", "BG", "EE", "LV", "LT", "UA", "BY", "MD")

# Get Population Data (NUTS3 Level)
pop_data <- get_eurostat("demo_r_pjanaggr3", filters = list(age = "TOTAL", sex = "T")) %>%
  filter(time == max(time)) %>%
  select(geo, values)  # only region codes and population values

# Get NUTS3 Shapefile
map_data <- get_eurostat_geospatial(nuts_level = "3", year = "2021", resolution = "60")

# Extract Country Codes
map_data$country <- substr(map_data$geo, 1, 2)

# Merge Data
map_data <- left_join(map_data, pop_data, by = c("geo" = "geo"))

# Normalize Population Data for 3D Height and Increase Scaling Factor by 10,000
height_scale_factor <- 250
map_data$height <- (map_data$values - min(map_data$values, na.rm = TRUE)) / 
  (max(map_data$values, na.rm = TRUE) - min(map_data$values, na.rm = TRUE)) * height_scale_factor

# Ensure no NA values in height (replace with 0)
map_data$height[is.na(map_data$height)] <- 0  

# Extract Centroids for Positioning
map_data <- map_data %>% mutate(centroid = st_centroid(geometry))

# Extract x (longitude) and y (latitude) from centroids
coords <- st_coordinates(map_data$centroid)
map_data$x <- coords[,1]  
map_data$y <- coords[,2]  

# Compute Map Centering
center_x <- mean(map_data$x, na.rm = TRUE)
center_y <- mean(map_data$y, na.rm = TRUE)

# Define Base Colors for Regions
map_data$region_color <- case_when(
  map_data$country %in% western_europe ~ "blue",
  map_data$country %in% northern_europe ~ "green",
  map_data$country %in% southern_europe ~ "red",
  map_data$country %in% eastern_europe ~ "purple",
  TRUE ~ "gray"
)

# Adjust Color Lightness Based on Population Density
height_scaled <- map_data$height / max(map_data$height, na.rm = TRUE)  # Normalize (0 to 1)
height_scaled[is.na(height_scaled)] <- 0  # Replace NAs

# Convert to RGB with Alpha for Transparency
color_map <- sapply(1:nrow(map_data), function(i) adjustcolor(map_data$region_color[i], alpha.f = height_scaled[i]))

# Open 3D Device with Rotation Limits
open3d()
par3d(windowRect = c(50, 50, 1200, 900))
par3d(userMatrix = rotationMatrix(0, 1, 0, 0))
par3d(ignoreExtent = TRUE)

# Center the Map
map_data$x <- map_data$x - center_x
map_data$y <- map_data$y - center_y

#  Draw Region Boundaries in 3D (Base Layer)
for (i in 1:nrow(map_data)) {
  region_coords <- st_coordinates(st_cast(map_data$geometry[i], "MULTILINESTRING"))
  lines3d(region_coords[,1] - center_x, region_coords[,2] - center_y, rep(0, nrow(region_coords)), col = "black", lwd = 0.7)  
}

# Extrude Each Region into a 3D Prism
for (i in 1:nrow(map_data)) {
  region_poly <- st_cast(map_data$geometry[i], "POLYGON")
  if (!is.null(region_poly)) {
    poly_coords <- st_coordinates(region_poly)  
    
    # Base of the region at z=0
    polygon3d(poly_coords[,1] - center_x, poly_coords[,2] - center_y, rep(0, nrow(poly_coords)), col = color_map[i], alpha = 0.6)
    
    # Top of the extruded region
    polygon3d(poly_coords[,1] - center_x, poly_coords[,2] - center_y, rep(map_data$height[i], nrow(poly_coords)), col = color_map[i], alpha = 0.9)
    
    # Connect base and top to form a prism
    for (j in 1:(nrow(poly_coords) - 1)) {
      x_vals <- c(poly_coords[j,1], poly_coords[j+1,1], poly_coords[j+1,1], poly_coords[j,1]) - center_x
      y_vals <- c(poly_coords[j,2], poly_coords[j+1,2], poly_coords[j+1,2], poly_coords[j,2]) - center_y
      z_vals <- c(0, 0, map_data$height[i], map_data$height[i])
      quads3d(x_vals, y_vals, z_vals, col = color_map[i], alpha = 0.8)
    }
  }
}

# Label Major Cities
major_cities <- data.frame(
  name = c("London", "Paris", "Berlin", "Madrid", "Rome", "Warsaw"),
  lon = c(-0.1278, 2.3522, 13.4050, -3.7038, 12.4964, 21.0122),
  lat = c(51.5074, 48.8566, 52.5200, 40.4168, 41.9028, 52.2298)
)

# Center City Coordinates
major_cities$lon <- major_cities$lon - center_x
major_cities$lat <- major_cities$lat - center_y

# Place Text Labels in 3D Space
text3d(major_cities$lon, major_cities$lat, 0, major_cities$name, adj = c(0.5, 0.5), cex = 1.2, col = "black")

# Adjust View for Better Visualization
rgl.viewpoint(theta = 30, phi = 20, zoom = 0.8)  
par3d(zoom = 1, userMatrix = rotationMatrix(0.5, 0, 0, 1))  
