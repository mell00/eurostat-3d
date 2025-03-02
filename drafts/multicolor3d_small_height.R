library(eurostat)
library(sf)
library(rgl)
library(dplyr)
library(viridis)  # For the multicolored gradient

# Ensure RStudio uses the Plots tab
options(rgl.useNULL = FALSE)

# Get Population Data (NUTS3 Level)
pop_data <- get_eurostat("demo_r_pjanaggr3", filters = list(age = "TOTAL", sex = "T")) %>%
  filter(time == max(time)) %>%
  select(geo, values)  # Keep only region codes and population values

# Get NUTS3 Shapefile
map_data <- get_eurostat_geospatial(nuts_level = "3", year = "2021", resolution = "20")

# Merge Data
map_data <- left_join(map_data, pop_data, by = c("geo" = "geo"))

# Normalize Population Data for 3D Height
map_data$height <- (map_data$values - min(map_data$values, na.rm = TRUE)) / 
  (max(map_data$values, na.rm = TRUE) - min(map_data$values, na.rm = TRUE)) * 10

# Ensure no NA values in height (replace with 0)
map_data$height[is.na(map_data$height)] <- 0  

# Generate a Multicolored Viridis Gradient (Light Yellow → Purple)
height_scaled <- map_data$height / max(map_data$height, na.rm = TRUE)  # Normalize (0 to 1)
height_scaled[is.na(height_scaled)] <- 0  # Replace NAs
color_map <- viridis(length(height_scaled), option = "magma", direction = -1)[rank(height_scaled, ties.method = "first")]

# Open 3D Device with Rotation Limits
open3d()
par3d(windowRect = c(50, 50, 1000, 800))  # Adjust window size
par3d(userMatrix = rotationMatrix(0, 1, 0, 0))  # Lock vertical flip
par3d(ignoreExtent = TRUE)  # Prevent flipping

#  Draw Region Boundaries in 3D (Base Layer)
for (i in 1:nrow(map_data)) {
  region_coords <- st_coordinates(st_cast(map_data$geometry[i], "MULTILINESTRING"))
  lines3d(region_coords[,1], region_coords[,2], rep(0, nrow(region_coords)), col = "black", lwd = 0.7)  # Base map
}

#  Extrude Each Region into a 3D Prism with Multicolor Gradient
for (i in 1:nrow(map_data)) {
  region_poly <- st_cast(map_data$geometry[i], "POLYGON")
  if (!is.null(region_poly)) {
    poly_coords <- st_coordinates(region_poly)  # Extract polygon coordinates
    
    # Base of the region at z=0
    polygon3d(poly_coords[,1], poly_coords[,2], rep(0, nrow(poly_coords)), col = color_map[i], alpha = 0.6)
    
    # Top of the extruded region
    polygon3d(poly_coords[,1], poly_coords[,2], rep(map_data$height[i], nrow(poly_coords)), col = color_map[i], alpha = 0.9)
    
    # Connect base and top to form a prism
    for (j in 1:(nrow(poly_coords) - 1)) {
      x_vals <- c(poly_coords[j,1], poly_coords[j+1,1], poly_coords[j+1,1], poly_coords[j,1])
      y_vals <- c(poly_coords[j,2], poly_coords[j+1,2], poly_coords[j+1,2], poly_coords[j,2])
      z_vals <- c(0, 0, map_data$height[i], map_data$height[i])
      quads3d(x_vals, y_vals, z_vals, col = color_map[i], alpha = 0.8)
    }
  }
}

#  Adjust View for Better Visualization
rgl.viewpoint(theta = 30, phi = 20, zoom = 0.8)  # Locks the default view
par3d(zoom = 0.9, userMatrix = rotationMatrix(0.5, 0, 0, 1))  # Prevents flipping upside down

#  Toggle Between 3D and Flat Mode
flatten_map <- function(flatten = FALSE) {
  if (flatten) {
    map_data$height <- rep(0, nrow(map_data))  # Flatten all heights
  }
  rgl.clear()
  
  # Re-draw the base outline
  for (i in 1:nrow(map_data)) {
    region_coords <- st_coordinates(st_cast(map_data$geometry[i], "MULTILINESTRING"))
    lines3d(region_coords[,1], region_coords[,2], rep(0, nrow(region_coords)), col = "black", lwd = 0.7)
  }
  
  # Re-draw extrusions with modified height
  for (i in 1:nrow(map_data)) {
    region_poly <- st_cast(map_data$geometry[i], "POLYGON")
    if (!is.null(region_poly)) {
      poly_coords <- st_coordinates(region_poly)
      
      polygon3d(poly_coords[,1], poly_coords[,2], rep(0, nrow(poly_coords)), col = color_map[i], alpha = 0.6)
      polygon3d(poly_coords[,1], poly_coords[,2], rep(map_data$height[i], nrow(poly_coords)), col = color_map[i], alpha = 0.9)
      
      for (j in 1:(nrow(poly_coords) - 1)) {
        x_vals <- c(poly_coords[j,1], poly_coords[j+1,1], poly_coords[j+1,1], poly_coords[j,1])
        y_vals <- c(poly_coords[j,2], poly_coords[j+1,2], poly_coords[j+1,2], poly_coords[j,2])
        z_vals <- c(0, 0, map_data$height[i], map_data$height[i])
        quads3d(x_vals, y_vals, z_vals, col = color_map[i], alpha = 0.8)
      }
    }
  }
}

# switch to a "flat map" mode
# flatten_map(flatten = TRUE)
