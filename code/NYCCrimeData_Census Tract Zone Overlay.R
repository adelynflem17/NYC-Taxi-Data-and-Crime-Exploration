## Census Tract Zone Overlay

map_taxi_zones_by_borough <- function(zone_path, borough_path, tract_path, borough_name, clip = TRUE, label_areas = NULL) {
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  
  # Read and transform data
  zones <- st_read(zone_path) %>% st_transform(crs = 4326)
  boroughs <- st_read(borough_path) %>% st_transform(crs = 4326)
  tracts <- st_read(tract_path) %>% st_transform(crs = 4326)
  
  # Validate geometries
  zones <- st_make_valid(zones)
  tracts <- st_make_valid(tracts)
  
  # Filter for selected borough
  selected_borough <- boroughs %>% filter(BoroName == borough_name)
  zone_borough <- zones %>% filter(borough == borough_name)
  tract_col <- if ("boroname" %in% colnames(tracts)) "boroname" else "BoroName"
  tracts_borough <- tracts %>% filter(.data[[tract_col]] == borough_name)
  
  # Ensure CRS consistency before clipping
  selected_borough <- st_transform(selected_borough, st_crs(zone_borough))
  
  # Optional: spatial clip
  if (clip) {
    zone_borough <- st_intersection(zone_borough, selected_borough)
    tracts_borough <- st_intersection(tracts_borough, selected_borough)
  }
  
  # Fail-safe
  if (!exists("tracts_borough") || nrow(tracts_borough) == 0) {
    stop("❌ tracts_borough is missing or empty. Check column names and borough_name.")
  }
  
  # Prepare highlighted zones and labels
  if (!is.null(label_areas)) {
    highlighted_zones <- zone_borough %>% filter(zone %in% label_areas)
    highlighted_zones$centroid <- st_point_on_surface(highlighted_zones$geometry)
    coords <- st_coordinates(highlighted_zones$centroid)
    highlighted_zones$lon <- coords[,1]
    highlighted_zones$lat <- coords[,2]
    
    # Get bounding box of the selected borough
    bbox <- st_bbox(selected_borough)
    
    # Push labels just outside the right edge of the map
    highlighted_zones$label_x <- bbox["xmax"] + 0.01
    highlighted_zones$label_y <- highlighted_zones$lat
    
    # Optional: stagger vertically to avoid overlap
    highlighted_zones <- highlighted_zones %>%
      arrange(desc(lat)) %>%
      mutate(label_y = label_y + seq(-0.01, 0.01, length.out = n()))
  }
  
  # Plot
  p <- ggplot() +
    # Census tracts first so they appear beneath everything
    geom_sf(data = tracts_borough, fill = NA, color = "gray60", size = 0.3) +
    
    # Fill selected zones with light blue
    { if (!is.null(label_areas)) geom_sf(data = highlighted_zones, fill = "lightblue", color = NA) } +
    
    # overlay census tract outlines again so they appear on top of the blue
    geom_sf(data = tracts_borough, fill = NA, color = "gray60", size = 0.3) +
    
    # Remaining taxi zones
    geom_sf(data = zone_borough, fill = NA, color = "black", size = 0.4) +
    
    # Borough outline
    geom_sf(data = selected_borough, fill = NA, color = "black", size = 1) +
    
    theme_void() +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"] + 0.05), clip = "off") +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  
  # External labels and arrows
  if (!is.null(label_areas)) {
    p <- p +
      geom_segment(data = highlighted_zones,
                   aes(x = label_x, y = label_y, xend = lon, yend = lat),
                   arrow = arrow(length = unit(0.15, "cm")),
                   color = "blue", linewidth = 0.2) +
      geom_text(data = highlighted_zones,
                aes(x = label_x, y = label_y, label = zone),
                size = 2, fontface = "bold", hjust = 0, color = "blue")
  }
  
  return(p)
}




queens_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/taxi_zones/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/nybb_25b/nybb_25b/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/2020 Census Tracts_20250717/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Queens",
  clip = TRUE,
  label_areas = c("Astoria", "Steinway", "Jackson Heights", "Forest Hills", "Long Island City & Hunters Point")
)


manhattan_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/taxi_zones/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/nybb_25b/nybb_25b/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/2020 Census Tracts_20250717/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Manhattan",
  clip = TRUE,
  label_areas = c("East Harlem North", "East Harlem South", "Two Bridges/Seward Park", "West Chelsea/Hudson Yards")
)


StatenIsland_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/taxi_zones/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/nybb_25b/nybb_25b/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/2020 Census Tracts_20250717/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Staten Island",
  clip = TRUE,
  label_areas = c("Bloomfield/Emerson Hill", "Stapleton")
)


Brooklyn_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/taxi_zones/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/nybb_25b/nybb_25b/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/2020 Census Tracts_20250717/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Brooklyn",
  clip = TRUE,
  label_areas = c("Brooklyn Heights", "Greenpoint", "Park Slope", "Williamsburg (North Side)", "Williamsburg (South Side)")
)


Bronx_map <- map_taxi_zones_by_borough(
  zone_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/taxi_zones/taxi_zones.shp",
  borough_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/nybb_25b/nybb_25b/nybb.shp",
  tract_path = "C:/Users/Adelyn/Desktop/MS_Thesis_Final/2020 Census Tracts_20250717/geo_export_ee39abce-5726-4c3a-b4b1-fe4e5d1063cc.shp",
  borough_name = "Bronx",
  clip = TRUE,
  label_areas = c("East Concourse/Concourse Village", "Mott Haven/Port Morris", "Spuyten Duyvil/Kingsbridge")
)

plot(queens_map)