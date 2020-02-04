#### Maps #### 


##### Maps of Africa
africa <- map(
  database = "world",
  regions = c("Nigeria", "Cameroon", "Central African Republic", "Chad"),
  fill = T,
  plot = F
)

africa_fill <- map2SpatialPolygons(
  africa,
  IDs = africa$names,
  proj4string = CRS(
    "+proj=utm +zone=30"
  ) 
)

world_ref1 <- map(
  database = "world",
  fill = T,
  plot = F
)

world_ref1 <- map2SpatialPolygons(
  world_ref1,
  IDs = world_ref1$names,
  proj4string = CRS(
    "+proj=utm +zone=30"
  ) 
)


##### World Reference Map 

ggplot(
  world_ref1, 
  aes(
    x = long, 
    y = lat, 
    group = group
  )
) +
  geom_polygon(
    fill="lightgreen", 
    colour = "grey"
  ) + 
  coord_equal(
    xlim = c(-25, 60),
    ylim = c(-35, 45)
  ) + 
  theme_map(
    
  ) + 
  theme(
    plot.background = element_rect(
      fill = 'skyblue'
    )
  ) + 
  annotate(
    "rect", 
    xmin = min(loa$lon) - correction,
    xmax = max(loa$lon) + correction, 
    ymin = min(loa$lat) - correction,
    ymax = max(loa$lat) + correction,
    alpha = .25,
    fill = "red"
  )


names <- data.frame(
  country = c("Nigeria", "Cameroon", "Central African Republic", "Chad"),
  lat = c(7.602539, 10.070801, 15.395507, 15.820313),
  long = c(5.207658, 4.477856, 5.047171, 8.566537)
)

##### Central Africa Reference map 
ggplot(
  world_ref1, 
  aes(
    x = long, 
    y = lat, 
    group = group
  )
) +
  geom_polygon(
    fill="lightgreen", 
    colour = "grey"
  ) + 
  coord_equal(
    xlim = c(min(loa$lon) - 4, max(loa$lon)) + 2,
    ylim = c(min(loa$lat) - 2, max(loa$lat) + 2)
  ) + 
  theme_map(
    
  ) + 
  theme(
    plot.background = element_rect(
      fill = 'skyblue'
    )
  ) + 
  annotate(
    "rect", 
    xmin = min(loa$lon) - correction,
    xmax = max(loa$lon) + correction, 
    ymin = min(loa$lat) - correction,
    ymax = max(loa$lat) + correction,
    alpha = .05,
    fill = "red"
  ) + 
  annotate(
    "text", 
    label = c("Nigeria", "Cameroon", (expression(atop("Central", paste("African Republic")))), "Chad"),
    x = c(7.602539, 11.370801, 16.2, 16.4),
    y = c(8, 5.2, 5.8, 8.566537),
    size = 5
  ) + 
  annotate(
    "text",
    label = c("Yaounde", "Douala"),
    x = c(11.501346, 9.786072) + .5,
    y = c(3.844119, 4.061536) - .15
  ) + 
  annotate(
    "point",
    color = "black",
    x = 9.786072,
    y = 4.061536
  ) + 
  annotate(
    "text",
    label = "*",
    x = 11.501346,
    y = 3.844119,
    size = 8.5
  )
  

##### L. Loa Prevalence Map

tm_shape(
  africa_fill,
  ylim = c(
    min_lat - correction, 
    max_lat + correction
  ),
  xlim = c(
    min_lon - correction, 
    max_lon +  correction
  )
) + 
  tm_fill(
    col = "lightgreen"
  ) + 
  tm_borders(
    lwd = .5,
    col = "grey"
  ) + 
  tm_shape(
    loa_sf
  ) + 
  tm_symbols(
    col = "perc_pos",
    title.col = "Prevalence (%)",
    palette = "-RdYlBu",
    n = 6,
    style = "jenks",
    border.lwd = 0.1,
    border.col = 'gray',
    alpha = 0.9,
    jitter = 0.1,
    scale = 0.85
  )  +
  tm_layout(
    bg.color = "skyblue",
    saturation = 1,
    legend.position = c("right", "center")
  ) + 
  tm_legend(
    legend.outside = TRUE,
    frame = F
  )


##### OLS Residual Map 
tm_shape(
  africa_fill,
  ylim = c(
    min_lat - correction, 
    max_lat + correction
  ),
  xlim = c(
    min_lon - correction, 
    max_lon +  correction
  )
) + 
  tm_fill(
    col = "lightgreen"
  ) + 
  tm_borders(
    lwd = .5,
    col = "grey"
  ) + 
  tm_shape(
    loa_sf
  ) + 
  tm_symbols(
    col = "resid",
    title.col = "Residual OLS",
    palette = "-RdYlBu",
    n = 6,
    style = "jenks",
    border.lwd = 0.1,
    border.col = 'gray',
    alpha = 0.9,
    jitter = 0.1,
    scale = 0.85,
    midpoint = 0
  ) +
  tm_layout(
    bg.color = "skyblue",
    saturation = 1,
    legend.position = c("right", "center")
  )  + 
  tm_legend(
    legend.outside = TRUE,
    frame = F
  ) 

### Latent Spatial Effects 

latent <- ranef(spamm_fit)[[1]]

loa$latent <- latent

loa_sp <- SpatialPoints(cbind(loa$lon, loa$lat), proj4string = CRS("+proj=longlat"))

loa_sp <- spTransform(loa_sp, CRS("+init=epsg:26391"))    # Needed for maps

loa_sf <- st_as_sf(
  loa,
  coords = c("lon", "lat"),
  crs = CRS("+proj=utm +zone=30")
)


tm_shape(
  africa_fill,
  ylim = c(
    min_lat - correction, 
    max_lat + correction
  ),
  xlim = c(
    min_lon - correction, 
    max_lon +  correction
  )
) + 
  tm_fill(
    col = "lightgreen"
  ) + 
  tm_borders(
    lwd = .5,
    col = "grey"
  ) + 
  tm_shape(
    loa_sf
  ) + 
  tm_symbols(
    col = "latent",
    title.col = "Latent Effects",
    palette = "BrBG",
    n = 6,
    style = "jenks",
    border.lwd = 0.1,
    border.col = 'gray',
    alpha = 0.9,
    jitter = 0.1,
    scale = 0.85,
    midpoint = 0
  ) +
  tm_layout(
    bg.color = "skyblue",
    saturation = 1,
    legend.position = c("right", "center")
  )  + 
  tm_legend(
    legend.outside = TRUE,
    frame = F
  ) 

###### End of Maps ###### 