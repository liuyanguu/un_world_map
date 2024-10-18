# some quick plot of the world shapefiles

library("ggplot2")
library("sf")

# working directory is the project folder
work.dir <- getwd()

map.shp.dir <- file.path(work.dir, "shp") # location of the shapefiles
map.rds.dir <- file.path(work.dir, "rds") # location to save the rds object


# plot the shp  -------------------------------------------------

shp_file_dir <- file.path(map.shp.dir, "un-world-2012-no-antartica-10pct.shp")
geo <- sf::st_read(shp_file_dir)

# choose projection: 
geo <- sf::st_transform(geo, crs = "+proj=longlat +datum=WGS84 +no_defs")  # convert to WGS84
geo <- sf::st_transform(geo, crs = "+proj=robin")  # convert to Robinson projection

g1 <- ggplot(data = geo) +
  geom_sf(fill = "grey", color = "#69b3a2") +
  theme_bw() +
  coord_sf()
g1
ggsave(plot = g1, file.path(work.dir, "fig/un-world-2012-no-antartica-10pct.png"), width = 20, height = 10, bg = "white")


# or use the rds
# Load the SpatialPolygonsDataFrame
world.robin <- readRDS(file.path(map.rds.dir, "sp.world.robin.rds"))

# Convert the SpatialPolygonsDataFrame to an sf object
world.robin.sf <- st_as_sf(world.robin)

# Plot with ggplot2 using geom_sf
g2 <- ggplot(data = world.robin.sf) +
  geom_sf(fill = "grey", color = "#69b3a2") +
  theme_bw() +
  coord_sf()

g2
