args = commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)

city <- args[1]

#Read and cleanup origin grid

origin_grid <- st_read("./origin_grid.gpkg", layer = "origin_grid")
origin_grid <- subset(origin_grid, cityname == city)
col <- colnames(origin_grid)
col <- col[! col %in% c('geom')]
for (i in col) {
  origin_grid %>% mutate(origin_grid[[i]] == replace(origin_grid[[i]], origin_grid[[i]] == "NA", NA))
}
origin_grid$id <- seq(1,length(origin_grid$ID_GRID))
origin_grid$id <- as.character(origin_grid$id)

#Grid to points & save to file
origin_points <- st_centroid(origin_grid)
origin_points <- st_transform(origin_points, crs = 4236)
origin_points <- cbind(origin_points, st_coordinates(origin_points))
origin_points <-rename(origin_points, lon=X, lat=Y)
origin_points_df <- as.data.frame(origin_points)
origin_points_df <- subset(origin_points_df, select =-geom)
output_dir <- './routing'
output_dir <- paste(output_dir, city, sep = "/")
output <- paste(city, "origin.csv", sep ="_")
setwd(output_dir)
write.csv(origin_points_df, output, sep = ",", row.names = FALSE)

