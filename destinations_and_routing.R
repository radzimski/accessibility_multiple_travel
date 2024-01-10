args = commandArgs(trailingOnly = TRUE)

library(data.table)
library(dplyr)
library(r5r)
library(sf)
library(tidyr)

city <- args[1]
filename <- paste('./destinations/', city, '.gpkg', sep = '')
dir.create("./results/")
dir.create(paste0("./results/", city))

#Read & calculate the buildings layer
layer <- 'budynki'
dest <- st_read(dsn = filename, layer = layer)
dest_grid <- st_read(dsn = 'dest_grid.gpkg', layer = 'hex_250')
dest_grid <- subset(dest_grid, cityname == city)
dest_grid <- select(dest_grid, -c("left", "right", "top", "bottom", "NAZWA", "cityname"))

dest_list <- c("zlobek", 
               "przedszkole", 
               "szkolaPodstawowa", 
               "szkolaPonadpodstawowa",
               "szkolaWyzsza", 
               "apteka", 
               "placowkaOchronyZdrowia", 
               "szpital",
               "kino",
               "teatr",
               "muzeum",
               "biblioteka", 
               "domKultury", 
               "pawilonHandlowoUslugowy",
               "produkcyjny", 
               "siedzibaFirmyLubFirm",
               "domTowarowyLubHandlowy", 
               "hipermarketLubSupermarket", 
               "centrumHandlowe",
               "halaSportowa", 
               "plywalnia")

#Calculate no. of opportunities for the buildings layer
for (i in dest_list) {
  sel_dest <- subset(dest, grepl(i, dest$FUNSZCZ))
  add_col <- lengths(st_intersects(dest_grid, sel_dest))
  add_col[add_col>0] <- 1
  dest_grid[, ncol(dest_grid) +1] <- add_col
}

colnames(dest_grid) <- c("id", "geom", dest_list)

#Read & calculate the forests layer
layer <- 'lasy'
dest <- st_read(dsn = filename, layer = layer)
union <- st_union(dest)
intersect <- st_intersection(dest_grid, union)
intersect$las <- as.numeric(st_area(intersect))
intersect <- as.data.frame(intersect)
dest_grid <- left_join(dest_grid, intersect %>% dplyr::select(id, las), by = "id")
dest_grid$las[is.na(dest_grid$las)] <- 0


#Read & calculate the parks layer
layer <- 'parki'
dest <- st_read(dsn = filename, layer = layer)
union <- st_union(dest)
intersect <- st_intersection(dest_grid, union)
intersect$park <- as.numeric(st_area(intersect))
intersect <- as.data.frame(intersect)
dest_grid <- left_join(dest_grid, intersect %>% dplyr::select(id, park), by = "id")
dest_grid$park[is.na(dest_grid$park)] <- 0

#Grid to points & save to file
destinations <- st_centroid(dest_grid)
destinations <- st_transform(destinations, crs = 4326)
destinations <- cbind(destinations, st_coordinates(destinations))
destinations <- rename(destinations, lon=X, lat=Y)
destinations_df <- as.data.frame(destinations)
destinations_df <- select(destinations_df, -geom)
write.csv(destinations_df, "destination_points.csv", row.names = FALSE)

#Append destination list

dest_list <- append(dest_list, c("park", "las"))

#Read origin points layer
origin_points <- read.csv(paste0("./routing/", city, "/", city, "_origin.csv"))
#origin_points <-rename(origin_points, id = ID, lon=x, lat=y)

# Allocate memory to Java
options(java.parameters = "-Xmx16G")

# Build R5 graph object
r5r_core <- setup_r5(data_path = paste0("./routing/", city), elevation = "MINETTI", verbose = FALSE)

#Calculate 15 min walking accessibility
walking <- accessibility(r5r_core, 
                      origins = origin_points,
                      destinations = destinations_df,
                      opportunities_colnames = dest_list,
                      mode = "WALK",
                      walk_speed = 4.5,
                      decay_function = "step",
                      cutoffs = 16)

walking <- pivot_wider(walking, names_from = "opportunity", values_from = "accessibility")
write.csv(walking, file = paste0("./results/", city, "/walking_accessibility.csv"), row.names = FALSE)

#Calculate 15 min cycling accessibility using only safe routes (LTS 1 or 2)
cycling_safe <- accessibility(r5r_core, 
                         origins = origin_points,
                         destinations = destinations_df,
                         opportunities_colnames = dest_list,
                         mode = "BICYCLE",
                         max_lts = 2,
                         decay_function = "step",
                         cutoffs = 16)

cycling_safe <- pivot_wider(cycling_safe, names_from = "opportunity", values_from = "accessibility")
write.csv(cycling_safe, file = paste0("./results/", city, "/cycling_safe_accessibility.csv"), row.names = FALSE)

#Calculate 30 min cycling accessibility using all routes
cycling_all <- accessibility(r5r_core, 
                              origins = origin_points,
                              destinations = destinations_df,
                              opportunities_colnames = dest_list[!dest_list %in% c("zlobek", "przedszkole", "szkolaPodstawowa", "szkolaPonadpodstawowa")],
                              mode = "BICYCLE",
                              max_lts = 4,
                              decay_function = "step",
                              cutoffs = 31)

cycling_all <- pivot_wider(cycling_all, names_from = "opportunity", values_from = "accessibility")
write.csv(cycling_all, file = paste0("./results/", city, "/cycling_all_accessibility.csv"), row.names = FALSE)

#Calculate 30 min public transport peak accessibility
pt_peak <- accessibility(r5r_core, 
                         origins = origin_points,
                         destinations = destinations_df,
                         opportunities_colnames = dest_list,
                         mode = c("WALK", "TRANSIT"),
                         departure_datetime = as.POSIXct("17-11-2022 07:00:00", format = "%d-%m-%Y %H:%M:%S"),
                         decay_function = "step",
                         cutoffs = 31)

pt_peak <- pivot_wider(pt_peak, names_from = "opportunity", values_from = "accessibility")
write.csv(pt_peak, file = paste0("./results/", city, "/pt_peak_accessibility.csv"), row.names = FALSE)

#Calculate 45 min public transport off-peak accessibility
pt_off <- accessibility(r5r_core, 
                        origins = origin_points,
                        destinations = destinations_df,
                        opportunities_colnames = dest_list[dest_list %in% c("apteka", 
                                                                            "szpital", 
                                                                            "pawilonHandlowoUslugowy",
                                                                            "produkcyjny", 
                                                                            "siedzibaFirmyLubFirm",
                                                                            "domTowarowyLubHandlowy", 
                                                                            "hipermarketLubSupermarket", 
                                                                            "centrumHandlowe")],
                        mode = c("WALK", "TRANSIT"),
                        departure_datetime = as.POSIXct("17-11-2022 22:00:00", format = "%d-%m-%Y %H:%M:%S"),
                        decay_function = "step",
                        cutoffs = 46)

pt_off <- pivot_wider(pt_off, names_from = "opportunity", values_from = "accessibility")
write.csv(pt_off, file = paste0("./results/", city, "/pt_off_accessibility.csv"), row.names = FALSE)

#Calculate 45 min public transport accessibility on Saturday
pt_sat <- accessibility(r5r_core, 
                        origins = origin_points,
                        destinations = destinations_df,
                        opportunities_colnames = dest_list[!dest_list %in% c("zlobek", "przedszkole", "szkolaPodstawowa", "szkolaPonadpodstawowa")],
                        mode = c("WALK", "TRANSIT"),
                        departure_datetime = as.POSIXct("19-11-2022 13:00:00", format = "%d-%m-%Y %H:%M:%S"),
                        decay_function = "step",
                        cutoffs = 46)

pt_sat <- pivot_wider(pt_sat, names_from = "opportunity", values_from = "accessibility")
write.csv(pt_sat, file = paste0("./results/", city, "/pt_sat_accessibility.csv"), row.names = FALSE)

#Calculate standardized scores
walking_std <- as.data.table(cbind(as.numeric(walking$id), scale(walking[,4:ncol(walking)]))) 
cycling_safe_std <- as.data.table(cbind(as.numeric(cycling_safe$id), scale(cycling_safe[,4:ncol(cycling_safe)])))
cycling_all_std <- as.data.table(cbind(as.numeric(cycling_all$id), scale(cycling_all[,4:ncol(cycling_all)])))
pt_peak_std <- as.data.table(cbind(as.numeric(pt_peak$id), scale(pt_peak[,4:ncol(pt_peak)])))
pt_off_std <- as.data.table(cbind(as.numeric(pt_off$id), scale(pt_off[,4:ncol(pt_off)])))
pt_sat_std <- as.data.table(cbind(as.numeric(pt_sat$id), scale(pt_sat[,4:ncol(pt_sat)])))

names(walking_std) <- paste0("walking_", names(walking_std))
names(cycling_safe_std) <- paste0("cycling_safe_", names(cycling_safe_std))
names(cycling_all_std) <- paste0("cycling_all_", names(cycling_all_std))
names(pt_peak_std) <- paste0("pt_peak_", names(pt_peak_std))
names(pt_off_std) <- paste0("pt_off_", names(pt_off_std))
names(pt_sat_std) <- paste0("pt_sat_", names(pt_sat_std))

colnames(walking_std) [1] <- "id"
colnames(cycling_safe_std) [1] <- "id"
colnames(cycling_all_std) [1] <- "id"
colnames(pt_peak_std) [1] <- "id"
colnames(pt_off_std) [1] <- "id"
colnames(pt_sat_std) [1] <- "id"

# Combine results into one large table
acc_combined <- walking_std %>% 
  left_join(cycling_safe_std, by = "id") %>% 
  left_join(cycling_all_std, by = "id") %>% 
  left_join(pt_peak_std, by = "id") %>% 
  left_join(pt_off_std, by = "id") %>% 
  left_join(pt_sat_std, by = "id")
  
acc_combined$acc_schools <- rowMeans(select(acc_combined, contains("zlobek") | contains("przedszkole") | contains("szkolaPodstawowa") | contains("szkolaPonadpodstawowa")))
acc_combined$acc_jobs <- rowMeans(select(acc_combined, contains("produkcyjny") | contains("siedzibaFirmyLubFirm")))
acc_combined$acc_healthcare <- rowMeans(select(acc_combined, contains("apteka") | contains("placowkaOchronyZdrowia") | contains("szpital")))
acc_combined$acc_other <- rowMeans(select(acc_combined, contains("szkolaWyzsza") | 
                                            contains("kino") | 
                                            contains("teatr") | 
                                            contains("muzeum") | 
                                            contains("biblioteka") | 
                                            contains("domKultury") |
                                            contains("pawilonHandlowoUslugowy") |
                                            contains("domTowarowyLubHandlowy") |
                                            contains("hipermaketLubSupermarket") |
                                            contains("centrumHandlowe") |
                                            contains("halaSportowa") |
                                            contains("plywalnia")))

write.csv(acc_combined, file = paste0("./results/", city, "/acc_combined.csv"), row.names = FALSE)

# Merge with spatial coords
origin_acc_scores <- origin_points %>% left_join(acc_combined, by = "id")
write.csv(origin_acc_scores, file = paste0("./results/", city, "/acc_combined_coords.csv"))

grid <- st_read(dsn = "origin_grid.gpkg", layer = "full_grid", fid_column_name = "id")
grid <- subset(grid, cityname == city, select = c("ID_GRID", "res_0_14", "res_15_64", "res_65_"))
grid_acc_scores <- grid %>% left_join(origin_acc_scores, by = "ID_GRID")
st_write(grid_acc_scores, dsn = paste0("./results/", city, "/accessibility_results.gpkg"), append = FALSE)