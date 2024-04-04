#Add urban green category

args = commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)

city <- args[1]
filename <- paste('./destinations/', city, '.gpkg', sep = '')
dir.create("./results/")
dir.create(paste0("./results/", city))

origin_points <- read.csv(paste0("./routing/", city, "/", city, "_origin.csv"))
acc_combined <- read.csv(file = paste0("./results/", city, "/acc_combined.csv"))

acc_combined$acc_green <- rowMeans(select(acc_combined,
                                            contains("park") |
                                            contains("las")))

write.csv(acc_combined, file = paste0("./results/", city, "/acc_combined.csv"), row.names = FALSE)

origin_acc_scores <- origin_points %>% left_join(acc_combined, by = "id")
write.csv(origin_acc_scores, file = paste0("./results/", city, "/acc_combined_coords.csv"))

grid <- st_read(dsn = "origin_grid.gpkg", layer = "full_grid", fid_column_name = "id")
grid <- subset(grid, cityname == city, select = c("ID_GRID", "res_0_14", "res_15_64", "res_65_"))
grid_acc_scores <- grid %>% left_join(origin_acc_scores, by = "ID_GRID")
st_write(grid_acc_scores, dsn = paste0("./results/", city, "/accessibility_results.gpkg"), append = FALSE)