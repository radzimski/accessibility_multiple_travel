library(dplyr)
library(sf)
library(tmap)

gdansk <- st_read("./results/gdansk/accessibility_results.gpkg")
krakow <- st_read("./results/krakow/accessibility_results.gpkg")
poznan <- st_read("./results/poznan/accessibility_results.gpkg")
wroclaw <- st_read("./results/wroclaw/accessibility_results.gpkg")

acc_combined <- rbind(gdansk, krakow, poznan, wroclaw)
acc_combined$combined_class <- factor(acc_combined$combined_class, levels = c("LL", "LM", "LH", "HL", "HM", "HH"))
acc_combined$cityname <- factor(acc_combined$cityname, levels = c("gdansk", "krakow", "poznan", "wroclaw"))
acc_combined$cityname <- recode(acc_combined$cityname, gdansk = "Gdańsk", krakow = "Kraków", poznan = "Poznań", wroclaw = "Wrocław")

#Neighborhood type map
tm_shape(acc_combined) + 
  tm_polygons(fill = "combined_class", title = "Neighbourhood \ntype\n", textNA = "Not inhabited", palette = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") + 
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/neighbourhod_type_map.png", width = 24, height = 20, units = "cm")

#Accessibility to schools map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_schools", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_schools_map.png", width = 24, height = 20, units = "cm")

#Accessibility to healthcare map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_healthcare", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_healthcare_map.png", width = 24, height = 20, units = "cm")

#Accessibility to jobs map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_jobs", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_jobs_map.png", width = 24, height = 20, units = "cm")

#Accessibility to other opportunities map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_other", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_other_map.png", width = 24, height = 20, units = "cm")
