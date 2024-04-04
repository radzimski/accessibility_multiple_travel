library(dplyr)
library(ggplot2)
library(sf)
library(sfdep)
library(tmap)

gdansk <- st_read("./results/gdansk/accessibility_results.gpkg")
krakow <- st_read("./results/krakow/accessibility_results.gpkg")
poznan <- st_read("./results/poznan/accessibility_results.gpkg")
wroclaw <- st_read("./results/wroclaw/accessibility_results.gpkg")

acc_combined <- rbind(gdansk, krakow, poznan, wroclaw)
acc_combined$combined_class <- factor(acc_combined$combined_class, levels = c("LL", "LM", "LH", "HL", "HM", "HH"))
acc_combined$cityname <- factor(acc_combined$cityname, levels = c("gdansk", "krakow", "poznan", "wroclaw"))
acc_combined$cityname <- recode(acc_combined$cityname, gdansk = "Gdańsk", krakow = "Kraków", poznan = "Poznań", wroclaw = "Wrocław")
acc_combined$vulnerability <- factor(acc_combined$vulnerability, levels = c("High", "Moderate", "Low"))

#Demographic type map
tm_shape(acc_combined) + 
  tm_polygons(fill = "dep_type", title = "Demographic \ndependency\n", textNA = "Not inhabited", palette = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") + 
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/demographic_type_map.png", width = 24, height = 20, units = "cm")

#Income map
tm_shape(acc_combined) + 
  tm_polygons(fill = "income_class", title = "Income \nlevel\n", textNA = "Not inhabited", palette = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") + 
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/income_map.png", width = 24, height = 20, units = "cm")

#Vulnerability map
tm_shape(acc_combined) + 
  tm_polygons(fill = "vulnerability", title = "Socio-economic \nvulnerability\n", textNA = "Not inhabited", palette = c("#b2182b", "#ef8a62", "#fddbc7")) + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") + 
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/vulnerability_map.png", width = 24, height = 20, units = "cm")

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

#Accessibility to retail map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_retail", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_retail_map.png", width = 24, height = 20, units = "cm")

#Accessibility to urban green map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_green", title = "Accessibility \nscore\n", style = "quantile", palette = "-plasma") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_green_map.png", width = 24, height = 20, units = "cm")

#Accessibility to other opportunities map
tm_shape(acc_combined) + 
  tm_polygons(fill = "acc_other", title = "Accessibility \nscore\n", style = "quantile") + 
  tm_layout(legend.frame = FALSE) + 
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/acc_other_map.png", width = 24, height = 20, units = "cm")

#Local Moran maps & charts

neighbors <- st_contiguity(acc_combined, queen = T)
weights <- st_weights(neighbors)

acc_weighted <- acc_combined %>% 
  mutate(
    nb = st_contiguity(acc_combined),
    wt = st_weights(nb)
  )

#Schools
schools_moran <- acc_weighted %>% 
  mutate(moran = local_moran(acc_schools, nb, wt))

#Population by cluster and neighborhood type
schools_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), "Not significant")) %>% 
  subset(!is.na(vulnerability)) %>%
  filter(pysal == "Low-Low" | pysal == "High-High" | pysal ==  "Not significant") %>% 
  ggplot(aes(x = cityname, y = res_0_14, fill = vulnerability)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~pysal) +
  scale_fill_manual(name = "Neighbourhood \ntype", values = c("#b2182b", "#ef8a62", "#fddbc7")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Share of population") +
  theme_minimal()

ggsave(filename = "./figures/schools_by_cluster.png", width = 24, height = 20, units = "cm", bg = "white")

#Cluster map
schools_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |>
  tm_shape() +
  tm_polygons(fill = "pysal", title = "Local Moran's I \nclusters\n", textNA = "Not significant", palette = c("#b2182b", "#fddbc7", "#d1e5f0", "#2166ac")) +
  tm_layout(legend.frame = FALSE) +
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/schools_moran.jpg", width = 24, height = 20, units = "cm")

#Jobs
jobs_moran <- acc_weighted %>% 
  mutate(moran = local_moran(acc_jobs, nb, wt))

#Population by cluster and neighborhood type
jobs_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), "Not significant")) %>% 
  subset(!is.na(vulnerability)) %>%
  filter(pysal == "Low-Low" | pysal == "High-High" | pysal == "Not significant") %>% 
  ggplot(aes(x = cityname, y = res_15_64 + res_65_, fill = vulnerability)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~pysal) +
  scale_fill_manual(name = "Socio-economic \nvulnerability", values = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Share of population") +
  theme_minimal()

ggsave(filename = "./figures/jobs_by_cluster.png", width = 24, height = 20, units = "cm", bg = "white")

#Cluster map
jobs_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |>
  tm_shape() +
  tm_polygons(fill = "pysal", title = "Local Moran's I \nclusters\n", textNA = "Not significant", palette = c("#b2182b", "#fddbc7", "#d1e5f0", "#2166ac")) +
  tm_layout(legend.frame = FALSE) +
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/jobs_moran.jpg", width = 24, height = 20, units = "cm")

#Healthcare
healthcare_moran <- acc_weighted %>% 
  mutate(moran = local_moran(acc_healthcare, nb, wt))

#Population by cluster and neighborhood type
healthcare_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), "Not significant")) %>% 
  subset(!is.na(vulnerability)) %>%
  filter(pysal == "Low-Low" | pysal == "High-High" | pysal == "Not significant") %>% 
  ggplot(aes(x = cityname, y = res, fill = vulnerability)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~pysal) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(name = "Socio-economic \nvulnerability", values = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Share of population") +
  theme_minimal()

ggsave(filename = "./figures/healthcare_by_cluster.png", width = 24, height = 20, units = "cm", bg = "white")

#Cluster map
healthcare_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |>
  tm_shape() +
  tm_polygons(fill = "pysal", title = "Local Moran's I \nclusters\n", textNA = "Not significant", palette = c("#b2182b", "#fddbc7", "#d1e5f0", "#2166ac")) +
  tm_layout(legend.frame = FALSE) +
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/healthcare_moran.jpg", width = 24, height = 20, units = "cm")

#Retail
retail_moran <- acc_weighted %>% 
  mutate(moran = local_moran(acc_retail, nb, wt))

#Population by cluster and neighborhood type
retail_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), "Not significant")) %>% 
  subset(!is.na(vulnerability)) %>%
  filter(pysal == "Low-Low" | pysal == "High-High" | pysal == "Not significant") %>% 
  ggplot(aes(x = cityname, y = res, fill = vulnerability)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~pysal) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(name = "Socio-economic \nvulnerability", values = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Share of population") +
  theme_minimal()

ggsave(filename = "./figures/retail_by_cluster.png", width = 24, height = 20, units = "cm", bg = "white")

#Cluster map
green_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |>
  tm_shape() +
  tm_polygons(fill = "pysal", title = "Local Moran's I \nclusters\n", textNA = "Not significant", palette = c("#b2182b", "#d1e5f0", "#2166ac")) +
  tm_layout(legend.frame = FALSE) +
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/retail_moran.jpg", width = 24, height = 20, units = "cm")

#Urban green
green_moran <- acc_weighted %>% 
  mutate(moran = local_moran(acc_green, nb, wt))

#Population by cluster and neighborhood type
green_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), "Not significant")) %>% 
  subset(!is.na(vulnerability)) %>%
  filter(pysal == "Low-Low" | pysal == "High-High" | pysal == "Not significant") %>% 
  ggplot(aes(x = cityname, y = res, fill = vulnerability)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~pysal) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(name = "Socio-economic \nvulnerability", values = c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Share of population") +
  theme_minimal()

ggsave(filename = "./figures/green_by_cluster.png", width = 24, height = 20, units = "cm", bg = "white")

#Cluster map
green_moran %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |>
  tm_shape() +
  tm_polygons(fill = "pysal", title = "Local Moran's I \nclusters\n", textNA = "Not significant", palette = c("#b2182b", "#d1e5f0", "#2166ac")) +
  tm_layout(legend.frame = FALSE) +
  tm_facets(by = "cityname") +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(filename = "./figures/green_moran.jpg", width = 24, height = 20, units = "cm")
