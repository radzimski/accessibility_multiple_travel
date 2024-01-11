args = commandArgs(trailingOnly = TRUE)

city <- args[1]

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

# Read data and group by amenity type

acc_results <- st_read(paste0("./results/", city, "/accessibility_results.gpkg"), fid_column_name = "id")
acc_results <- as.data.table(acc_results)

acc_schools <- acc_results %>% subset(select = c("id", "ID_GRID", "res_0_14","income_class", "ineq_class", "combined_class", "acc_schools"))
acc_jobs <- acc_results %>% subset(select = c("id", "ID_GRID", "res_15_64","income_class", "ineq_class", "combined_class", "acc_jobs"))
acc_health <- acc_results %>% subset(select = c("id", "ID_GRID", "res","income_class", "ineq_class", "combined_class", "acc_healthcare"))
acc_other <- acc_results %>% subset(select = c("id", "ID_GRID", "res","income_class", "ineq_class", "combined_class", "acc_other"))

#Expand by number of residents
acc_schools_exp <- acc_schools %>% subset(res_0_14 >0) %>% uncount(res_0_14) %>% rename(acc = acc_schools) %>% mutate(type = "Schools")
acc_jobs_exp <- acc_jobs %>% subset(res_15_64 >0) %>% uncount(res_15_64) %>% rename(acc = acc_jobs) %>% mutate(type = "Jobs")
acc_health_exp <- acc_health %>% subset(res >0) %>% uncount(res) %>% rename(acc = acc_healthcare) %>% mutate(type = "Healthcare")
acc_other_exp <- acc_other %>% subset(res >0) %>% uncount(res) %>% rename(acc = acc_other) %>% mutate(type = "Other")

acc_summary <- rbind(acc_schools_exp, acc_jobs_exp, acc_health_exp, acc_other_exp)

ggplot(data = subset(acc_summary, income_class != "NA"), aes(x = type, y = acc, fill = combined_class)) + geom_boxplot()