#Calculate percentiles of accessibility and population

args = commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(tidyr)

city <- args[1]
dir.create("./results/")
dir.create(paste0("./results/", city))
filename <- paste0('./results/', city, '/accessibility_results.gpkg')

acc_combined <- st_read(filename)

acc_combined <- acc_combined %>% mutate_if(is.numeric, ~replace_na(., 0))

acc_combined$pct_schools <- ecdf(acc_combined$acc_schools)(acc_combined$acc_schools)
acc_combined$pct_healthcare <- ecdf(acc_combined$acc_healthcare)(acc_combined$acc_healthcare)
acc_combined$pct_jobs <- ecdf(acc_combined$acc_jobs)(acc_combined$acc_jobs)
acc_combined$pct_retail <- ecdf(acc_combined$acc_retail)(acc_combined$acc_retail)
acc_combined$pct_green <- ecdf(acc_combined$acc_green)(acc_combined$acc_green)

acc_combined$pct_res <- ecdf(acc_combined$res)(acc_combined$res)
acc_combined$pct_res_0_14 <- ecdf(acc_combined$res_0_14)(acc_combined$res_0_14)
acc_combined$res_15over <- acc_combined$res_15_64 + acc_combined$res_65_
acc_combined$pct_res_15over <- ecdf(acc_combined$res_15over)(acc_combined$res_15over)

acc_combined$diff_schools <- acc_combined$pct_schools - acc_combined$pct_res_0_14
acc_combined$diff_healthcare <- acc_combined$pct_healthcare - acc_combined$pct_res
acc_combined$diff_jobs <- acc_combined$pct_jobs - acc_combined$pct_res_15over
acc_combined$diff_retail <- acc_combined$pct_retail - acc_combined$pct_res
acc_combined$diff_green <- acc_combined$pct_green - acc_combined$pct_res

st_write(acc_combined, dsn = paste0("./results/", city, "/accessibility_results.gpkg"), append = FALSE)
