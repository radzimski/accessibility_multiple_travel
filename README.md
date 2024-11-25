Multimodal accessibility analysis using data from Polish Head Office of Geodesy and Cartography (BDOT10k). Instructions on how to access the dataset may be found here: https://www.geoportal.gov.pl/en/data/topographic-objects-database-bdot10k/

Scripts should be run in the following order:

1. make_origin_points.R
2. add_category.R
3. destinations_and_routing.R
4. add_percentiles.R
5. calculate_vulnerability.R
6. acc_maps_barplots.R

The "origin_grid.gpkg" file may be used to define origin points. It combines data on income and demographics drawn from the following sources:

- https://stat.gov.pl/statystyki-eksperymentalne/jakosc-zycia/stratyfikacja-dochodowa-mieszkancow-miast,5,1.html
- https://geo.stat.gov.pl/atom-web/download/?fileId=2795b7ed9098f86ab761352519ba1d85&name=GRID_NSP2021_RES.zip
