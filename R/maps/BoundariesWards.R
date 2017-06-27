library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)

## download here:
## https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2015-/sp34-6z76/data

shp_ward <- readOGR(dsn = "data/BoundariesWardsUnzipped",
                    layer = "geo_export_80d799eb-95bc-4380-bdf8-f3621099ac6f",
                    stringsAsFactors = FALSE)
saveRDS(shp_ward, "data/BoundariesWards.Rds")

