# Simplification of big shp SHAPEFILEs

library(sf)
library(sp)

##### ROADS #####
# Create simplified shapefiles for more efficient use
tx_st_filename <- "EIA and DOE topo data/TX Roads/TxDOT_Roadways.shp"
sf.tx_roads <- st_read(dsn = paste0(path.data, tx_st_filename))
sf.tx_roads.sub <- sf.tx_roads[, c("OBJECTID", "RTE_PRFX", "RTE_NBR", "RTE_SFX", "FUNC_SYS", "ZOOM")]
rm(sf.tx_roads)

# Diagnostic  
scalar <- .0001
# sf.simple <- sf.tx_roads.sub[1, ]
# st_as_text(st_geometry(sf.simple))
# sf.simple <- st_simplify(x = sf.simple, preserveTopology = T, dTolerance = scalar)
# st_as_text(st_geometry(sf.simple))

# Reduce LINESTRING resolution
sf.simple <- st_simplify(x = sf.tx_roads.sub, preserveTopology = T, dTolerance = scalar)

# Subset roads to a certain "zoom" (traffic? corresponds to size somehow)
scalar <- 3
sf.simple <- sf.simple[sf.simple$ZOOM <= scalar, ]

# Write simplified .shp
st_write(obj = sf.simple, dsn = paste0(path.data, "EIA and DOE topo data/TX Roads/TxDOT_Roadways_SIMPLE.shp"))

##### LINES #####
tx_st_filename <- "EIA and DOE topo data/TX State/Texas_State_Boundary.shp"
sf.tx_st_bound <- st_read(dsn = paste0(path.data, tx_st_filename))
tx_st_filename = "EIA and DOE topo data/HIFLD/Electric_Power_Transmission_Lines.shp"
sf.tx_lines = st_read(dsn = paste0(path.data, tx_st_filename))

# Reduce dimensionality
sf.tx_lines <- sf.tx_lines[, c("OBJECTID", "TYPE", "STATUS", "VOLTAGE", "VOLT_CLASS",
                                         "VAL_METHOD", "SUB_1", "SUB_2")]

# Mask lines with TX state boundaries
scalar <- "IN SERVICE"
scalar1 <- 100
scalar2 <- 500
sf.tx_lines.plot <- st_intersection(sf.tx_lines[sf.tx_lines$VOLTAGE >= scalar1 & 
                                                  sf.tx_lines$VOLTAGE <= scalar2 &
                                                  sf.tx_lines$STATUS == scalar, ], 
                                    sf.tx_st_bound)
rm(sf.tx_lines)

# Reduce LINESTRING resolution
scalar <- .0001
sf.simple <- st_simplify(x = sf.tx_lines.plot, preserveTopology = T, dTolerance = scalar)
st_write(obj = sf.simple, dsn = paste0(path.data, "EIA and DOE topo data/HIFLD/Electric_Power_Transmission_Lines_TX_SIMPLE.shp"))
