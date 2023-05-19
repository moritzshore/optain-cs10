# Set input/output paths -------------------------------------------
#
# Project path (where output files are saved) ----------------------
project_path <- './output'
project_name <- 'cs10'

# Path of the TxtInOut folder (project folder where the SWAT+ text
# files are written with the SWAT+Editor)
txt_path <- './output/cs10/txt'
txt_path <- '../swat_runs/run3/'


# Input data -------------------------------------------------------
## DEM raster layer path
dem_path <- '../input_files/elevation/dtm3_ns_v5.tif'

##Soil raster layer and soil tables paths
soil_layer_path  <- '../input_files/soil/soil_layer.tif'
soil_lookup_path <- '../input_files/soil/soil_lookup.csv'
soil_data_path   <- '../input_files/soil/UserSoil_Krakstad.csv'

## Land input vector layer path
land_path <- '../input_files/landuse/cs10_lu_id_fixed.shp'

## Channel input vector layer path 
channel_path <- '../input_files/line/cs10_channels.shp'

## Catchment boundary vector layer path, all layers will be masked by the
## basin boundary
bound_path <- '../input_files/shape/cs10_basin.shp'

## Path to point source location layer
point_path <- '../input_files/point/cs10_pointsource.shp'

# Settings ---------------------------------------------------------
## Project layers
## The input layers might be in different coordinate reference systems (CRS). 
## It is recommended to project all layers to the same CRS and check them
## before using them as model inputs. The model setup process checks if 
## the layer CRSs differ from the one of the basin boundary. By setting 
## 'proj_layer <- TRUE' the layer is projected if the CRS is different.
## If FALSE different CRS trigger an error.
project_layer <- TRUE

## Set the outlet point of the catchment
## Either define a channel OR a reservoir as the final outlet
## If channel then assign id_cha_out with the respective id from the 
## channel layer:
id_cha_out <- 37

## If reservoir then assign the respective id from the land layer to
##  id_res_out, otherwise leave as set
id_res_out <- NULL

## Threshold to eliminate land object connectivities with flow fractions
## lower than 'frc_thres'. This is necessary to i) simplify the connectivity
## network, and ii) to reduce the risk of circuit routing between land 
## objects. Circuit routing will be checked. If an error due to circuit 
## routing is triggered, then 'frc_thres' must be increased to remove 
## connectivities that may cause this issue.
frc_thres <- 0.3

## Define wetland land uses. Default the wetland land uses from the SWAT+ 
## plants.plt data base are defined as wetland land uses. Wetland land uses
## should only be changed by the user if individual wetland land uses were 
## defined in the plant data base.
wetland_landuse <- c('wehb', 'wetf', 'wetl', 'wetn')

## Maximum distance of a point source to a channel or a reservoir to be included
## as a point source object (recall) in the model setup
max_point_dist <- 500 #meters