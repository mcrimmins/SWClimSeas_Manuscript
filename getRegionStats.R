# extract descriptive stats for regions
# MAC 05/14/25

library(dplyr)
library(terra)
library(elevatr)
library(sf)

# load cluster map --- from moPrecipClustering.R
classMap<-rast("./data/classMap5.grd")
levels(classMap) <- data.frame(id=1:5, category=c("Colorado Plateau (CP)","Great Plains (GP)","Lower Gila/Colorado (LGC)","Upper Gila (UG)","Rio Grande (RG)"))

##### elevation ----
# Convert extent to sf polygon
# regions_ext <- as.polygons(ext(classMap), crs = crs(classMap))
# regions_sf <- st_as_sf(regions_ext)
# regions_sf <- st_transform(regions_sf, crs = 4326)
# elev_rast <- get_elev_raster(locations = regions_sf, z = 10, clip = "locations")
# # check values 
# summary(values(elev_rast))
# hist(values(elev_rast), breaks = 100, main = "Elevation Histogram", xlab = "Elevation (m)")
# elev_rast[elev_rast < 0] <- NA

# check 0 values
# Copy raster
#  elev_zero_only <- elev_rast
# Set non-zero values to NA
#  elev_zero_only[elev_rast != 0] <- NA
# Plot
#  plot(elev_zero_only, main = "Elevation = 0", col = "blue", legend = FALSE)


# # save elev_rast
# writeRaster(elev_rast, filename = "./data/elev_rast.grd", overwrite = TRUE)
#####

# resample classMap
# load elevation if needed
elev_rast <- rast("./data/elev_rast.grd")

# resample classmap to lc
classMap <- resample(classMap, elev_rast, method = "near")

# mask elev_rast with classMap
elev_rast <- mask(elev_rast, classMap)

##### get stats ----
# load land cover -- https://www.mrlc.gov/
lc <- rast("./data/Annual_NLCD_LndCov_2023_CU_C1V0.tif")
# reproject to match classMap
lc <- project(lc, classMap, method = "near")
# crop to classMap
lc <- crop(lc, classMap)

# Create the NLCD class table with correct column name 'ID'
nlcd_levels <- data.frame(
  ID = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
  class = c("Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
            "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
            "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
            "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops",
            "Woody Wetlands", "Emergent Herbaceous Wetlands")
)

# Reconvert to categorical raster to ensure factor structure
lc <- as.factor(lc)

# Now assign levels *after* converting
levels(lc) <- list(nlcd_levels)


# resample lc to elev_rast
#lc <- resample(lc, elev_rast, method = "near")

# create stack
rstack <- c(classMap, elev_rast, lc)
names(rstack) <- c("region", "elev", "landcover")

# convert to df
df <- as.data.frame(rstack, xy = FALSE, na.rm = TRUE)

# elev summary
elev_summary <- df %>%
  group_by(region) %>%
  summarise(
    elev_min = min(elev, na.rm = TRUE),
    elev_max = max(elev, na.rm = TRUE),
    elev_mean = mean(elev, na.rm = TRUE),
    elev_sd = sd(elev, na.rm = TRUE)
  )

landcover_summary <- df %>%
  group_by(region, landcover) %>%
  tally(name = "count") %>%
  group_by(region) %>%
  mutate(
    total = sum(count),
    percent = 100 * count / total
  ) %>%
  slice_max(percent, n = 3) %>%
  rename(dominant_landcover = landcover) %>%
  select(region, dominant_landcover, percent)  # drop 'count' and 'total'

