library(terra)

# Load — inspect it first
r <- rast("C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\data\\processed\\AEMET_precip_avg.tif")
print(r)         # check CRS, resolution, extent, value range
plot(r)          # quick sanity check
res(r)         # check resolution — should be 0.01° (~1 km)

# Check what each band contains
names(r)        # may give descriptive names
print(r)        # shows all band metadata

# Extract band 3
r_precip <- r[[3]]

# Verify it looks right — values should be ~150 to 2000+ mm
plot(r_precip)

# Reproject to equal-area (better for Blender since it preserves shape)
r_proj <- project(r_precip, "EPSG:3035")  # LAEA Europe

# Normalize 0–1 for Blender displacement
mn <- minmax(r_proj)[1]
mx <- minmax(r_proj)[2]
r_norm <- (r_proj - mn) / (mx - mn)

# Write out the normalized version for Blender
writeRaster(r_norm, "spain_precip_blender.tif", overwrite=TRUE)

# Also write a non-normalized version for QGIS visualization
writeRaster(r_proj, "spain_precip_qgis.tif", overwrite=TRUE)



# ---------------------------------------------------------
# Create colorized precipitation raster (light grey → navy)
# ---------------------------------------------------------

# Normalize precipitation values to 0–1
r_scaled <- (r_proj - global(r_proj, "min", na.rm=TRUE)[1,1]) /
            (global(r_proj, "max", na.rm=TRUE)[1,1] -
             global(r_proj, "min", na.rm=TRUE)[1,1])

# Define endpoint colors
# Light grey (#E5E5E5)
grey <- c(229, 229, 229)

# Navy blue (#001F5B)
navy <- c(0, 31, 91)

# Interpolate RGB channels
r_red   <- grey[1] + (navy[1] - grey[1]) * r_scaled
r_green <- grey[2] + (navy[2] - grey[2]) * r_scaled
r_blue  <- grey[3] + (navy[3] - grey[3]) * r_scaled

# Combine into RGB raster
r_rgb <- c(r_red, r_green, r_blue)
names(r_rgb) <- c("red", "green", "blue")

# Convert to 8-bit values
r_rgb <- round(r_rgb)

# Export GeoTIFF
writeRaster(
  r_rgb,
  "spain_precip_coloured_2.tif",
  overwrite = TRUE,
  datatype = "INT1U"
)

# Quick preview
plotRGB(r_rgb)