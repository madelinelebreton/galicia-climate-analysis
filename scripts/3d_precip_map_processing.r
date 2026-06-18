library(terra)

# Load — inspect it first
r <- rast("C:\\Users\\Equipo\\Downloads\\descarga_clima\\AEMET_precip_avg.tif")
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



