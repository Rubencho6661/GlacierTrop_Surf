# Install and load packages
install.packages("terra")
install.packages("raster")
install.packages("sf")
library(terra)
library(raster)
library(sf)
install.packages("remotes")
remotes::install_github("bleutner/RStoolbox", force = TRUE)

# Clear environment
rm(list = ls())

# Set working directory
setwd("D:/Imagenes_landsat/010060_1979")

# Import metadata
metaData <- RStoolbox::readMeta("LM02_L1TP_010060_19790204_20180418_01_T2_MTL.txt")
print(metaData)

# Create a RasterStack from the metadata
ls <- RStoolbox::stackMeta(metaData)
ls
# Define extent (adjust coordinates as needed)
# e <- terra::ext(740000, 810000, -40000, 30000)

# Crop Landsat data by the extent
# ls_crop <- terra::crop(ls, e)
# terra::plot(ls_crop)
terra::plot(ls)
# Perform DN to TOA reflectance conversion
# ls_crop_ref <- RStoolbox::radCor(ls_crop, metaData, method = "apref")
# terra::plot(ls_crop_ref)
ls_ref <- RStoolbox::radCor(ls, metaData, method = "apref")
terra::plot(ls_ref)

# Rename bands for Landsat 4
# names(ls_crop_ref) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2', 'TIRS')
# names(ls_crop_ref)
names(ls_ref) <- c('green', 'red', 'NIR', 'SWIR1')
names(ls_crop_ref)

# Export raster to folder
terra::writeRaster(ls_crop_ref, filename = "output/landsat_sck_crop_ref.tif", overwrite=TRUE)
# Scatterplot matrix to explore relationships between bands
terra::pairs(ls_crop[[1:6]], main = "Bands relationship")
# Stretch reflectance values
red_strch <- terra::stretch(ls_crop_ref$red, minq=0.02, maxq=0.9)
green_strch <- terra::stretch(ls_crop_ref$green, minq=0.02, maxq=0.9)
blue_strch <- terra::stretch(ls_crop_ref$blue, minq=0.02, maxq=0.9)

# Plot histograms
par(mfrow = c(1,3))
terra::hist(red_strch, main = "Red Band", col = "red", xlim = c(0, 255))
terra::hist(green_strch, main = "Green Band", col = "green", xlim = c(0, 255))
terra::hist(blue_strch, main = "Blue Band", col = "blue", xlim = c(0, 255))
# Create RGB composites
landsatRGB1 <- c(ls_crop_ref$red, ls_crop_ref$green, ls_crop_ref$blue)
landsatRGB2 <- c(red_strch, green_strch, blue_strch)

# Plot RGB composites
par(mfrow = c(1,2))
terra::plotRGB(landsatRGB1, axes = TRUE, main = "Landsat True Color Composite - No Stretch")
terra::plotRGB(landsatRGB2, axes = TRUE, main = "Landsat True Color Composite - Stretch")

# Export RGB composites
terra::writeRaster(landsatRGB1, filename = "output/landsat_crop_ref_RGB_nstch.tif", overwrite=TRUE)
terra::writeRaster(landsatRGB2, filename = "output/landsat_crop_ref_RGB_stch.tif", overwrite=TRUE)
# Perform unsupervised classification
set.seed(123)
unsup_class <- RStoolbox::unsuperClass(ls_crop_ref, nClasses = 2)
terra::plot(unsup_class$map, main = "Unsupervised Classification")

# Export classification result
terra::writeRaster(unsup_class$map, filename = "output/unsup_class.tif", overwrite=TRUE)
# Define NDSI function
ndsi <- function(img, green, swir) {
  bg <- img[[green]]
  bswir <- img[[swir]]
  ndsi <- (bg - bswir) / (bg + bswir)
  return(ndsi)
}

# Compute NDSI (Green = 2, SWIR = 5 for Landsat 4)
ls_crop_ref_ndsi <- ndsi(ls_crop_ref, 2, 5)
terra::plot(ls_crop_ref_ndsi, col = rev(terrain.colors(10)), main = "Landsat NDSI")

# Export NDSI raster
terra::writeRaster(ls_crop_ref_ndsi, filename = "output/ndsi.tif", overwrite=TRUE)
# Compute ratio bands (e.g., NIR/Red for vegetation)
nir_red_ratio <- ls_crop_ref$NIR / ls_crop_ref$red
terra::plot(nir_red_ratio, main = "NIR/Red Ratio")

# Export ratio band
terra::writeRaster(nir_red_ratio, filename = "output/nir_red_ratio.tif", overwrite=TRUE)