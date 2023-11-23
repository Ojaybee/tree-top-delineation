tool_exec <- function(in_params, out_params) {
  arc.progress_label("Checking and loading packages...")
  arc.progress_pos(10)

  # Check and install 'sf' if necessary
  if (!require(sf, quietly = TRUE)) {
    install.packages("sf")
    library(sf)
  }

  # Check and install 'ForestTools' if necessary
  if (!require(ForestTools, quietly = TRUE)) {
    install.packages("ForestTools")
    library(ForestTools)
  }

  library(raster)

  arc.progress_label("Reading Data...")
  arc.progress_pos(40)

  # Tree Tops
  treetop_path <- out_params[[1]]
  # Ensure the path ends with '.shp'
  if (substr(treetop_path, nchar(treetop_path)-3, nchar(treetop_path)) != ".shp") {
    treetop_path <- paste0(treetop_path, ".shp")
  }

  input_raster <- in_params[[1]]
  canopy <- arc.data2sp(arc.raster(arc.open(input_raster)))
  canopyht <- raster(canopy)

  arc.progress_label("Generating Tree Tops...")
  arc.progress_pos(60)
  lin <- function(x) { x * .25 + 3.5 }
  minht <- in_params[[2]]
  ttops <- vwf(CHM = canopyht, minHeight = minht, minWinNeib = "queen", winFun = lin)

  # Create sf object for Tree Tops and write to shapefile
  if (!file.exists(treetop_path)) {
    treetop_sf <- st_as_sf(ttops, coords = c("x", "y"), crs = st_crs(canopy))
    st_write(treetop_sf, treetop_path)
  }

  arc.progress_label("Generating Crown Polygons...")
  arc.progress_pos(80)

  # Crowns
  crownpoly_path <- out_params[[2]]
  # Ensure the path ends with '.shp'
  if (substr(crownpoly_path, nchar(crownpoly_path)-3, nchar(crownpoly_path)) != ".shp") {
    crownpoly_path <- paste0(crownpoly_path, ".shp")
  }
  crowns <- mcws(treetops = ttops, format = "polygons", CHM = canopyht, minHeight = 10)

  # Create sf object for Crown Polygons and write to shapefile
  if (!file.exists(crownpoly_path)) {
    crownpoly_sf <- st_as_sf(crowns, crs = st_crs(canopy))
    st_write(crownpoly_sf, crownpoly_path)
  }

  return(out_params)
}