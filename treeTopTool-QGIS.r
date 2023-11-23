##foresttools=group
##canopy_height_model=raster
##min_height=number
##treetops= output vector
##crowns= output vector
##Tree crown delineation =name

library(ForestTools)

canopyht <- raster(canopy_height_model, layer=1)
>print(class(canopyht))

minht<-min_height
>print('Generating treetops')
lin <- function(x) { x * .05 + 3.5 }
treetops <- vwf(CHM = canopyht, minHeight = minht, minWinNeib = "queen", winFun = lin)

crowns <- mcws(treetops = treetops, format = "polygons", CHM = canopyht, minHeight = 10)
