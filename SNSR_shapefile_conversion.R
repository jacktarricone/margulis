library(raster)
library(sp)
library(rgdal)
Sys.which("gdal_polygonize.py")

DEM <-raster("/Volumes/jt/projects/margulis/static/rasters/SNSR_dem.tif")
DEM <-raster(xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
DEM
view(DEM)

SNSR_DEM<- raster("/Volumes/jt/projects/margulis/static/rasters/SNSR_dem.tif", xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42) #set lat lon cords
crs(SNSR_DEM) <- "+proj=leac +ellps=clrk66" 
image(SNSR_DEM) #print raster
SNAR_sp <- rasterToPolygons(SNSR_DEM)


e <- extent( c(4304916, 4305325, 365216, 365439) )
p <- as(e, 'SpatialPolygons')
crs(p) <- "xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42 +proj=leac +ellps=clrk66"
shapefile(p, 'file.shp')

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile', 
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.") 
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.asc')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', 
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp) 
  }
  return(NULL)
}

gdal_polygonizeR(SNSR_DEM)
