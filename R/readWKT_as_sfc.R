#' Read string in WKT format and convert it into an sf geometry list column.
#'
#' Read the input WKT string, parse it, and convert it into an sf geometry list 
#' column.
#'
#' @param x Character string of WKT
#' 
#' @param crs integer or character; coordinate reference system for the geometry
#'  as in function \code{sf::st_as_sfc}
#'  
#' @param ... further arguments
#' 
#' @param EWKB logical; if TRUE, parse as EWKB 
#' (extended WKB; PostGIS: ST_AsEWKB), otherwise as ISO WKB 
#' (PostGIS: ST_AsBinary)
#' 
#' @param spatialite logical; if TRUE, WKB is assumed to be in the spatialite 
#' dialect, see https://www.gaia-gis.it/gaia-sins/BLOB-Geometry.html; this is 
#' only supported in native endian-ness (i.e., files written on system with the 
#' same endian-ness as that on which it is being read).
#' 
#' @param pureR logical; if TRUE, use only R code, if FALSE, use compiled (C++) 
#' code; use TRUE when the endian-ness of the binary differs from the host 
#' machine (.Platform$endian).
#' 
#' @param GeoJSON logical; if TRUE, try to read geometries from GeoJSON text 
#' strings geometry, see st_crs()
#' 
#' @param precision precision value; see st_as_binary
#' 
#' @param forceMulti logical; if TRUE, force coercion into MULTIPOLYGON or 
#' MULTILINE objects, else autodetect
#'
#' @rdname readWKT_as_sfc
#'
#' @name readWKT_as_sfc
#'
#' @seealso \code{\link[sf]{st_as_sfc}}
#'
#' @examples
#' txt <- readLines(
#'    system.file("extdata/input_files", "map.wkt", package = "simutils"))
#' map_polygon_sfc <- readWKT_as_sfc(txt)
#' class(map_polygon_sfc)
#'
#' @export
readWKT_as_sfc <- function(x, ..., crs = NA_integer_, EWKB, spatialite, pureR, precision = 0, forceMulti = FALSE, GeoJSON){
  
  mc      <- match.call()
  mc[[1]] <- sf::st_as_sfc
  wkt.sfc <- eval(mc, parent.frame())
  return(wkt.sfc)
}
