#' @title Read and parse xml file with map specifications.
#'
#' @description Read and parse the input xml file to return an sf object.
#'
#' @param xmlname Name of the xml file with the map specifications.
#' 
#' @param crs integer or character; coordinate reference system for the geometry
#'  as in function \code{sf::st_as_sfc}
#'
#' @details Return an sf object with the geometry column parsed from the
#' input xml file.
#'
#' @rdname read_xml_map
#'
#' @name read_xml_map
#'
#' @import xml2 data.table
#'
#' @examples
#' read_xml_map(
#'   system.file("extdata/input_files", "map.xml", package = "simutils"),
#'   crs = 2062)
#' 
#' @export
read_xml_map <- function(xmlname, crs){
  
  xml_object <- read_xml(xmlname)
  sp_syntax <- unique(xml_attr(xml_find_all(xml_object, './/sp_spec'), 'syntax'))
  if (length(sp_syntax) > 1) {
    
    stop('[simutils::read_xml_map] Multiple spatial syntaxes are not supported.\n')
    
  }
  
  if (sp_syntax == 'WKT') {
    
    wkt <- xml_text(xml_find_all(xml_object, './/sp_spec'))
    name_long <- xml_text(xml_find_all(xml_object, './/name_long'))
    if(length(unique(name_long)) > 1) {
      
      stop('[simutiles::read_xml_map] All spatial units must have the same name_long (region, province, subregion,...).\n')
      
    }
    
    regions <- xml_find_all(xml_object, ".//region")
    no_regions <- length(regions)
    
    names<-c("Subregion_long", "Subregion_code", "Region_long", "Region_code", "geometry")
    map.dt <- setNames(data.table(matrix(nrow = 0, ncol = 5)), names)
    
    for (i in 1:no_regions) {
      spatial_units <- xml_find_all(regions[i], './/spatial_unit')
      region_name <- xml_text(xml_find_first(regions[i], './/name'))
      region_code <- xml_text(xml_find_first(regions[i], './/code'))
      no_sp_units <-  length(spatial_units)
      for(j in 1:no_sp_units) {
        wkt <- xml_text(xml_find_first(spatial_units[j], './/sp_spec'))
        sr_long_name <- xml_text(xml_find_first(spatial_units[j], './/name_value'))
        sr_code <- xml_text(xml_find_first(spatial_units[j], './/name_code'))
        row <- list(sr_long_name, sr_code, region_name, region_code, wkt)
        map.dt<-rbind(map.dt, row)
      }
    }
    map.sf <- st_as_sf(map.dt, wkt = 'geometry', crs = crs)
    
  }
  return(map.sf)
}