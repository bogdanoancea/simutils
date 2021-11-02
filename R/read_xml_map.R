#' Read string in WKT format and convert it into an sf object.
#'
#' Read the input WKT string, parse it, and convert it into an sf object.
#'
#' @param text Character string of WKT
#'
#' @param id Character vector of unique ids to label geometries as in function
#' \code{rgeos::readWKT}. Length must match the number of subgeometries in the
#' WKT
#'
#' @param p4s Either a character string or an object of class \code{CRS} as in
#' the function \code{rgeos::readWKT}
#' 
#' @param ... passed on to \code{sf::st_as_sf}, might included named arguments
#'  crs or precision
#'
#' @rdname read_xml_map
#'
#' @name read_xml_map
#'
#' @import xml2 data.table
#'
#' @examples
#' read_xml_map(system.file(
#'    "extdata/input_files", "map.xml", package = "simutils"
#' ))
#' 
#' @export
read_xml_map <- function(xmlname){
  
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
    name_long <- unique(name_long)
    name_code <- xml_text(xml_find_all(xml_object, './/name_code'))
    name_value <- xml_text(xml_find_all(xml_object, './/name_value'))
    wkt.dt <- data.table(
      wkt = wkt,
      name_long = name_value,
      name_code = name_code
    )
    new_names <- gsub('name', name_long, names(wkt.dt))
    setnames(wkt.dt, new_names)

    nesting_units <- as_list(xml_find_all(xml_object, './/nesting_unit'))
    nesting_units <- lapply(nesting_units, function(nest_unit){
      
      dt <- data.table(
        name_long = nest_unit$nest_name_value[[1]],
        name_code = nest_unit$nest_name_code[[1]]
      )
      new_names <- gsub('name', nest_unit$nest_name_long[[1]], names(dt))
      setnames(dt, new_names)
      return(dt)
      
    })
    
    wkt.dt <- data.table(wkt.dt, rbindlist(nesting_units))
    wkt_dt_fn <- 'temp_wkt_dt.csv'
    fwrite(wkt.dt, wkt_dt_fn)
    map.sf <- read_sf(wkt_dt_fn, options = 'GEOM_POSSIBLE_NAMES=wkt')
    map.sf$wkt <- NULL
    file.remove(wkt_dt_fn)
  }
  return(map.sf)
}
