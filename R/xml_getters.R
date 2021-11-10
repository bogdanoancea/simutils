#' Get different elements from an xml file.
#'
#' @param xmlname character vector of length 1 with the name of the xml file
#'
#' @param dataset character vector of length 1 with the dataset name
#' 
#' @import sf xml2 data.table
#' 
#' @include readWKT_as_sf.R
#' 
#' @examples
#' xmlname <- system.file(
#'   "extdata/metadata/output_files/antennas_dict.xml", package = "simutils") 
#' getCoordsNames(xmlname, 'antennas')
#' 
#' xmlname <- system.file(
#'   "extdata/metadata/output_files/persons_dict.xml", package = "simutils") 
#' getCoordsNames(xmlname, 'individuals')
#' 
#' xmlname <- system.file(
#'   "extdata/metadata/output_files/grid_dict.xml", package = "simutils") 
#' getGridNoTilesX(xmlname, 'grid')
#' getGridNoTilesY(xmlname, 'grid')
#' 
#' @rdname xml_getters
#' 
#' @export
getCoordsNames <- function(xmlname, dataset) {
  if (dataset == 'antennas') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$antennas
    #get column names and column types
    antennas_coords_colNames <- c()

    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_antenna_coords') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'ColName')) {
            coordsColName <- (ant[[i]])[[j]][[1]]
            antennas_coords_colNames <- c(antennas_coords_colNames, coordsColName)
          }
        }
      }
    }
    #out <- rbindlist(lapply(x, as.data.table), fill = TRUE)
    return(antennas_coords_colNames)
  }
  
  if (dataset == 'individuals') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$persons
    #get column names and column types
    persons_coords_colNames <- c()
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_person_coords') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'ColName')) {
            coordsColName <- (ant[[i]])[[j]][[1]]
            persons_coords_colNames <- c(persons_coords_colNames, coordsColName)
          }
        }
      }
    }
    return(persons_coords_colNames)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getGridNoTilesX <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$grid
    #get column names and column types
    XColName <- ''
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_grid_tile_no') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'XColName')) {
            XColName <- (ant[[i]])[[j]][[1]]
          }
        }
      }
    }
    return(XColName)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getGridNoTilesY <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$grid
    #get column names and column types
    YColName <- ''
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_grid_tile_no') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'YColName')) {
            YColName <- (ant[[i]])[[j]][[1]]
          }
        }
      }
    }
    return(YColName)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getSignalType <- function(xmlname, dataset) {
  if (dataset == 'signal') {
    xml <- read_xml(xmlname)
    signal_type <- xml_attr(xml_child(xml, 'specs_signal'), attr = 'type')
    return(signal_type)
  } 
  stop('[simutils::getSignalType] dataset not yet implemented.\n')
}


#' @rdname xml_getters
#' 
#' @export
getXTileDimColName <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    XTileDimColName <- xml_text(xml_child(xml_child(xml, 'specs_grid_tile_dim'), 'XTileDimColName'))
    return(XTileDimColName)
  } 
  stop('[simutils::getSignalType] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getYTileDimColName <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    YTileDimColName <- xml_text(xml_child(xml_child(xml, 'specs_grid_tile_dim'), 'YTileDimColName'))
    return(YTileDimColName)
  } 
  stop('[simutils::getSignalType] dataset not yet implemented.\n')
}


#' @rdname xml_getters
#' 
#' @export
getCellCoordName <- function(xmlname, dataset) {
  if (dataset == 'cells') {
    xml <- read_xml(xmlname)
    cellCoordName <- xml_text(xml_find_all(xml, './/cellCoordsColName'))
    return(cellCoordName)
  } 
  stop('[simutils::getCellCoordName] dataset not yet implemented.\n')
}
#' @rdname xml_getters
#' 
#' @export
getCellIDName <- function(xmlname, dataset) {
  if (dataset == 'cells') {
    xml <- read_xml(xmlname)
    cellIDName <- xml_text(xml_find_all(xml, './/cellIDColName'))
    return(cellIDName)
  } 
  stop('[simutils::getCellIDName] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getSpatialUnitName <- function(xmlname, dataset) {
  if (dataset == 'map') {
    xml <- read_xml(xmlname)
    SpatialUnitName <- xml_text(xml_find_all(xml, './/name_long'))
    if (length(unique(SpatialUnitName)) > 1) {
      
      stop('[simutils::getSpatialUnitName] Multiple spatial units not allowed.\n')
      
    }
    SpatialUnitName <- paste0(unique(SpatialUnitName), '_long')
    return(SpatialUnitName)
  } 
  stop('[simutils::getSpatialUnitName] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getNestingSpatialUnitName <- function(xmlname, dataset) {
  if (dataset == 'map') {
    xml <- read_xml(xmlname)
    NestingSpatialUnitName <- xml_text(xml_find_all(xml, './/nest_name_long'))
    
    NestingSpatialUnitName <- paste0(unique(NestingSpatialUnitName), '_long')
    return(NestingSpatialUnitName)
  } 
  stop('[simutils::getNestingSpatialUnitName] dataset not yet implemented.\n')
}

#' @rdname xml_getters
#' 
#' @export
getDeviceIDColName <- function(xmlname, dataset) {
  if (dataset == 'individuals') {
    xml <- read_xml(xmlname)
    deviceIDColName <- xml_text(xml_find_all(xml, './/devColName'))
    
    return(deviceIDColName)
  } 
  stop('[simutils::getNestingSpatialUnitName] dataset not yet implemented.\n')
}