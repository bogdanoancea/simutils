#' @title Update simulation xml input files.
#' 
#' @description 
#' 
#' @param xmlSimInput
#' 
#' @param newParam
#' 
#' @param xsdName
#' 
#' @param newFile
#' 
#' @param configParamList
#' 
#' @details 
#' 
#' @include xmlValidate.R flatten_deepest.R list_deepest.R
#' 
#' @import xml2 
#' 
#' @examples 
#' config        <- list(
#'   Path = list(
#'     SIMULATOR_PATH_TO_EXE = file.path(system.file(package = "simutils"), "bin")))
#' sysinfo           <- Sys.info()
#' input_folder      <- file.path(system.file(package = "simutils", "extdata/input_files"))
#' simulationCFGFile <- "simulation.xml"
#' mapFile           <- "map.wkt"
#' personsCFGFile    <- "persons.xml"
#' antennasCFGFile   <- "antennas.xml"
#' outputFolder      <- Sys.getenv('R_USER')
#' runExeFile(
#'   configParamList   = config,
#'   sysinfo           = sysinfo,
#'   input_folder      = input_folder,
#'   simulationCFGFile = simulationCFGFile,
#'   mapFile           = mapFile,
#'   personsCFGFile    = personsCFGFile,
#'   antennasCFGFile   = antennasCFGFile,
#'   outputFolder      = outputFolder)
#'  
#' 
#' 
#' rootPath <- system.file(package = 'simutils')
#' xmlSimInput  <- file.path(rootPath, 'extdata/input_files', 'simulation.xml')
#' newParam.lst <- list(
#'  end_time = 11,
#'  movement_pattern = structure(list(
#'     trend_angle_1_distribution = structure(
#'         list(mean = 140, sd = 18),
#'         type = 'Normal')),
#'     type = 'random_walk_closed_map'))
#' xsdName  <- file.path(rootPath, 'extdata/metadata/input_files/schema_definition', 'simulation_dict.xsd')
#' newSimInputFile <- file.path(Sys.getenv('R_USER'), 'newSimulation.xml')
#' updateSimInput(
#' xmlSimInput = xmlSimInput, 
#' newParam = newParam.lst, 
#' xsdName  = xsdName,
#' newFile = newSimInputFile)
#' @export

updateSimInput <- function(xmlSimInput, newParam, xsdName, newFile = NULL, config_file_name = 'config.ini'){
  
  xmlSimInputName <- deparse(substitute(xmlSimInput))
  cat(paste0('[simutils::updateSimInput] Validating ', xmlSimInputName , '...   '))
  xmlValid <- xmlValidate(xsdName, xmlSimInput, config_file_name) 
  if (!xmlValid) {
    
    stop(paste0('[simutils::updateSimInput] The xml object ', xmlSimInputName, ' is not valid.\n'))
    
  }
  if (inherits(xmlSimInput, 'xml_document')) {

    xmlInput.lst <- flatten_deepest(as_list(xmlSimInput))
    
    
  }
  
  if (inherits(xmlSimInput, 'character')) {
    
    xmlInput.lst <- flatten_deepest(as_list(read_xml(xmlSimInput)))  
    
  }
  
  newxmlInput.lst <- list_deepest(purrr::list_modify(xmlInput.lst, newParam))
  newxmlInput.xml <- as_xml_document(newxmlInput.lst)
  cat('[simutils::updateSimInput] Validating updated xml object... \n')
  newxmlValid <- xmlValidate(xsdName, newxmlInput.xml, config_file_name)
  if (!newxmlValid) {
    
    stop(paste0('[simutils::updateSimInput] The new xml object ', xmlSimInputName, ' is not valid.\n'))
    
  }
  if (!is.null(newFile)) {
    
    write_xml(newxmlInput.xml, newFile, encoding = 'UTF-8')
    cat(paste0('[simutils::updateSimInput] New xml file written in ', newFile, '.\n'))

  }
  
  cat('\n')
  
  return(newxmlInput.xml)
  
}