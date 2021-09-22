#' @title Update simulation xml input files.
#' 
#' @description This function takes an xml input file for the simulator and 
#' updates it according to the set of new parameters specified in the input 
#' parameter list \code{newParam}. The schema definition of the xml file is also
#' required as an input parameter to validate the structure of the output file.
#' 
#' @param xmlSimInput Object of class \code{xml_document} from package 
#' \code{xml2} or of class \code{character} specifying the path of the 
#' simulation input xml file to update.
#' 
#' @param newParam \code{list} with the new values of the parameters to be 
#' updated
#' 
#' @param xsdName
#' 
#' @param configParamList
#' 
#' @param newFileName
#' 
#' @details 
#' 
#' @include xmlValidate.R flatten_deepest.R list_deepest.R
#' 
#' @import xml2 
#' 
#' @examples  
#' rootPath <- system.file(package = 'simutils')
#' config   <- list(Path = list(XML_VALIDATOR = file.path(rootPath, "bin")))
#' 
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
#' configParamList = config,
#' newFileName = newSimInputFile)->x
#' 
#' @export

updateSimInput <- function(xmlSimInput, newParam, xsdName, configParamList, newFileName = NULL){
  
  xmlSimInputName <- deparse(substitute(xmlSimInput))
  cat(paste0('[simutils::updateSimInput] Validating ', xmlSimInputName , '...   '))
  
  xmlValid <- xmlValidate(xsdName, xmlSimInput, configParamList) 
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
  newxmlValid <- xmlValidate(xsdName, newxmlInput.xml, configParamList)
  if (!newxmlValid) {
    
    stop(paste0('[simutils::updateSimInput] The new xml object ', xmlSimInputName, ' is not valid.\n'))
    
  }
  if (!is.null(newFileName)) {
    
    write_xml(newxmlInput.xml, newFileName, encoding = 'UTF-8')
    cat(paste0('[simutils::updateSimInput] New xml file written in ', newFileName, '.\n'))

  }
  
  cat('\n')
  
  return(newxmlInput.xml)
  
}