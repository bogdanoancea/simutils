#' @title Validate an xml file using an xsd schema file.
#' 
#' @description \code{xmlvalidate} validates an xml file using an xsd schema 
#' file.
#' 
#' This function is a wrapper for a validator written in Java. Thus, it needs a 
#' jar file, which runs in the command line. If the xml file does not conform to
#'  the schema definition, it provides information where the error is.
#'  
#' The xsdName and xmlObject arguments must be provided with absolute paths. 
#' 
#' The jarName argument only needs the name of the file because it must be 
#' placed in the folder structure of the simulation.
#' 
#' @param xsdName Path of the xsd file containing the schema.
#' 
#' @param xmlObject Object of class \code{xml_document} from package 
#' \code{xml2} or of class \code{character} specifying the path of the xml file 
#' to validate.
#' 
#' @param jarName Filename of jar file.
#' 
#' @param configParamList \code{list} which must have a component named 
#' \code{Path}, which, in turn, must contain a list with a component named
#' \code{XML_VALIDATOR} (see example below).
#' 
#' @details Return TRUE if xml file is validated; otherwise, FALSE.
#' 
#' @examples
#' rootPath <- system.file(package = "simutils")  
#' config   <- list(Path = list(XML_VALIDATOR = file.path(rootPath, "bin")))
#' 
#' # xml file 1
#' xsdName  <- file.path(rootPath, 
#'     "extdata/metadata/input_files/schema_definition", "antennas_dict.xsd")
#' xmlFile  <- file.path(rootPath, "extdata/input_files", "antennas.xml")
#' xmlValidate(xsdName, xmlFile, config)
#' 
#' # xml file 2
#' xsdName  <- file.path(rootPath, 
#'     "extdata/metadata/input_files/schema_definition", "simulation_dict.xsd")
#' xmlFile  <- file.path(rootPath, "extdata/input_files", "simulation.xml")
#' xmlValidate(xsdName, xmlFile, config)
#' 
#' 
#' @export
xmlValidate <- function(xsdName, xmlObject, configParamList){
  
  jarName <- file.path(configParamList$Path$XML_VALIDATOR, 'schema-check.jar')
  if (!file.exists(jarName)) {
    
    stop(paste0('[simutils::xmlvalidate]', jarName, 
                'does not exist. Please check XML_VALIDATOR component in list ', 
                configParamList,
                ' and/or jarName argument.\n'))
    
  }
  
  
  if (inherits(xmlObject, 'xml_document')) {
    
    xmlName <- deparse(substitute(xmlObject))
    tempFileName <- tempfile(fileext = '.xml')
    cat(paste0('[simutils::xmlValidate] ', xmlName, ' temporarily written in ', tempFileName, '.\n\n'))
    xmlFile <- write_xml(xmlObject, tempFileName)
    
  } 
  
  if (inherits(xmlObject, 'character')) {
    
    tempFileName <- xmlObject
    
  }
  
  sysinfo <- Sys.info()
  if (sysinfo['sysname'] == 'Windows') {
    
    output <- system2('java', args = paste0(' -jar ', jarName, ' ', xsdName, ' ', tempFileName ))
    
  } else {
    
    output <- system(paste0('java' , ' -jar ', jarName, ' ', xsdName, ' ', tempFileName))
  }
  
  if (inherits(xmlObject, 'xml_document')) {
    
    unlink(tempFileName)
    
  } 
  
  output <- ifelse(output == 0, TRUE, FALSE)
  
  return(output)
  
}

