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
#' @param config_file_name Filename of the configuration file of the simulation.
#' Default value is \code{'config.ini'}.
#' 
#' @details Return TRUE if xml file is validated; otherwise, FALSE.
#' 
#' @include getRootPath.R
#' 
#' @examples 
#' rootPath <- 'F:/simulation1'
#' xsdName  <- file.path(rootPath, 'metadata/1.networkEvents', 'networkEvents_dict.xsd')
#' xmlObject  <- file.path(rootPath, 'metadata/1.networkEvents', 'networkEvents_dict.xml')
#' xmlValidate(xsdName, xmlObject)
#' 
#' rootPath <- 'F:/simulation1'
#' xsdName  <- file.path(rootPath, 'metadata/0.simulation', 'simulation_dict.xsd')
#' xmlObject  <- xml2::read_xml(file.path(rootPath, '0.simulation/input/scenario1', 'simulation.xml'))
#' xmlValidate(xsdName, xmlObject)
#' @export
xmlValidate <- function(xsdName, xmlObject, config_file_name = 'config.ini'){
  
  ROOT_PATH     <- getRootPath()
  if (!file.exists(file.path(ROOT_PATH, config_file_name))){
    
    stop(paste0('[xmlvalidate]', config_file_name, 'does not exist.\n'))
    
  }
  config  <- configr::read.config(file.path(ROOT_PATH, config_file_name))
  jarName <- file.path(ROOT_PATH, config$Path$XML_VALIDATOR)
  if (!file.exists(jarName)) {
    
    stop(paste0('[xmlvalidate]', jarName, 
                'does not exist. Please check XML_VALIDATOR path in ', 
                config_file_name,
                ' and/or jarName argument.\n'))
    
  }
  
  
  if (inherits(xmlObject, 'xml_document')) {
    
    xmlName <- deparse(substitute(xmlObject))
    tempFileName <- tempfile(, fileext = '.xml')
    cat(paste0('[xmlValidate] ', xmlName, ' temporarily written in ', tempFileName, '.\n\n'))
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

