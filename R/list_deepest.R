#' @title Transform an object and its elements in a list of lists keeping 
#' the attributes' structure. 
#' 
#' @description \code{list_deepest} receives an object and transforms it 
#' and all its elements in a list of lists keeping the original attributes' 
#' structure.
#' 
#' @param object A \code{list} or another object with an attributes' structure.
#' 
#' @details Return a \code{list} of \code{list}s using the attributes' structure
#'  from the original object. 
#' 
#' @examples 
#' rootPath  <- system.file(package = "simutils")  
#' xmlFile   <- file.path(rootPath, "extdata/input_files", "simulation.xml")
#' xmlObject <- as_list(xml2::read_xml(xmlFile))
#' list_deepest(xmlObject)
#' 
#' @export
list_deepest <- function(object){
  
  if (!is.list(object)) return(list(object))
  original_attr <- attributes(object)
  output <- lapply(object, function(comp){
    
    localOutput <- list_deepest(comp)
    attributes(localOutput) <- attributes(localOutput) 
    return(localOutput)
    
  })
  attributes(output) <- original_attr
  return(output)

}
