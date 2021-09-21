#' @title Return the root path for the MND simulation as \code{character}.
#' 
#' @description \code{getRootPath} returns the root path for the MND simulation. 
#' 
#' It searches, successively upon failure, the path in the environment variable 
#' of the system called "MNO_SIMULATION_PATH" and in the mno_simulation.ini file
#'  stored in the userprofile directory. 
#'  
#' If it doesn't find the path, it stops the execution and returns an error.
#' 
#' @details \code{character} with the name of the root path.
#' 
#' @examples 
#' getRootPath()
#' 
#' 
#' 
#' @export
getRootPath <- function() {
  
  info <- Sys.info()
  
  if (info['sysname'] == "Windows") {
    
    ROOT_PATH <- Sys.getenv("MNO_SIMULATION_PATH")
    ROOT_PATH <- file.path(dirname(ROOT_PATH), basename(ROOT_PATH))
    if (ROOT_PATH == "") {
      
      filename <- file.path(Sys.getenv("userprofile"), '.mno_simulation.ini')
      if (file.exists(filename)) {
        
        rootpath <- configr::read.config(
          file.path( Sys.getenv("userprofile"), '.mno_simulation.ini') )
        ROOT_PATH <- rootpath$Path$MNO_SIMULATION_PATH
        if (ROOT_PATH == "") {
          
          stop(paste0("[getRootPath] Path to the root folder not set in ",
                      filename, "\n."))
          
        }  
      } else {
        
        stop(paste0("[getRootPath] ", filename, "does not exist.\n"))
      }
    }
    
  } else {
    
    root_path <- configr::read.config(file.path(fs::path_home(), '.mno_simulation.ini'))
    ROOT_PATH <- root_path$ROOT$MNO_SIMULATION_PATH
    
  }
  
  return(ROOT_PATH)
}