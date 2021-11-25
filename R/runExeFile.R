#' @title Run an MND simulation using the simulator.exe file.
#' 
#' @description \code{runExeFile} reads the simulator path to the executable
#'  file of the simulator of mobile network event data and the input file 
#' parameters to execute the simulator.exe from this path.
#'  
#' If the executable file doesn't exist, it returns an error.
#' 
#' The executable file must be download from the Assets section of this URL
#' in github.com:
#' 
#' https://github.com/bogdanoancea/simulator/releases/tag/1.2.0
#' 
#' The name of the executable file is \code{simulator.exe}.
#' 
#' @param path_to_exe character vector with the absolute path to the executable 
#' file of the simulator of mobile network event data.
#' 
#' @param input_folder absolute path where the simulation input files are.
#' 
#' @param simulationCFGFile name of the configuration simulation file 
#' (input file).
#' 
#' @param mapFile name of the map file (input file).
#' 
#' @param personsCFGFile name of the persons file (input file).
#' 
#' @param antennasCFGFile name of the antennas file (input file).
#' 
#' @param outputFolder Absolute path where the output files are going to be 
#' saved.
#' 
#' @details Return invisible \code{NULL} after placing output files of the 
#' simulation in the output folder.
#' 
#' @examples
#' \dontrun{
#' # if exe file dowloaded into the R_USER folder:
#' local_exe<- file.path(Sys.getenv('R_USER'), 'simulator.exe')
#' # Otherwise specify your own path to the simulator.exe file
#' 
#' rootPath <- system.file(package = "simutils")
#' runExeFile(
#'  path_to_exe       = dirname(local_exe),
#'  input_folder      = file.path(rootPath, "extdata/input_files"),
#'  simulationCFGFile = "simulation.xml",
#'  mapFile           = "map.wkt",
#'  personsCFGFile    = "persons.xml",
#'  antennasCFGFile   = "antennas.xml",
#'  outputFolder      = Sys.getenv('R_USER'))
#'  }
#'   
#' @export
runExeFile <- function(
  path_to_exe,
	input_folder,
	simulationCFGFile,
	mapFile,
	personsCFGFile,
	antennasCFGFile,
	outputFolder) {
  
	if (path_to_exe == ""){
		
	  stop("[simutils::runExeFile] Path to simulator.exe file not set.\n")
	  
	}
	sysinfo <- Sys.info()
	if (sysinfo['sysname'] == "Windows") {
	  
		exe_file <- file.path(path_to_exe, "simulator.exe")
		
	} else {
	  
		exe_file <- file.path(path_to_exe, "simulator")
		
	}
	
	if (!file.exists(exe_file)) {
	  
		stop(paste0("[simutils::runExeFile] simulator.exe file cannot be found at ", path_to_exe, "\n"))
	
	}
	
	
	# Execute the simulator
	if (sysinfo['sysname'] == 'Windows') {
	  
		system2(
		  exe_file,
			args = paste0(
				" -s ",
				file.path(input_folder, simulationCFGFile),
				" -m ",
				file.path(input_folder, mapFile),
				" -p ",
				file.path(input_folder, personsCFGFile),
				" -a ",
				file.path(input_folder, antennasCFGFile)
			)
		)
	  
	} else {
	  
		system(
		  paste0(exe_file, 
					" -s ",
					file.path(input_folder, simulationCFGFile),
					" -m ",
					file.path(input_folder, mapFile),
					" -p ",
					file.path(input_folder, personsCFGFile),
					" -a ",
					file.path(input_folder, antennasCFGFile)
			)
		)
	}
	
	cat(paste0('[simutils::runExeFile] output written in ', outputFolder))
	
	return(invisible(NULL))
	
}
