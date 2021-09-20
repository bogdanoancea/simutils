#' @title Run an MND simulation using the simulator.exe file.
#' 
#' @description \code{runExeFile} reads the simulator path to exe from the 
#' \code{configParamList} input parameter and execute the simulator.exe from 
#' this path. This input parameter must have a component named \code{Path}, 
#' which, in turn, must contain a list with a component named
#' \code{SIMULATOR_PATH_TO_EXE} (see example below).
#'  
#' If the file doesn't exist, it returns an error.
#' 
#' @param configParamList \code{list} which must have a component named 
#' \code{Path}, which, in turn, must contain a list with a component named
#' \code{SIMULATOR_PATH_TO_EXE} (see example below).
#' 
#' @param sysinfo \code{character} with the info about the system (basically the
#'  output from \code{Sys.info()}).
#' 
#' @param input_folder Absolute path where the simulation input files are.
#' 
#' @param simulationCFGFile Name of the configuration simulation file 
#' (input file).
#' 
#' @param mapFile Name of the map file (input file).
#' 
#' @param personsCFGFile Name of the persons file (input file).
#' 
#' @param antennasCFGFile Name of the antennas file (input file).
#' 
#' @param outputFolder Full path where the output files are going to be saved.
#' 
#' @details Return invisible \code{NULL} after placing output files of the 
#' simulation in the output folder.
#' 
#' @examples
#' config <- list(
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
#' @export
runExeFile <- function(
  configParamList,
	sysinfo,
	input_folder,
	simulationCFGFile,
	mapFile,
	personsCFGFile,
	antennasCFGFile,
	outputFolder) {
  
	path_to_exe <- configParamList$Path$SIMULATOR_PATH_TO_EXE
	if (path_to_exe == ""){
		
	  stop("[runExeFile] Path to simulator.exe file not set in config.ini.\n")
	  
	}
	
	if (sysinfo['sysname'] == "Windows") {
	  
		exe_file <- file.path(path_to_exe, "simulator.exe")
		
	} else {
	  
		exe_file <- file.path(configParamList$Path$SIMULATOR_PATH_TO_EXE, "simulator")
		
	}
	
	if (!file.exists(exe_file)) {
	  
		stop("[runExeFile] Path to simulator exe file is wrong.\n")
	
	}
	# Execute the simulator
  setwd(outputFolder)
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
			))
	} else {
	  
		system(paste0(exe_file, 
					" -s ",
					file.path(input_folder, simulationCFGFile),
					" -m ",
					file.path(input_folder, mapFile),
					" -p ",
					file.path(input_folder, personsCFGFile),
					" -a ",
					file.path(input_folder, antennasCFGFile)
				))
	}
	
	return(invisible(NULL))
	
}
