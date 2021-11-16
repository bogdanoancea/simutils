#' @title Run the simulation software using the docker image.
#' 
#' @description \code{runDockerImage} runs the simulation software using
#' the docker image. 
#' 
#' @param dockerImage Absolute path where the file to run the docker image is.
#' 
#' @param input_folder Absolute path of the folder which contains the input data.
#' 
#' @param simulationCFGFile Name of the simulation configuration file.
#' 
#' @param mapFile Name of the map file.
#' 
#' @param personsCFGFile Name of the persons' configuration file.
#' 
#' @param antennasCFGFile Name of the antennas' configuration file.
#' 
#' @param output_folder Name of the output folder indicated in the 
#' simulation's configuration file. 
#' 
#' @details Return invisible NULL after executing the docker image.
#' 
#' @examples 
#' rootPath          <- system.file(package = 'simutils')
#' dockerImage       <- file.path(rootPath, "extdata/docker/simulator.tar")
#' input_folder      <- file.path(rootPath, "extdata/input_files")
#' simulationCFGFile <- 'simulation.xml'
#' mapFile           <- 'map.wkt'
#' personsCFGFile    <- 'persons.xml'
#' antennasCFGFile   <- 'antennas.xml'
#' output_folder     <- file.path(Sys.getenv('HOME'), 'example_docker')
#' \dontrun{
#' # run only after checking your installation of docker
#' runDockerImage(
#'   dockerImage = dockerImage,
#'   input_folder = input_folder,
#'   simulationCFGFile = simulationCFGFile,
#'   mapFile = mapFile,
#'   personsCFGFile = personsCFGFile,
#'   antennasCFGFile = antennasCFGFile,
#'   output_folder = output_folder)
#' }
#' @export 
runDockerImage <- function(
  dockerImage,
	input_folder,
	simulationCFGFile,
	mapFile,
	personsCFGFile,
	antennasCFGFile,
	output_folder) {

	if (!file.exists(dockerImage)){
	  
		stop("[runDockerImage] Docker image does not exists.\n")
	  
	}
  
	sysinfo <- Sys.info()
	if (sysinfo['sysname'] == "Windows"){
	  
		system2("docker", args = paste0("load -i ", dockerImage))
	  
	} else {
	  
	  username <- sysinfo[['user']]
		system(paste0("sudo -u ", username, " docker load -i ", dockerImage))
		
	}
	initial_wd <- getwd()
	#setwd(output_folder)
	on.exit(setwd(initial_wd))
	
	# Prepare input files
	if (!file.exists(output_folder)) {
	  
		of <- file.path(output_folder, 'output')
	  result <- dir.create(of, recursive = TRUE)
		if (result[[1]] != TRUE) {
		  
			stop("[simutils::runDockerImage] output folder for docker cannot be created")
		  
		}
		
	} else {
	  
		existingFiles <- list.files(of)
		if (length(existingFiles) >= 1) {
			for (i in 1:length(existingFiles)) {
			  
				file.remove(file.path(of, existingFiles[[i]]))
			  
			}
		}
	}
	# Copy the input files files
	file.copy(
	  from      = file.path(input_folder, simulationCFGFile), 
	  to        = of, 
	  overwrite = TRUE
	)
	file.copy( 
	  from      = file.path(input_folder, mapFile), 
	  to        = of, 
	  overwrite = TRUE 
	)
	file.copy( 
	  from      = file.path(input_folder, personsCFGFile), 
	  to        = of, 
	  overwrite = TRUE
	)
	file.copy( 
	  from      = file.path(input_folder, antennasCFGFile),
	  to        = of, 
	  overwrite = TRUE
	)

	# Command line arguments for docker
	simargs <- paste0(
		" -s ",
		file.path('output', simulationCFGFile),
		" -m ",
		file.path('output', mapFile),
		" -p ",
		file.path('output', personsCFGFile),
		" -a ",
		file.path('output', antennasCFGFile)
	)

	# Run!
	cmd_args = paste0(
		"run --rm -v ",
		file.path(of),
		":/repo/",
		'output',
		" simulator -t -i ",
		simargs
	)
	setwd(output_folder)
	if (sysinfo['sysname'] == "Windows"){
	  
		system2( "docker", args = cmd_args )
	  
	} else {
	  
	  username = sysinfo[['user']]
	  system(paste0("sudo -u ", username, " docker ", cmd_args) )
	}
  
	return(invisible(NULL))
	
}
