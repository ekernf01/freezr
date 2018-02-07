## ------------------------------------------------------------------------
# freezr runs scripts and freezes copies along with the results generated.

#' Run an analysis while freezing a copy of the code (and perhaps dependencies) for later perusal.
#'
#' @export
#' @param analyses_to_run R or R markdown files to be run and frozen.
#' @param prefix_omit \code{freeze} archives your code in a way that preserves folder structures, 
#' but if you want to omit a prefix, specify it here. 
#' @param destination Where to save the code (and log, and maybe dependencies and output)
#' @param run_from_cryo_storage If \code{FALSE} (default), runs the code from the current working directory.
#' If \code{TRUE}, runs from \code{destination} (or a folder within it -- see param \code{timestamp_as_folder}).
#' The default option, \code{FALSE}, makes it easier to write your code because you know where it'll be run from.
#' You can access \code{Sys.getenv()[["FREEZR_DESTINATION"]]} within your scripts to produce custom output.
#' The non-default option makes it easier to
#' break code unless it uses absolute paths throughout.
#' @param dependencies A list of file paths to extra files your analysis uses.
#' They will get saved to destination if they are smaller than \code{copy_deps_kb_limit.}
#' @param seed_to_set seed for rng. If \code{NULL} (default), seed doesn't get set.
#' @param timestamp_as_folder If \code{TRUE} (default), one extra folder is added to \code{destination}
#' based on the current time and date (down to the second). If \code{FALSE}, code gets saved to \code{destination}.
#' @param force If \code{FALSE}, refuses to continue when \code{destination} is not empty.
#' If \code{TRUE}, continues saving results even to non-empty \code{destination}. Default is \code{FALSE}.
#' @param copy_deps_kb_limit \code{dependencies} get saved to \code{destination} if they are
#' smaller than \code{copy_deps_kb_limit}.
#' @param purl_aggressively If \code{TRUE} (default), then when purling Rmd files, the
#' corresponding R files may be overwritten.
#' @param chastise If \code{TRUE} (default), creates a \code{notes.txt} file for you and nags
#' you about filling it.
#' @param notes_file Name of file to be created while \code{chastise}ing.
#' @param repos_to_track For git users. If you want to record what commit some outside repo is on -- for 
#' example, a package that you are developing separate from your data analysis repo -- put
#' the path here. You can put more than one. 
#' @return returns \code{destination} arg.
#' @examples
#' 
#' #setwd( file.path( "~", "my_freezr_example_project" ), )
#' #freeze( analyses_to_run = c( "scripts/my_functions.Rmd", "scripts/my_script.R" ),
#' #        destination = file.path("~", "my_freezr_example_project", "results") )
freeze = function( analyses_to_run, 
                   prefix_omit = "scripts",
                   destination,
                   run_from_cryo_storage = FALSE,
                   dependencies = NULL,
                   seed_to_set = NULL,
                   timestamp_as_folder = TRUE,
                   force = FALSE,
                   copy_deps_kb_limit = 100,
                   purl_aggressively = TRUE,
                   chastise = TRUE,
                   notes_file = "notes.md", 
                   repos_to_track = NULL ){
  # # Nag user about leaving themselves notes.
  notes_file = file.path( destination, "notes.md" )
  if( chastise ){
    if( !file.exists( notes_file ) ){
      cat( paste( "\nI noticed there was no file called notes.txt in your destination folder.\n",
                  "I'll make one so you can leave your future self some notes.\n",
                  "If you want me to shut up about this, put `chastise=F`.\n") )
      file.create( file.path( destination, "notes.txt" ) )
    } else {
      notes_length = nchar( paste( trimws( readLines( notes_file ) ), collapse = "" ) )
      if( notes_length <= 50 ){
        cat("\nYour notes.txt file has only ", notes_length, " non-whitespace character(s). \n")
        cat("Fine, I mean, whatever. It's your research, not mine... \n")
      } else {
        cat("\nI see you have a notes.txt file with", 
            notes_length, 
            "non-whitespace characters! \n Keep up the good work! \n")
      }
    }
  }

  # # Prepare and announce destination
  destination = file.path( destination, format( Sys.time(), "%Y_%b_%d__%H_%M_%S") )
  dir.create( destination, recursive = TRUE )
  empty = ( 0 == length( list.files( destination, all.files = TRUE, include.dirs = TRUE, no.. = TRUE ) ) )
  if( file.exists( destination ) && !empty && !force ){
    warning( paste( "freezr is quitting early because that folder already has something in it.",
                    "Try setting `timestamp_as_folder=TRUE` or (if absolutely necessary) `force=TRUE`. ") )
    return( destination )
  }
  if( file.exists( destination ) && !empty && force ){
    warning( paste( "freezr is modifying a folder that already has something in it.",
                    "If that makes you nervous, try setting `timestamp_as_folder=TRUE`. ") )
  }
  cat( paste0( "Saving analysis tools to `", destination, "`.\n" ) )

  # # run analyses and freeze them, capturing graphics and text.
  dir.create( file.path( destination, "output" ), recursive = T )
  dir.create( file.path( destination, "code" ) )
  dir.create( file.path( destination, "logs" ) )
  outfile_graphics = file.path( destination, "output", "graphics.pdf" )
  outfile_text     = file.path( destination, "output", "text.txt" )
  file.create( outfile_text )
  {
    grDevices::pdf( outfile_graphics )
    sink( file = outfile_text )
    if( !is.null( seed_to_set ) ) { set.seed( seed_to_set ) }
    for( analysis_i in analyses_to_run ){
      cat("\n Freezing and running ", analysis_i, "\n")
      if( !file.exists( analysis_i ) ){
        sink_reset()
        stop(paste0("\nfreezr::freeze couldn't find the analysis script at: ", analysis_i, " .\n" ))
      } 
      # Do the actual freezing! 
      my_pattern = paste0( prefix_omit, "|", prefix_omit, .Platform$file.sep )
      frozen_analysis_i = file.path( destination, 
                                     "code",
                                     gsub( pattern = my_pattern, 
                                           replacement = "", 
                                           analysis_i ) )
      if( !dir.exists( dirname( frozen_analysis_i ) ) ){ 
        dir.create( dirname( frozen_analysis_i ) )
      }
      file.copy( from = analysis_i, to = frozen_analysis_i )
      
      # Run script from appropriate wd
      if( run_from_cryo_storage ){
        old_wd = getwd()
        setwd( destination )
        my_err = tryCatch( expr = { run_r_or_rmd( frozen_analysis_i, destination ) }, error = function(e) e )
        setwd( old_wd )
      } else {
        my_err = tryCatch( expr = { run_r_or_rmd( frozen_analysis_i, destination ) }, error = function(e) e )
      }
      # TODO: Send errors and tracebacks (traces back?) to logfile
      # if( !is.null( my_err ) ){
      #   logfile = file.path( destination, "logs", paste0( analysis_i, ".log" ) )
      #   warning( paste0( "Error when running ", analysis_i, "; check ", logfile, " for error and traceback.\n " ) ) 
      #   sink( logfile )
      #   print(my_err)
      #   print(traceback())
      #   sink()
      # }
    }
    sink_reset()
    grDevices::dev.off()
  }

  # # copy dependencies
  if( !is.null( dependencies) )
  {
    deps = data.frame( name = unlist( lapply( dependencies, basename ) ),
                       saved = rep( F, length( dependencies ) ),
                       size_kb = unlist( lapply( dependencies, file.size ) ) / 1000,
                       full_path = dependencies,
                       stringsAsFactors = FALSE)
    for( ii in seq_along( deps$name ) ){
      if( deps$size_kb[[ii]] < copy_deps_kb_limit ){
        suppressWarnings( dir.create( file.path( destination, "dependencies" ), recursive = TRUE ) )
        file.copy( from = deps$full_path[[ii]],
                   to = file.path( destination, "dependencies", deps$name[[ii]] ) )
        deps$saved[ii] = TRUE
      } else {
        if( missing( copy_deps_kb_limit ) ){
          warning( paste0( "By default, freeze assumes you only want to save files below",
                           copy_deps_kb_limit,
                           "kb. To change this, use e.g. `copy_deps_kb_limit=Inf`. " ) )
        }
      }
    }
  } else {
    deps = NULL
  }

  # # Save info on package versions and save hashes of relevant repos.
  logfile_session = file.path( destination, "logs", "sessionInfo.txt" )
  cat( x = paste0( utils::capture.output( utils::sessionInfo() ), collapse = "\n"), file = logfile_session)
  
  logfile_commit = file.path( destination, "logs", "commit_sha1_info.txt" )
  repo_hashes = data.frame( path_to_repo = paste0( repos_to_track ),
                            stringsAsFactors = FALSE ) #goddamn factors fuck up everything
  get_hash = function(path_to_repo){
    gsub( ".git", "", path_to_repo )
    tryCatch({
      hash = system2("git", args = c( " -C ", path_to_repo, " rev-parse HEAD"), stdout = TRUE )
      return( hash )
    }, 
    error = function(e){
      warning( paste0( "Error when trying to obtain hash of repo at ", path_to_repo, ":\n", gettext( e ) ) )
      return( "" )
    }, 
    warning = function(w){
      warning( paste0( "Warning when trying to obtain hash of repo at ", path_to_repo, ":\n", gettext( w ) ) )
      return( "" )
    } )
  }
  repo_hashes$hash =  sapply(repo_hashes$path_to_repo, get_hash)
  write.table( x = repo_hashes, file = logfile_commit, row.names = F, quote = F, sep = "\t")

  # # Make log files, both human and machine readable
  freeze_call = list( analyses_to_run = analyses_to_run,
                      destination = destination,
                      run_from_cryo_storage = run_from_cryo_storage,
                      dependencies = dependencies,
                      seed_to_set = seed_to_set,
                      timestamp_as_folder = timestamp_as_folder,
                      force = force,
                      copy_deps_kb_limit = copy_deps_kb_limit,
                      purl_aggressively = purl_aggressively,
                      chastise = chastise,
                      notes_file = notes_file, 
                      repos_to_track = repos_to_track )
  logfile = file.path( destination, "logs", "freeze_call.txt" )
  saveRDS( freeze_call, file.path( destination, "logs", "freeze_call_RDS.data" ) )
  utils::write.table( getwd(), file.path( destination, "logs", "freeze_call_wd.txt" ),
               quote = FALSE, row.names = FALSE, col.names = FALSE )
  utils::write.table( x = t( as.data.frame( pad_list( freeze_call ) ) ),
               file = logfile,
               quote = FALSE, col.names = FALSE, sep = "\t" )
  return( invisible( destination ) )
}

#' Return a function "flash_freeze" that saves on project-specific boilerplate compared to "freeze". 
#' 
#' @param project_directory Where's your freezr project? Should contain subdirectories "results" and "scripts." Try "freezr::create_empty_project".
#' @param setup_scripts Scripts to be run first every time you flash_freeze an analysis. Helps avoid copy-pasting code. For example, a setup script might load packages, utility functions, or reusable functions that you haven't yet packaged up.
#' @param repos_to_track Helps track outside repos. See corresponding arg of `freeze`. 
#'
#' 
#' @details When run without args, this "flash_freeze" runs the setup scripts and saves to the "interactive" folder. Your namespace retains objects created by the setup scripts, so you can experience what your analysis scripts are experiencing. 
#' Otherwise, "flash_freeze" takes these args: 
#' \itemize{
#' \item "analyses_to_run" should be a character vector containing paths to R or .Rmd files in your "scripts" folder
#' \item "results_subdir" should be a character vector to be added onto the "results" folder before
#' It's optional, and if NULL (recommended), it names the results subfolder after the subfolder 
#' of the last analysis script. This way, the organizational structure is mirrored between the
#' analysis scripts and the results.
#' \item ... extra args passed to "freeze"
#' }
#' @export
#' 
configure_flash_freeze = function( project_directory = getwd(), 
                                   setup_scripts = NULL, 
                                   repos_to_track = project_directory ){
  project_directory = path.expand(project_directory)
  cat("\n")
  cat("Will work from: ",      project_directory, "\n")
  cat("Will run these setup scripts at each call: ", setup_scripts, "\n")
  cat("Will track these repos: \n", repos_to_track, "\n")
  
  if( any( grepl("scripts", setup_scripts) ) ) {
    stop("setup_scripts paths should be relative to <my_project>/scripts/.")
  }

  flash_freeze = function( analyses_to_run = NULL, results_subdir = NULL, ... ){
    if( any( grepl("scripts", analyses_to_run) ) ) {
      stop("Input paths should be relative to <my_project>/scripts/.")
    }
    if ( any( grepl("results", results_subdir ) ) ) {
      stop("results paths should be relative to <my_project>/results/.")
    }
    
    # Set default results subdir to mirror script subdirectory
    if(is.null(results_subdir)){
      if(is.null(analyses_to_run)){
        results_subdir = "interactive"
      } else {
        results_subdir = dirname(rev(analyses_to_run)[1]) 
      }
    }
    
    # Handle case when there is no subdir between "scripts/" and "analyses_to_run"
    if(results_subdir=="." ){
      dest = file.path( project_directory, "results")
    } else {
      dest = file.path( project_directory, "results", results_subdir)
    }

    setup_scripts   = file.path("scripts", setup_scripts )
    analyses_to_run = file.path("scripts", analyses_to_run )
    cat( "\n project_directory: ", project_directory)
    cat( "\n setup_scripts: ",     setup_scripts)
    cat( "\n analyses_to_run:",    analyses_to_run )
    cat( "\n results_subdir:",     results_subdir )
    cat( "\n destination:",        dest )

    freezr::freeze( analyses_to_run = c( setup_scripts, analyses_to_run )  ,
                    destination     = dest,
                    repos_to_track = repos_to_track, ... )
  }
  return( flash_freeze )
}

#' Create a template project suitable for use with freezr. 
#'
#' @export
#' 
create_empty_project = function( directory ){
  cat("Sorry, create_empty_project has not been implemented yet. Please nag eric about this.")
}
  

## ---- eval = F-----------------------------------------------------------
## #' Check frozen dependencies for changes.
## #'
## #' @param freeze_path Location of the freezr archive you want to check.
## #'
## #' @export
## #'
## check_dependencies = function( freeze_path = dirname( Sys.getenv( "FREEZR_DESTINATION" ) ) ) {
##   # Set up output path
##   timestamp = format( Sys.time(), "%Y_%b_%d|%H_%M_%S")
##   dep_check_res_path = file.path( freeze_path, "dependency_diffs", timestamp )
##   suppressWarnings( dir.create( dep_check_res_path ) )
## 
##   # Retrieve recorded dependencies
##   freeze_call = readRDS( file.path( freeze_path, "logs", "freeze_call_RDS.data" ) )
##   dependencies = freeze_call$dependencies
##   deps = data.frame( name = unlist( lapply( dependencies, basename ) ),
##                      size_kb = unlist( lapply( dependencies, file.size ) ) / 1000,
##                      full_path = dependencies,
##                      orig_exists = rep(NA, length(dependencies)),
##                      orig_still_identical = rep(NA, length(dependencies)),
##                      pct_diff = rep(NA, length(dependencies)),
##                      stringsAsFactors = FALSE)
## 
##   # Go through each file and compare frozen version with current version.
##   for( ii in seq_along( deps$name ) ){
##     orig   = deps$full_path[[ii]]
##     frozen = file.path( "dependencies", deps$name[[ii]] )
##     if( !file.exists(orig)){
##       deps$orig_exists[[ii]] = F
##       deps$orig_still_identical[[ii]] = F
##       deps$pct_diff[[ii]] = 0
##     } else if( tools::md5sum(frozen) == tools::md5sum(orig) ){
##       deps$orig_exists[[ii]] = T
##       deps$orig_still_identical[[ii]] = T
##       deps$pct_diff[[ii]] = 0
##     } else {
##       diffs = diffobj::diffFile( target = orig, current = frozen, format = raw )
##       diffs = strsplit(utils::capture.output(show(diffs)), "\n")[[1]]
##       utils::write.table( diffs, dep_check_res_path, row.names = F, col.names = F, quote = F)
##       deps$orig_exists[[ii]] = T
##       deps$orig_still_identical[[ii]] = F
##       deps$pct_diff[[ii]] = length( diffs ) / ( length( readlines( orig ) ) + length( readlines( frozen ) ) )
##     }
##   }
##   return( deps )
## }
## 
## #' Run an analysis previously frozen by \code{freezr::freeze}.
## #'
## #' @export
## #' @param freeze_path A directory created by \code{freezr::freeze} containing these subfolders:
## #' - logs
## #' - code
## #' - output
## #' - user
## #' - dependencies (optional)
## #'
## thaw = function( freeze_path, thaw_path = NULL, verbose = T ){
##   warning("Thaw is not working yet. Stay tuned!")
##   return()
## 
##   # # Retrieve original call to freezr::freeze
##   freeze_call = readRDS( file.path( freeze_path, "logs", "freeze_call_RDS.data" ) )
##   thaw_call = freeze_call
## 
##   # # Execute freezr::freeze call as done originally
##   setwd( readLines(file.path(freeze_call$destination, "logs", "freeze_call_wd.txt")) )
##   if( is.null( thaw_path ) ){
##     thaw_call$destination = file.path( thaw_call$destination, "thaw" )
##   } else {
##     thaw_call$destination = thaw_path
##   }
##   do.call( what = freezr::freeze, args = thaw_call )
## 
##   # # Check environment for changes
## 
## 
##   # # Check dependencies for changes
##   if( !is.null( freeze_call$dependencies) )
##   {
##     check_dependencies(freeze_path = freeze_path )
##   } else {
##     deps = NULL
##   }
## 
##   # # Put thaw report in folder with re-frozen results
##   Sys.getenv("FREEZR_DESTINATION")
## }
## 

