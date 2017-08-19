## ------------------------------------------------------------------------
# freezr runs scripts and freezes copies along with the results generated.

#' Run an analysis while freezing a copy of the code (and perhaps dependencies) for later perusal.
#'
#' @export
#' @param analyses_to_run R or R markdown files to be run and frozen.
#' @param destination Where to save the code (and log, and maybe dependencies and output)
#' @param capture_output If \code{"text"}, catures the console output.
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
#' @return returns \code{destination} arg.
#' @examples
#' setwd( file.path( "~", "my_project" ) )
#' freeze(analyses_to_run = c( "my_functions.Rmd", "my_script.R" ),
#'        destination = file.path("~", "my_project", "results") )
freeze = function( analyses_to_run,
                   destination,
                   run_from_cryo_storage = FALSE,
                   dependencies = NULL,
                   seed_to_set = NULL,
                   timestamp_as_folder = TRUE,
                   force = FALSE,
                   copy_deps_kb_limit = 100,
                   purl_aggressively = TRUE,
                   chastise = TRUE,
                   notes_file = "notes.md" ){

  # # Nag user about leaving themselves notes.
  notes_file = file.path( destination, "notes.md" )
  if( chastise ){
    if( !file.exists( notes_file ) ){
      cat( paste( "I noticed there was no file called notes.txt in your destination folder.\n",
                  "I'll make one so you can leave your future self some notes.\n",
                  "If you want me to shut up about this, put `chastise=F`.\n") )
      file.create( file.path( destination, "notes.txt" ) )
    } else {
      notes_length = nchar( paste( trimws( readLines( notes_file ) ), collapse = "" ) )
      if( notes_length <= 50 ){
        cat("Your notes.txt file has", notes_length, "non-whitespace character(s). \n")
        cat("Fine, I mean, whatever. It's your research, not mine... \n")
      } else {
        cat("I see you have a notes.txt file with", notes_length, "non-whitespace characters! \n Keep up the good work! \n")
      }
    }
  }

  # # Prepare and announce destination
  destination = file.path( destination, format( Sys.time(), "%Y_%b_%d|%H_%M_%S") )
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
  dir.create( file.path( destination, "output" ) )
  dir.create( file.path( destination, "code" ) )
  dir.create( file.path( destination, "logs" ) )
  graphics_out = file.path( destination, "output", "graphics.pdf" )
  pdf( graphics_out )
  {
    outfile = file.path( destination, "output", "text.txt" )
    file.create( outfile )
    sink( file = outfile )
    {
      if( !is.null( seed_to_set ) ) { set.seed( seed_to_set ) }
      for( analysis_i in analyses_to_run ){
        frozen_analysis_i = file.path( destination, "code", analysis_i )
        if( !dir.exists( dirname( frozen_analysis_i ) ) ){ dir.create( dirname( frozen_analysis_i ) )}
        file.copy( from = analysis_i, to = frozen_analysis_i )
        if( run_from_cryo_storage ){
          old_wd = getwd()
          setwd( destination )
          try( expr = { freezr:::run_r_or_rmd( frozen_analysis_i, destination ) } )
          setwd( old_wd )
        } else {
          try( expr = { freezr:::run_r_or_rmd( frozen_analysis_i, destination ) } )
        }
      }
    }
    sink_reset()
  }
  dev.off()

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

  # # Save info on e.g. package versions.
  session_info = file.path( destination, "logs", "sessionInfo.txt" )
  cat( x = paste0( capture.output( sessionInfo() ), collapse = "\n"), file = session_info)

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
                      notes_file = notes_file )
  logfile = file.path( destination, "logs", "freeze_call.txt" )
  saveRDS( freeze_call, file.path( destination, "logs", "freeze_call_RDS.data" ) )
  write.table( getwd(), file.path( destination, "logs", "freeze_call_wd.txt" ),
               quote = FALSE, row.names = FALSE, col.names = FALSE )
  write.table( x = t( as.data.frame( freezr:::pad_list( freeze_call ) ) ),
               file = logfile,
               quote = FALSE, col.names = FALSE, sep = "\t" )
  return( invisible( destination ) )
}


## ------------------------------------------------------------------------
#' Run an analysis previously frozen by \code{freezr::freeze}. NOT GUARANTEED TO WORK but it does its best.
#'
#' @export
#' @param freeze_path A directory created by \code{freezr::freeze} containing these subfolders:
#' - logs
#' - code
#' - output (optional)
#' - dependencies (optional)
#' - user (optional)
#' @param use_old_dependencies Defaults to \code{FALSE}, so if your dependencies have changed, then
#' your analysis may not come out of cryo storage working exactly the same way.
#' If \code{alter_dependencies==TRUE}, then before reviving your analysis, \code{thaw} will GO INTO
#' THE FILES AND FOLDERS LISTED AS DEPENDENCIES, MOVE THEM OUT OF THE WAY, AND PUT THE FROZEN VERSIONS IN THEIR PLACES.
#' It cleans up after itself, though, moving the newer dependencies back once it has finished.
thaw = function( freeze_path, alter_dependencies = F ){
  stop("Thaw is not working yet. Stay tuned!")
  return()

  # # Retrieve original call to freezr::freeze
  freeze_call = readRDS( file.path( freeze_path, "logs", "freeze_call_RDS.data" ) )


  # # Move frozen scripts to where they were run from, moving stuff out of the way as needed
  # if( !freeze_call$run_from_cryo_storage ){
  #   original_wd = read.table( file.path( freeze_path, "logs", "freeze_call_wd.txt" ),
  #                             header = FALSE, stringsAsFactors = FALSE )[[1]]
  #
  #   for( script in freeze_call$analyses_to_run ){
  #     if( ???)
  #       file.copy( original_wd )
  #   }
  # }

  # # Move frozen dependencies to where they were run from
  if( dir.exists( file.path( freeze_path, "dependencies" ) ) ){
    warning( "you froze some dependencies" )
  }

  # # Execute freezr::freeze call as done originally
  do.call( what = freezr::freeze, args = freeze_call )

}


