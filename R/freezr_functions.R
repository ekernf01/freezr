# freezr runs scripts and freezes copies along with the results generated.

#' Run an analysis while freezing a copy of the code (and perhaps dependencies) for later perusal.
#'
#' @param analyses_to_run R or R markdown files to be run and frozen.
#' @param destination Where to save the code (and log, and maybe dependencies)
#' @param run_from_cryo_storage If \code{FALSE} (default), runs the code from the current working directory.
#' If \code{TRUE}, runs from \code{destination} (or a folder within it -- see param \code{timestamp_as_folder}).
#' The default option makes it easy to find files your code depends on, but hard to put your own
#' output files in \code{destination}. The non-default option has the opposite properties and tends to
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
#' @param chastize If \code{TRUE} (default), creates a \code{notes.txt} file for you and nags
#' you about filling it.
#' @param notes_file Name of file to be created while \code{chastize}ing.
#' @return \code{NULL} return value.
#' @examples
#' setwd( file.path( "~", "my_project" ) )
#' freeze(analyses_to_run = c( "my_functions.Rmd", "my_script.R" ),
#'        destination = file.path("~", "my_project", "results") )
freeze = function( analyses_to_run,
                   destination,
                   run_from_cryo_storage = F,
                   dependencies = NULL,
                   seed_to_set = NULL,
                   timestamp_as_folder = T,
                   force = F,
                   copy_deps_kb_limit = 100,
                   purl_aggressively = T,
                   chastise = T,
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
  dir.create( destination, recursive = T )
  empty = ( 0 == length( list.files( destination, all.files = TRUE, include.dirs = TRUE, no.. = TRUE ) ) )
  if( file.exists( destination ) && !empty && !force ){
    warning( paste( "freezr is quitting early because that folder already has something in it.",
                    "Try setting `force=TRUE` or `timestamp_as_folder=TRUE`. ") )
    return()
  }
  if( file.exists( destination ) && !empty && force ){
    warning( paste( "freezr is modifying a folder that already has something in it.",
                    "If that makes you nervous, try setting `timestamp_as_folder=TRUE`. ") )
    return()
  }
  graphics_out = file.path( destination, "your_graphics_out.pdf" )
  cat( paste0( "Saving analysis tools to `", destination, "`\n with plots in `", graphics_out, "`.\n" ) )

  # # run analyses and freeze them, capturing graphics and text.
  if( !is.null( seed_to_set ) ) { set.seed( seed_to_set ) }
  pdf( graphics_out )
  {
    outfile = file.path( destination, "your_text_output.txt" )
    file.create( outfile )
    # # tryCatch statements should undo the sink call in case the user's script fails.
    sink( file = outfile )
    {
      for( analysis_i in analyses_to_run ){
        frozen_analysis_i = file.path( destination, analysis_i )
        file.copy( from = analysis_i, to = frozen_analysis_i )
        if( run_from_cryo_storage ){
          old_wd = getwd()
          setwd( destination )
           try( expr = { run_r_or_rmd( frozen_analysis_i, destination ) } )
          setwd( old_wd )
        } else {
          try( expr = { run_r_or_rmd( frozen_analysis_i, destination ) } )
        }
      }
    }
    sink()
  }
  dev.off()

  # # copy dependencies
  if( !is.null( dependencies) )
  {
    deps = data.frame( name = unlist( lapply( dependencies, basename ) ),
                       saved = rep( F, length( dependencies ) ),
                       size_kb = unlist( lapply( dependencies, file.size ) ) / 1000,
                       full_path = dependencies,
                       stringsAsFactors = F)
    for( ii in seq_along( deps$name ) ){
      if( deps$size_kb[[ii]] < copy_deps_kb_limit ){
        suppressWarnings( dir.create( file.path( destination, "dependencies" ), recursive = T ) )
        file.copy( from = deps$full_path[[ii]],
                   to = file.path( destination, "dependencies", deps$name[[ii]] ) )
        deps$saved[ii] = T
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
  session_info = file.path( destination, "sessionInfo.txt" )
  write( capture_output( print( sessionInfo() ) ), session_info)

  # # Make a nice happy log file
  logfile = file.path( destination, "freezr_log.txt" )
  file.create( logfile )
  sink( file = logfile )
  {
    cat(paste("freezr was called from `", getwd(),  "` .\n"))
    cat(paste("There were", length(analyses_to_run), "files to be sourced, in this order:\n"))
    cat( paste( analyses_to_run, collapse = "\n" ))
    cat( "\nThe destination was `", destination, "`.\n" )
    cat( "Any text sent to the console was diverted to `", outfile, "`.\n" )
    cat( "Any graphics sent to the interactive graphics window were diverted to `", graphics_out, "`.\n" )
    cat( "The R version and package version info is in `", session_info, "`.\n" )
    if(!is.null(deps)){
      cat( "Dependencies were saved if below", copy_deps_kb_limit, "kb.\n" )
      write.table( deps, row.names = F, quote = F, sep = "\t" )
    }
    if(!is.null(seed_to_set)){
      cat( "The seed was set as `", seed_to_set, "`." )
    }
    cat( "If you enjoyed using freezr, please write your future self some nice notes on my behalf!\n" )
  }
  sink()


  return()
}


#' \code{source} code from an R or Rmarkdown file.
#'
#' @param file_name An RMarkdown file or an R file to be (\code{purl}ed and) run.
#' The file gets run **from the directory it is in**, not from \code{getwd()}.
#' @return Name of input possibly with `.Rmd` changed to `.R`.
run_r_or_rmd = function( file_name, destination ) {
  name_ext = strsplit( x = file_name, split = ".", fixed = T)[[1]]
  ext = name_ext[ length( name_ext ) ]
  name_dot_R = paste( c( name_ext[ -length( name_ext ) ], "R"), collapse = "." )
  Sys.setenv(FREEZR_DESTINATION=destination)
  if( tolower( ext ) == "r" ) {
    source( file_name )
  } else if( tolower( ext ) == "rmd" ){
    # Purl, source, and clean up
    if( file.exists( name_dot_R ) && !purl_aggressively ){
      stop( paste( "freezr will not overwrite existing `.R` versions of `.Rmd`",
                   "files since you set `purl_aggressively=FALSE`." ) )
    }
    knitr::purl( file_name, output = name_dot_R, quiet = T )
    source( name_dot_R )
    file.remove( name_dot_R )
  } else {
    stop("Can only handle `.R` and `.Rmd` files.")
  }
  return( name_dot_R )
}

#' Fix a problem where nothing prints to the console.
#' @seealso Many thanks to this thread:
#' \url{http://stackoverflow.com/questions/18730491/sink-does-not-release-file}
sink_reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}
