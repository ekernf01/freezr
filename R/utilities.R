## ------------------------------------------------------------------------

#' \code{source} code from an R or Rmarkdown file.
#'
#' @param destination Goes into the FREEZR_DESTINATION environmental variable.
#' @param purl_aggressively If TRUE, don't worry about overwriting R files when purling.
#' @param file_name An RMarkdown file or an R file to be (\code{purl}ed and) run.
#' The file gets run **from the directory it is in**, not from \code{getwd()}.
#' @return Name of input possibly with `.Rmd` changed to `.R`.
run_r_or_rmd = function( file_name, destination, purl_aggressively ) {
  name_ext = strsplit( x = file_name, split = ".", fixed = TRUE)[[1]]
  ext = name_ext[ length( name_ext ) ]
  name_dot_R = paste( c( name_ext[ -length( name_ext ) ], "R"), collapse = "." )
  Sys.setenv(FREEZR_DESTINATION=file.path( destination, "user" ) )
  suppressWarnings( dir.create( file.path( destination, "user" ) ) )
  if( tolower( ext ) == "r" ) {
    source( name_dot_R )
  } else if( tolower( ext ) == "rmd" ){
    # Purl, source, and clean up
    if( file.exists( name_dot_R ) && !purl_aggressively ){
      warning( paste( "freezr is using existing `.R` versions of `.Rmd`",
                      "files since you set `purl_aggressively=FALSE`." ) )
    } else {
      knitr::purl( file_name, output = name_dot_R, quiet = TRUE )
    }
    source( name_dot_R )
    file.remove( name_dot_R )
  } else {
    stop("Can only handle `.R` and `.Rmd` files.")
  }
  return(NULL)
}

#' Turn a ragged list of atomic vectors to a rectangle by adding filler.
#'
#' @param x Ragged list
#' @param filler What to put in the new entries.
#'
pad_list = function( x, filler = "" ){
  n = max(sapply(x, length))
  pad_vec = function( v ){ c( v, rep( filler, n - length( v ) ) )}
  x = stats::setNames( lapply( x, pad_vec ), names( x ) )
  return( x )
}

#' Fix a problem where nothing prints to the console after an unknown number of \code{sink()} calls.
#'
#' @export
#' @seealso Many thanks to this thread:
#' \url{http://stackoverflow.com/questions/18730491/sink-does-not-release-file}
sink_reset <- function(){
  for( i in seq_len( sink.number( ) ) ){
    sink( NULL )
  }
}

#' A way to run code, handle errors, and (unlike tryCatch) preserve the traceback.
#'
#' @export
#' @seealso Many thanks to Berry Boessenkool for sharing this very useful bit of code.
#' \url{https://stackoverflow.com/a/40899766/3371472}
tryStack <- function( expr, silent = FALSE ){
  tryenv <- new.env()
  my_handler = function(e)
  {
    stack <- sys.calls()
    stack <- stack[-(2:7)]
    stack <- head(stack, -2)
    stack <- sapply(stack, deparse)
    assign("stackmsg", value=paste(stack,collapse="\n"), envir=tryenv)
  }
  out <- try(withCallingHandlers(expr, error=my_handler), silent=silent)
  if(inherits(out, "try-error")) {
    return( tryenv$stackmsg )
  } else {
    return( out )
  }
}


