## ------------------------------------------------------------------------
#' Make sure everything in the inventory is actually present
#'
#' This function issues warnings if the inventory or any file it points to is absent. 
#' It returns a dataframe with details, unless the inventory is absent, in which case
#' it returns NULL.
#'
#' @export
#'
inventory_check = function( inv_location = NULL ){

  # inventory_show will throw an error if the inventory's not there, but we want a warning instead.
  inv = tryCatch( inventory_show( inv_location, make_new = F ) , 
                  error = function(e){
                    if( conditionMessage(e)=="There is no inventory at that location."){
                      warning("There is no inventory at that location.")
                      return(NULL)
                    } else {
                      stop(paste0("Unexpected error in inventory_show:\n", print(e)))
                    }
                  }  )
  if(is.null(inv)){ return() }
  
  print(1)
  inventory_path = inventory_find( inv_location )
  inv_location = dirname( inventory_path )
  print(2)
  full_paths = sapply(inv$tag, inventory_get, inv_location = inv_location)
  results = data.frame( tag = inv$tag,
                        exists = file.exists(full_paths) | dir.exists(full_paths),
                        full_path = full_paths ) 
  print(3)
  if( !all(results$exists)){
    warning("Some of your inventory items cannot be found! \n")
  }
  return( results )
}

#' Keep track of important items from previous analyses.
#'
#' @export
#' @param inv_location Path to the inventory you want to create or modify. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @param tag identifier for an inventory record that you want to add.
#' @param parent_tag identifier for a file that this analysis depends on.
#' @param filename relative path from \code{inv_location} for a file that you want to add to the inventory.
#' @param extra Any character string without tabs. Meant for metadata to be associated with the given \code{tag}
#'  and \code{filename}.
#'  This may just be notes, or you could include an underscore- and pipe-delimited list of key-value pairs;
#'  the sky's the limit.
#' @param force Overwrite or delete any existing entry with the given tag. Default is \code{FALSE}.
#'
#' @details \code{inventory_*} functions help track data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory_add} looks for this table at the \code{inv_location} you specify and creates a new one if needed.
#' \code{inventory_add} will add a row with the given tag. If that tag is present already and
#'  \code{force==TRUE}, the record will be altered. If the tag is present but \code{force==FALSE},
#'  the tag will be altered via \code{make.unique} and a new record will be created.
#'
#'
inventory_add = function( tag = NULL, inv_location = NULL, filename = NULL,
                          extra = "", parent_tag = "",
                          force = FALSE ){

  if( !is.null( tag ) ) { assertthat::assert_that( tag!="" ) }
  if( length( grep( x=extra, pattern = "\t", fixed = T ) ) > 0 ){
    stop("Sorry, inventories are tab-delimited. `extra` field may not contain tabs.")
  }

  inventory_path = inventory_find( inv_location )
  inv_location = dirname( inventory_path )
  inv = inventory_show( inv_location, make_new = T )

  # # As a courtesy, check whether filename exists.
  relative = (substring(filename, 1, 1)[[1]]!=.Platform$file.sep )
  if( relative ){
    filename_full = file.path( inv_location, filename )
  } else {
    filename_full = filename
  }
  if( !file.exists( filename_full ) ){
    warning( paste0("The file you're adding, ", filename_full, ", does not exist! Adding it anyway.\n") )
  }

  # case tag already present
  if( tag %in% inv$tag ){
    ii = which(inv$tag==tag)
    if( force ) {
      warning( paste0( "Overwriting a row that currently says\n",
                       paste0( inv[ii, ], collapse = " \n " ) ) )
      inv[ii, "parent_tag"]    = parent_tag
      inv[ii, "date_modified"] = format( Sys.time(), "%Y_%b_%d|%H_%M_%S")
      inv[ii, "filename"] = filename
      inv[ii, "extra"]    = extra
    } else {
      tag = rev( make.unique( c( inv$tag, tag ) ) )[1]
      warning( paste0( "That tag is already taken. Using ", tag, " instead." ) )
      row_add = data.frame( tag=tag,
                            parent_tag=parent_tag,
                            date_modified = format( Sys.time(), "%Y_%b_%d|%H_%M_%S"),
                            filename=filename,
                            extra=extra )
      print( inv )
      print( row_add )
      inv = rbind( inv, row_add )
    }

  # case tag not present
  } else {
    row_add = data.frame( tag=tag,
                          parent_tag=parent_tag,
                          date_modified = format( Sys.time(), "%Y_%b_%d|%H_%M_%S"),
                          filename=filename,
                          extra=extra )
    print( inv )
    print( row_add )
    inv = rbind( inv, row_add )
  }
  write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
}




#' Remove no-longer important items from the inventory.
#'
#' @export
#' @param inv_location Path to the inventory you want to create or modify. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @param tag identifier for an inventory record that you want to add.
#' @details \code{inventory_*} functions help track data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory_rm} will remove the row with the given tag.
#'
#' @export 
#'
inventory_rm = function( tag = NULL, inv_location = NULL ){

  if( !is.null( tag ) ) { assertthat::assert_that( tag!="" ) }

  inventory_path = inventory_find( inv_location )
  inv_location = dirname( inventory_path )
  inv = inventory_show( inv_location, make_new = T )

  given_tag = tag
  if( ! given_tag %in% inv$tag ){
    warning("Tag not present. No action taken.")
  }
  inv = subset( inv, tag != given_tag)
  write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
}


#' Retrieve (paths to) important items from previous analyses.
#'
#' @export
#' @param inv_location Path to the inventory you want to access. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @param tag identifier for an inventory record that you want to access.
#' @param return_all_fields When retrieving data, return the whole inventory record instead of just the \code{filename}.
#' @details \code{inventory_*} functions help organize data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory_get} looks for this table at the \code{inv_location} you specify. Any record matching
#'  the tag you give will be returned.
#'
inventory_get = function( tag = NULL, inv_location = NULL, return_all_fields = FALSE ){
  inventory_path = inventory_find( inv_location )
  inv = inventory_show( inv_location )

  ii = which(inv$tag==tag)
  if( length(ii) == 0 ){
    warning( "That tag is not present. Quitting." )
    return()
  } else if ( length(ii) > 1 ){
    warning( paste0( "Duplicate tag detected! This is not supposed to happen. ",
                     "If you can reproduce this issue without altering `.inventory.txt` by hand, ",
                     "please contact the package maintainer." ) )
  }

  myrow = inv[ii, ]
  relative = substring(myrow$filename, 1, 1)[[1]]!=.Platform$file.sep
  if(relative){
    myrow$filename = file.path( dirname( inventory_path ), myrow$filename )
  }

  if( return_all_fields ){
    return( myrow )
  } else {
    return( myrow$filename )

  }
}



#' Return an entire inventory.
#'
#' @export
#' @param make_new If \code{make_new==TRUE} and if \code{inv_location} is empty, a new inventory will be created.
#' @param inv_location Path to the inventory you want to create, access, or modify. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @details \code{inventory_*} functions help organize data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory_show} will return the whole table.
#'
inventory_show = function( inv_location = NULL, make_new = FALSE ){

  inventory_path = inventory_find( inv_location )

  # What if it's not there?
  if( !file.exists( inventory_path ) ){
    if( make_new ){
      cat("There is no inventory at that location. Making an empty one now. \n\n")
      inv = data.frame( tag=as.Date( character() ),
                        filename=character(),
                        parent_tag=character(),
                        date_modified=character(),
                        extra=character(),
                        stringsAsFactors=FALSE )
      write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
    } else{
      stop("There is no inventory at that location.")
    }
  }

  # # Guarantees that what you see is fresh off the hard disk.
  inv = read.table( inventory_path, header = T, sep = "\t", stringsAsFactors = F )
  return( inv )
}

#' Locate your inventory.
#'
#' @export
#' @param inv_location Path to the inventory you want to create, access, or modify. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @details \code{inventory_*} functions help organize data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory_find} helps locate the inventory.
#'
inventory_find = function( inv_location = NULL ){

  # # Defaults to last inv_location given to freezr::freeze
  if( is.null( inv_location ) ){
    if("FREEZR_DESTINATION" %in% names(Sys.getenv())){
      inv_location = dirname( dirname( Sys.getenv()[["FREEZR_DESTINATION"]] ) )
    } else {
      stop("Please enter inv_location; default not available.")
      return()
    }
  }

  inventory_path = file.path( inv_location, ".inventory.txt" )
  return( inventory_path )
}

## ------------------------------------------------------------------------
#' Keep track of important items from previous analyses.
#'
#' @export
#' @param inv_location Path to the inventory you want to create, access, or modify. If possible, this arg
#'  defaults to the parent of the last destination given to `freeze`.
#' @param tag identifier for an inventory record that you want to add, access, or modify.
#' @param parent_tag identifier for a file that this analysis depends on.
#' @param filename relative path from \code{inv_location} for a file that you want to add to the inventory.
#'  Used only when adding or modifying records.
#' @param extra Any character string without tabs. Meant for metadata to be associated with the given \code{tag}
#'  and \code{filename}.
#'  This may just be notes, or you could include an underscore- and pipe-delimited list of key-value pairs;
#'  the sky's the limit. Used only when adding or modifying records.
#' @param return_all When retrieving data, return the whole inventory record instead of just the \code{filename}.
#'  Default is \code{FALSE}.
#' @param delete Overwrite or delete any existing entry with the given tag. Default is \code{FALSE}.
#'
#' @details \code{inventory} is a general tool to organize data as it passes through multiple stages of analysis.
#'  The central data structure is a table with the filename \code{.inventory.txt}. It has five
#'  columns: \code{tag}, \code{parent_tag}, \code{date_modified}, \code{filename}, and \code{extra}.
#'
#' \code{inventory} looks for this table at the \code{inv_location} you specify and creates a new one if needed.
#' \code{inventory} can do three things:
#'  \itemize{
#'  \item{ \strong{Show the whole table} } { If no tag is given, \code{inventory} will return the table. }
#'  \item{ \strong{Retrieve (delete) a row} } { If there's a \code{tag} given, but no \code{filename}, any
#'  record matching that tag will be returned (or, if \code{delete==TRUE}, will be deleted). }
#'  \item{ \strong{Add (modify) a row} } { If both tag and filename are given, an entry with that tag
#'  will be added. If that tag is present already and \code{delete==TRUE}, the record will be
#'  altered. If the tag is present but \code{delete==FALSE}, the tag will be altered via \code{make.unique}
#'  and a new record will be created.}
#' }
#'
inventory = function( tag = NULL, inv_location = NULL, filename = NULL,
                      extra = "", parent_tag = "",
                      delete = FALSE, return_all = FALSE ){
  warning("inventory() is deprecated. 
          \nUse inventory_add, inventory_get, inventory_rm, inventory_show, or inventory_find. ")
  if( !is.null( tag ) ) { assertthat::assert_that( tag!="" ) }
  assertthat::assert_that( 0==length( grep( x=extra, pattern = "\t", fixed = T ) ) )

  # # Defaults to last inv_location given to freezr::freeze
  if( is.null( inv_location ) ){
    if("FREEZR_DESTINATION" %in% names(Sys.getenv())){
      inv_location = dirname( dirname( Sys.getenv()[["FREEZR_DESTINATION"]] ) )
    } else {
      cat("Please enter inv_location.")
      return()
    }
  }

  # # Find the inventory or make a new one.
  inventory_path = file.path( inv_location, ".inventory.txt" )
  if( !file.exists( inventory_path ) ){
    cat("There is no inventory at that location. Making an empty one now. \n\n")
    inv = data.frame( tag=as.Date( character() ),
                      filename=character(),
                      parent_tag=character(),
                      date_modified=character(),
                      extra=character(),
                      stringsAsFactors=FALSE )
    write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
  } else {
    inv = read.table( inventory_path, header = T, sep = "\t", stringsAsFactors = F )
  }

  # # If no tag is given, return the inventory.
  if(is.null(tag)){
    cat("Here is your current inventory.\n\n")
    return( inv )

    # # If a tag is given, but no filename, retrieve or delete the entry with that tag.
  } else if( is.null( filename ) ){
    ii = which(inv$tag==tag)
    if( length(ii) == 0 ){
      warning( "That tag is not present. Quitting." )
      return()
    } else if ( length(ii) > 1 ){
      warning( paste0( "Duplicate tag detected! This shouldn't happen. ",
                       "If you can reproduce this issue without altering `.inventory.txt` by hand, ",
                       "please file an issue on Github." ) )
    }
    if( delete ){
      inv = inv[-ii, ]
      write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
    } else {
      myrow = inv[ii, ]
      if( return_all ){
        return( myrow )
      } else {
        return( file.path( dirname(inventory_path), myrow$filename ) )
      }
    }

    # # If both tag and filename are given, add or modify the entry with that tag.
    # # As a courtesy, check whether filename exists.
  } else {
    filename_full = file.path( inv_location, filename )
    if( !file.exists( filename_full ) ){
      warning( paste0("The file you're adding, ", filename_full, ", does not exist! Adding it anyway.\n") )
    }

    # case tag already present
    if( tag %in% inv$tag ){
      ii = which(inv$tag==tag)
      if( delete ) {
        warning( paste0( "Overwriting a row that currently says",
                         paste0( inv[ii, ], collapse = " \n " ) ) )
        inv[ii, "parent_tag"]    = parent_tag
        inv[ii, "date_modified"] = format( Sys.time(), "%Y_%b_%d|%H_%M_%S")
        inv[ii, "filename"] = filename
        inv[ii, "extra"]    = extra
      } else {
        tag = rev( make.unique( c( inv$tag, tag ) ) )[1]
        warning( paste0( "That tag is already taken. Using ", tag, " instead." ) )
        row_add = data.frame( tag=tag,
                              parent_tag=parent_tag,
                              date_modified = format( Sys.time(), "%Y_%b_%d|%H_%M_%S"),
                              filename=filename,
                              extra=extra )
        print( inv )
        print( row_add )
        inv = rbind( inv, row_add )
      }

      # case tag not present
    } else {
      row_add = data.frame( tag=tag,
                            parent_tag=parent_tag,
                            date_modified = format( Sys.time(), "%Y_%b_%d|%H_%M_%S"),
                            filename=filename,
                            extra=extra )
      print( inv )
      print( row_add )
      inv = rbind( inv, row_add )
    }
    write.table( inv, inventory_path, quote = F, row.names = F, col.names = T, sep = "\t" )
  }
}

