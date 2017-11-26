# Clean up from last time
delete_me = list.files( file.path(getwd(), "results"), full.names = T )
delete_me = delete_me[ !grepl(x = delete_me, pattern = "*inventory_guinea_pig*") ]
cat("These files will be cleaned up from past tests:\n", paste0(delete_me, collapse = "\n"), "\n\n")
unlink( delete_me, recursive = TRUE )

# Basic test
test_dest = freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                    destination = file.path(getwd(), "results") )

# Test with a script that throws an error
Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::freeze( analyses_to_run = c( "script_that_fails.R" ),
                destination = file.path(getwd(), "results") )

# Test with code files further down in the working directory
my_basename = basename(getwd())
setwd("..")
Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
test_dest = freezr::freeze( analyses_to_run = file.path( my_basename, c( "functions_to_freeze.Rmd" ) ),
                            destination = file.path( getwd(), my_basename, "results" ) )
setwd( file.path( getwd(), my_basename ) )

# Test with dependency listed
Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                destination = file.path(getwd(), "results"),
                dependencies = "dependency.txt")

Sys.sleep(2)
test_that("Tracking of a nonexistent git repo yields a warning.", {
  expect_warning(
    freezr::freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                    destination = file.path(getwd(), "results"), other_repos_to_track = "~")
  )
})



