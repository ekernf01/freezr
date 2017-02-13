# Basic test
test_dest = freezr::freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                            destination = file.path(getwd(), "results") )

# Test with a script that throws an error
Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::freeze( analyses_to_run = c( "script_that_fails.R" ),
                destination = file.path(getwd(), "results") )

# Basic thaw test
Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::thaw( test_dest )

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




