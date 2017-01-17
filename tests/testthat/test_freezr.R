freezr::freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                destination = file.path(getwd(), "results") )

Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
                destination = file.path(getwd(), "results"),
                dependencies = "dependency.txt")

Sys.sleep(2) #time stamps have a 1-second resolution, so need this to avoid collisions.
freezr::freeze( analyses_to_run = c( "script_that_fails.R" ),
                destination = file.path(getwd(), "results") )
