freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
        destination = file.path(getwd(), "results") )

Sys.sleep(2) #time stamps have a 1-second resolution.
freeze( analyses_to_run = c( "functions_to_freeze.Rmd", "script_to_freeze.R" ),
        destination = file.path(getwd(), "results"),
        dependencies = "dependency.txt")
