# # Cleanup from previous testing

suppressWarnings( file.remove( file.path( dirname( dirname( Sys.getenv()[["FREEZR_DESTINATION"]] ) ), ".inventory.txt") ) )

# # Testing

freezr::inventory()


freezr::inventory( tag="my_dummy_output",
                   filename=file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" ),
                   extra = paste( "This file is just some dummy output",
                                  "but if it were from one of my scientific projects",
                                  "I would ideally write about its origin, the processing it ",
                                  "has undergone, and its purpose." ) )

