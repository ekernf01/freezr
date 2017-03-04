# # Cleanup from previous testing
suppressWarnings( file.remove( file.path( dirname( dirname( Sys.getenv()[["FREEZR_DESTINATION"]] ) ), ".inventory.txt") ) )

# # Testing
f1 = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
freezr::inventory()
# Test with essential args
freezr::inventory( tag="my_dummy_output", filename=f1 )
# Test with parent_tag arg
freezr::inventory( tag="my_dummy_output_3", filename = f1, parent_tag = "my_dummy_output_2" )
# Test with incorrect arg
freezr::inventory( tag="this_tag_doesnt_exist", delete = T )
# Test with extra arg
freezr::inventory( tag="my_dummy_output_2", filename=f1, extra = paste( "This file is just some dummy output",
                                                                        "but if it were from one of my scientific projects",
                                                                        "I would ideally write about its origin, the processing it ",
                                                                        "has undergone, and its purpose." ) )
f1 = file.path( getwd(), "tests", "testthat", "results", f1 )
assertthat::are_equal(f1, inventory( tag = "my_dummy_output" ))
assertthat::are_equal(f1, inventory( tag = "my_dummy_output_2" ))
assertthat::are_equal(f1, inventory( tag = "my_dummy_output_3" ))
