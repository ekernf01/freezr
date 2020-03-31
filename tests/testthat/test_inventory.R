# # Helpful stuff # #

context("inventory")

# Files to use as guinea pigs
f1 = f1_rel = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
file_no_ext_rel = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
folder_rel      = file.path( "inventory_guinea_pig", "user", "file_without_extension" )
results_path = file.path("", "Users", "erickernfeld", "Desktop",
                         "software_projects", "freezr",
                         "tests", "testthat", "results")
f1_abs = file.path( results_path, f1 )

# Cleanup procedure from previous testing
clean_up_inv = function(){
  if(inventory_exists(f1_abs)){
    file.remove( inventory_find(f1_abs) )
  }
  if(inventory_exists(dirname(f1_abs))){
    file.remove( inventory_find(f1_abs) )
  }
}

# # New functions # #
test_that( "inventory_show errs when inv absent but works and warns with make_new", {
  clean_up_inv()
  expect_error(inventory_show(inv_location = dirname( f1_abs )))
  expect_warning( inventory_show(make_new = TRUE, inv_location = dirname( f1_abs ) ) )
  clean_up_inv()
} )

test_that( "make works", {
  clean_up_inv()
  expect_error(inventory_make())
  inventory_make( results_path )
  clean_up_inv()
})

test_that( "find works", {
  clean_up_inv()
  inventory_make( results_path )
  testthat::expect_true(inventory_exists(        f1_abs))
  testthat::expect_true(inventory_exists(dirname(f1_abs)))
  inventory_find( results_path )
  clean_up_inv()
  testthat::expect_false(inventory_exists(        f1_abs))
  testthat::expect_false(inventory_exists(dirname(f1_abs)))
  clean_up_inv()
})

test_that( "add works after make", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_find( results_path )
  inventory_add( tag="my_dummy_dupe_tag", filename=f1, inv_location = results_path )
  expect_warning( inventory_add( tag="my_dummy_dupe_tag", filename=f1, inv_location = results_path ) )
  clean_up_inv()
})


test_that( "save_and_add works after make", {
  clean_up_inv()
  inventory_make( results_path )
  demo_df = data.frame(LETTERS)
  demo_mat = matrix(LETTERS)
  inventory_save_and_add( demo_df,  extra = "Dataframe containing the alphabet.", inv_location = results_path )
  inventory_save_and_add( demo_mat, extra = "Matrix containing the alphabet.", inv_location = results_path )
  clean_up_inv()
})

test_that( "get works with incorrect arg", {
  clean_up_inv()
  inventory_make( results_path )
  expect_warning(inventory_get( tag="this_tag_doesnt_exist", inv_location = results_path ))
  clean_up_inv()
})

test_that( "get works with correct args", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="my_dummy_output_2", inv_location = results_path,
                 filename = f1,
                 parent_tag = "my_dummy_output_2",
                 extra = paste( "This file is just some dummy output",
                                "but if it were from one of my scientific projects",
                                "I would ideally write about its origin, the processing it ",
                                "has undergone, and its purpose."  ))
  testthat::expect_equal(f1_abs, inventory_get( tag = "my_dummy_output_2", inv_location = results_path ))
  clean_up_inv()
})

test_that( "add/get interface properly with absolute filepaths", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="my_absolute_input", filename = f1_abs )
  expect_equal(f1_abs, inventory_get( tag = "my_absolute_input" ))
  clean_up_inv()
})

test_that( "_transfer works for folders and for files with and without extensions.", {
  transferred_inv_path = file.path( dirname( results_path ), "transferred_inv" )
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="f1_rel",      filename = f1_rel,          inv_location = results_path )
  inventory_add( tag="file_no_ext", filename = file_no_ext_rel, inv_location = results_path )
  inventory_add( tag="folder",      filename = folder_rel,      inv_location = results_path )
  unlink(transferred_inv_path, recursive = T)
  inventory_transfer( target_location = transferred_inv_path, overwrite = F, inv_location = results_path )
  expect_warning( inventory_transfer( target_location = transferred_inv_path, overwrite = T, inv_location = results_path ),
                  regexp = "*already exists*")
  clean_up_inv()
})
clean_up_inv()

unlink(results_path, recursive = T)
