test_that("app_sys resolves paths inside the installed package", {
  root <- app_sys()
  expect_type(root, "character")
  expect_true(nzchar(root))
  expect_true(dir.exists(root))
  expect_equal(app_sys("DESCRIPTION"),
               system.file("DESCRIPTION", package = "stats2data"))
  expect_equal(app_sys("no_such_file_here"), "")
})

test_that("get_golem_config reads the packaged configuration", {
  skip_if_not_installed("config")
  skip_if(!nzchar(app_sys("golem-config.yml")))
  expect_equal(get_golem_config("golem_name"), "stats2data")
  expect_type(get_golem_config("app_prod"), "logical")
})

test_that("get_golem_config honours the active configuration", {
  skip_if_not_installed("config")
  skip_if(!nzchar(app_sys("golem-config.yml")))
  expect_true(get_golem_config("app_prod", config = "production"))
})
