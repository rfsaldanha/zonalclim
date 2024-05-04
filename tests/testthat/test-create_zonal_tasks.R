test_that("create_zonal_tasks works", {
  nc_list <- system.file("extdata", "2m_temperature_2000-01-01_2000-01-31_day_max.nc", package="zonalclim")
  sf_geom <- gadm41_moz
  zonal_list <- c("mean", "max", "min", "stdev")

  tmp <- create_zonal_tasks(
    nc_files_list = nc_list,
    nc_chunk_size = 10,
    sf_geom = sf_geom,
    sf_chunck_size = 10,
    zonal_functions = zonal_list)

  expect_equal(nrow(tmp), 16)
})

test_that("create_zonal_tasks works with pop", {
  testthat::skip()

  nc_list <- system.file("extdata", "2m_temperature_2000-01-01_2000-01-31_day_max.nc", package="zonalclim")
  sf_geom <- gadm41_moz
  zonal_list <- c("mean", "max", "min", "stdev")
  pop <- "/media/raphael/lacie/worldpop/brazil/count/bra_ppp_2000.tif"

  tmp <- create_zonal_tasks(
    nc_files_list = nc_list,
    nc_chunk_size = 10,
    sf_geom = sf_geom,
    sf_chunck_size = 10,
    zonal_functions = zonal_list,
    pop = pop)

  expect_equal(nrow(tmp), 16)
})
