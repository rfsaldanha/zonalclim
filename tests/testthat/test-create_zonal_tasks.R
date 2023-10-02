test_that("create_zonal_tasks works", {
  nc_list <- list.files(path = "~/Downloads/eradata/", full.names = TRUE)
  sf_geom <- geobr::read_municipality(code_muni = "RJ")
  zonal_list <- c("mean")

  tmp <- create_zonal_tasks(
    nc_files_list = nc_list,
    nc_chunk_size = 10,
    sf_geom = sf_geom,
    sf_chunck_size = 10,
    zonal_functions = zonal_list)

  expect_equal(nrow(tmp), 13)
})
