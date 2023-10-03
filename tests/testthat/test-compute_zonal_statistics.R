test_that("compute_zonal_tasks works", {
  # nc_list <- list.files(path = "~/Downloads/eradata/", full.names = TRUE)[1]
  # sf_geom <- geobr::read_municipality(code_muni = "RJ")
  # zonal_list <- c("mean")

  nc_list <- list.files(path = "../era5daily/era5_data_moz/", pattern = "^2m_temperature.*\\mean.nc$", full.names = TRUE)[1:10]
  sf_geom <- readRDS(file = "../brclim/utils/moz3.rds")
  zonal_list <- c("mean", "max", "min", "stdev")

  zonal_tasks <- create_zonal_tasks(
    nc_files_list = nc_list,
    nc_chunk_size = 50,
    sf_geom = sf_geom,
    sf_chunck_size = 50,
    zonal_functions = zonal_list
  )

  db_file <- tempfile(fileext = ".sqlite")

  res <- compute_zonal_tasks(
    zonal_tasks = zonal_tasks,
    g_var = "GID_3",
    db_file = db_file,
    cores = 1
  )

  expect_true(file.exists(db_file))
})
