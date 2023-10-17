test_that("compute_zonal_tasks works", {
  nc_list <- system.file("extdata", "2m_temperature_2000-01-01_2000-01-31_day_max.nc", package="zonalclim")
  sf_geom <- gadm41_moz
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
    db_file = db_file
  )

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE)
  tables <- DBI::dbListTables(conn)
  res2 <- dplyr::tbl(conn, tables[1]) %>% dplyr::collect()
  length(unique(res2$date))
  DBI::dbDisconnect(conn)

  expect_true(file.exists(db_file))
  expect_equal(length(unique(res2$date)), 31)
})



test_that("compute_zonal_tasks works with pop", {
  if(FALSE) dbplyr::translate_sql()

  nc_list <- system.file("extdata", "2m_temperature_2000-01-01_2000-01-31_day_max.nc", package="zonalclim")
  sf_geom <- gadm41_moz
  zonal_list <- c("weighted_mean")
  pop <- "/media/raphael/lacie/worldpop/mozambique/count/moz_ppp_2000.tif"

  zonal_tasks <- create_zonal_tasks(
    nc_files_list = nc_list,
    nc_chunk_size = 50,
    sf_geom = sf_geom,
    sf_chunck_size = 50,
    pop = pop,
    zonal_functions = zonal_list
  )

  db_file <- tempfile(fileext = ".sqlite")

  res <- compute_zonal_tasks(
    zonal_tasks = zonal_tasks,
    g_var = "GID_3",
    db_file = db_file
  )

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE)
  tables <- DBI::dbListTables(conn)
  res2 <- dplyr::tbl(conn, tables[1]) %>% dplyr::collect()
  length(unique(res2$date))
  DBI::dbDisconnect(conn)

  expect_true(file.exists(db_file))
  expect_equal(length(unique(res2$date)), 31)
})
