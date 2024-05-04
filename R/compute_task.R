#' Compute a zonal task
#'
#' @param rst The rst variable from a zonal tasks tibble
#' @param pol The pol variable from a zonal tasks tibble
#' @param pop Optional. Population weighting raster
#' @param g_var Variable name (string) that unique identifies each feature at the sf object.
#' @param fn_name The fn_name variable from a zonal tasks tibble
#' @param db_file Path and file name to SQLite database.
#'
#' @return Writes the results of the zonal taks into the SQLite database and return a TRUE value.
#' @export
#'
#' @importFrom rlang :=
compute_task <- function(rst, pol, pop = NULL, g_var, fn_name, db_file){

  # Set gdal cache
  terra::gdalCache(15000)

  # Set epsg
  terra::crs(rst) <- "epsg:4326"

  # Calculate zonal fn_name
  names(rst) <- terra::time(rst)
  if(is.null(pop)){
    tmp <- exactextractr::exact_extract(
      x = rst,
      y = pol,
      fun = fn_name,
      progress = FALSE
    )
  } else {
    tmp <- exactextractr::exact_extract(
      x = rst,
      y = pol,
      fun = fn_name,
      weights = pop,
      coverage_area = FALSE,
      progress = FALSE
    )
  }


  # Change names
  names(tmp) <- paste0("date_", as.Date(terra::time(rst)))

  # Data structure
  tmp <- dplyr::bind_cols(!!dplyr::sym(g_var) := get(g_var, pol), tmp) %>%
    tidyr::pivot_longer(!(!!dplyr::sym(g_var))) %>%
    dplyr::rename(date = "name") %>%
    dplyr::mutate(
      date = as.Date(substr(date, 6, 15)),
      name = paste0(terra::varnames(rst)[1], "_", fn_name)
    ) %>%
    dplyr::relocate("name", .before = "value")

  # Write to database

  # Extend types requires the hms package. The line below simulates its use to allow include it as a package dependency
  if(FALSE) hms::hms()
  if(FALSE) dbplyr::db_collect()

  conn = DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE, synchronous = NULL)
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "BEGIN IMMEDIATE TRANSACTION")
  DBI::dbWriteTable(conn = conn, name = terra::varnames(rst)[1], value = tmp, append = TRUE)
  DBI::dbExecute(conn, "COMMIT TRANSACTION")

  # Return true
  return(TRUE)
}
