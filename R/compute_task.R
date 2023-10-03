#' Compute tasks
#'
#' @param rst
#' @param pol
#' @param fn_name
#' @param db_file
#' @param p
#'
#' @return
#' @export
compute_task <- function(rst, pol, g_var, fn_name, db_file, p = NULL){

  # rst <- zonal_tasks$rst[[2]]
  # pol <- zonal_tasks$geom[[2]]
  # fn_name <- zonal_tasks$fn[[2]]
  # db_file <- tempfile(fileext = ".sqlite")


  # Progress bar update
  if(!is.null(p)){
    p()
  }

  # Set gdal cache
  terra::gdalCache(15000)

  # Calculate zonal fn_name
  names(rst) <- terra::time(rst)
  tmp <- exactextractr::exact_extract(
    x = rst,
    y = pol,
    fun = fn_name,
    progress = FALSE
  )

  # Change names
  names(tmp) <- paste0("date_", as.Date(terra::time(rst)))

  # Data structure
  tmp <- dplyr::bind_cols(!!dplyr::sym(g_var) := get(g_var, pol), tmp) %>%
    tidyr::pivot_longer(!(!!dplyr::sym(g_var))) %>%
    dplyr::rename(date = name) %>%
    dplyr::mutate(
      date = as.Date(substr(date, 6, 15)),
      name = paste0(terra::varnames(rst)[1], "_", fn_name)
    ) %>%
    dplyr::relocate(name, .before = value)

  # Write to database
  conn = DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE, synchronous = NULL)
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "BEGIN IMMEDIATE TRANSACTION")
  DBI::dbWriteTable(conn = conn, name = terra::varnames(rst)[1], value = tmp, append = TRUE)
  DBI::dbExecute(conn, "COMMIT TRANSACTION")

  # Return true
  return(TRUE)
}
