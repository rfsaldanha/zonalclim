#' Compute zonal tasks
#'
#' @param zonal_tasks A tibble of zonal tasks created by `create_zonal_tasks`
#' @param g_var Variable name (string) that unique identifies each feature at the sf object.
#' @param db_file A path and file name for the SQLite database.
#'
#' @return An SQLite database.
#' @export
compute_zonal_tasks <- function(zonal_tasks, g_var, db_file){
  # Delete database if exists
  if(file.exists(db_file)) unlink(db_file)


  if(!("pop_rst" %in% names(zonal_tasks))){
    zonal_tasks_list <- list(
      rst = zonal_tasks$rst,
      pol = zonal_tasks$geom,
      fn_name = zonal_tasks$fn,
      db_file = db_file,
      g_var = g_var
    )
  } else {
    zonal_tasks_list <- list(
      rst = zonal_tasks$rst,
      pol = zonal_tasks$geom,
      fn_name = zonal_tasks$fn,
      pop = zonal_tasks$pop_rst,
      db_file = db_file,
      g_var = g_var
    )
  }

  # Starting message
  cli::cli_alert_info("Starting...")

  tictoc::tic()
  result <- purrr::pmap(
    .l = zonal_tasks_list,
    .f = compute_task,
    .progress = TRUE
  )
  cli::cli_alert_info("Done!")
  tictoc::toc()

  return(result)
}
