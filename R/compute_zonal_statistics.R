#' Compute zonal tasks
#'
#' @param zonal_tasks A tibble of zonal tasks created by `create_zonal_tasks`
#' @param g_var Variable name (string) that unique identifies each feature at the sf object.
#' @param db_file A path and file name for the SQLite database. If already exists, it will be deleted.
#' @param cores Number of cores to use.
#'
#' @return An SQLite database.
#' @export
compute_zonal_tasks <- function(zonal_tasks, g_var, db_file, cores = 1){
  # Delete database if exists
  if(file.exists(db_file)) unlink(db_file)

  # Starting message
  usethis::ui_info("Starting...")

  tictoc::tic()
  # Single core or parallel
  if(cores == 1){

    result <- purrr::pmap(
      .l = list(
        rst = zonal_tasks$rst,
        pol = zonal_tasks$geom,
        fn_name = zonal_tasks$fn,
        db_file = db_file,
        g_var = g_var
      ),
      .f = compute_task,
      .progress = TRUE
    )
  } else {
    # progressr enable
    options(progressr.enable=TRUE)

    # Set progress bar format
    progressr::handlers(list(
      progressr::handler_progress(
        format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        clear = FALSE
      )
    ))

    # Plan parallel session
    future::plan(future::multicore, workers = cores)

    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(zonal_tasks))
      result <- furrr::future_pmap(
        .l = list(
          rst = zonal_tasks$rst,
          pol = zonal_tasks$geom,
          fn_name = zonal_tasks$fn,
          db_file = db_file,
          g_var = g_var
        ),
        .f = compute_task,
        .options = furrr::furrr_options(seed = TRUE),
        p = p
      )
    })
  }
  usethis::ui_done("Done!")
  tictoc::toc()

  return(result)
}
