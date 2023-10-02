compute_zonal_tasks <- function(zonal_tasks, db_dir){
  # # progressr enable
  # options(progressr.enable=TRUE)
  #
  # # Set progress bar format
  # progressr::handlers(list(
  #   progressr::handler_progress(
  #     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
  #     clear = FALSE
  #   )
  # ))

  # Delete database if exists
  if(file.exists(db_dir)) unlink(db_dir)

  # Starting message
  usethis::ui_info("Starting...")

  # Plan parallel session
  # if(interactive()){
  #   future::plan(future::multisession, workers = 4)
  # } else (
  #   future::plan(future::multicore, workers = 4)
  # )

  future::plan(future::multisession, workers = 4)


  tictoc::tic()
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(zonal_tasks))
    result <- furrr::future_pmap(
      .l = list(
        rst = zonal_tasks$rst,
        pol = zonal_tasks$geom,
        fn_name = zonal_tasks$fn,
        db_dir = db_dir
      ),
      .f = zonalclim::compute_tasks,
      .options = furrr::furrr_options(seed = TRUE),
      p = p
    )
  })
  tictoc::toc()

  # tictoc::tic()
  # result <- purrr::pmap(
  #   .l = list(
  #     rst = zonal_tasks$rst,
  #     pol = zonal_tasks$geom,
  #     fn_name = zonal_tasks$fn,
  #     db_dir = db_dir
  #   ),
  #   .f = zonalclim::compute_tasks
  # )
  # tictoc::toc()

}
