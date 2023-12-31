#' Create tasks of zonal statistics
#'
#' @param nc_files_list A vector of NetCDF files addresses.
#' @param nc_chunk_size Chunk size for raster layers processing.
#' @param sf_geom An sf object.
#' @param sf_chunck_size Chunk size for sf features processing.
#' @param zonal_functions A vector of zonal statistics to be computed.
#' @param pop optional. A vector of a population raster address.
#'
#' @return A tibble where each row is a zonal statistic that should be computed using a chunk of raster layers and a chunk of sf features.
#' @export
create_zonal_tasks <- function(nc_files_list, nc_chunk_size, sf_geom, sf_chunck_size, zonal_functions, pop = NULL){
  # Read nc files
  nc_data <- terra::rast(x = nc_files_list)

  # Retrieve aggregation function from file name
  agg_fun_file_name <- stringr::str_extract(string = nc_files_list, "[^_]+$") %>%
    stringr::str_remove(".nc$")

  # Add aggregation function to SpatRaster varname propertie
  terra::varnames(nc_data) <- paste0(terra::varnames(nc_data), "_", agg_fun_file_name)

  # Split nc_data into chunks
  nc_data_chunks <- terra::split(
    x = nc_data,
    f = (seq(terra::nlyr(nc_data))-1) %/% nc_chunk_size + 1
  )

  # Split sf_geom into chuncks
  sf_geom <- sf::st_as_sf(sf_geom)
  n_row <- nrow(sf_geom)
  sf_geom_chunks <- sf_geom %>%
    dplyr::mutate(chunk = (seq(n_row)-1) %/% sf_chunck_size + 1) %>%
    dplyr::group_split()

  # Create task lists with nc chuncks, sf chunks and zonal statistical functions
  zonal_tasks <- tidyr::expand_grid(
    rst = nc_data_chunks,
    geom = sf_geom_chunks,
    fn = zonal_functions
  )

  # If pop exists, add to task
  if(!is.null(pop)){
    pop_rst <- list(terra::rast(x = pop))

    zonal_tasks <- tidyr::expand_grid(
      rst = nc_data_chunks,
      geom = sf_geom_chunks,
      fn = zonal_functions,
      pop_rst = pop_rst
    )
  }

  # Return tasks
  return(zonal_tasks)
}
