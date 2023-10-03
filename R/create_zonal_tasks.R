create_zonal_tasks <- function(nc_files_list, nc_chunk_size, sf_geom, sf_chunck_size, zonal_functions){
  # Read nc files
  nc_data <- terra::rast(x = nc_files_list)

  # Retrieve aggregation function from file name
  agg_fun_file_name <- stringr::str_extract(string = nc_list, "[^_]+$") %>%
    stringr::str_remove(".nc$")

  # Add aggregation function to SpatRaster varname propertie
  terra::varnames(nc_data) <- paste0(terra::varnames(nc_data), "_", agg_fun_file_name)

  # Split nc_data into chunks
  nc_data_chunks <- terra::split(
    x = nc_data,
    f = (seq(terra::nlyr(nc_data))-1) %/% nc_chunk_size + 1
  )

  # Split sf_geom into chuncks
  sf_geom_chunks <- sf_geom %>%
    dplyr::mutate(chunk = (seq(nrow(.))-1) %/% sf_chunck_size + 1) %>%
    dplyr::group_split()

  # Create task lists with nc chuncks, sf chunks and zonal statistical functions
  zonal_tasks <- tidyr::expand_grid(
    rst = nc_data_chunks,
    geom = sf_geom_chunks,
    fn = zonal_functions
  )

  # Return tasks
  return(zonal_tasks)
}