## code to prepare `gadm41_moz` dataset goes here

gadm41_moz <- sf::st_read("../brclim/utils/gadm41_MOZ_shp/gadm41_MOZ_3.shp") %>%
  dplyr::select(GID_3, NAME_3)

usethis::use_data(gadm41_moz, overwrite = TRUE, compress = "xz")
