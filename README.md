---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# zonalclim

<!-- badges: start -->
[![R-CMD-check](https://github.com/rfsaldanha/zonalclim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/zonalclim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R package presents helper functions to compute zonal statistics using NetCDF and sf objects for climate studies. 

Zonal statistics is a Geographic Information System (GIS) method to compute a summary statistic (like mean, standard deviation, or sum) of a spatially contiguous indicator for a given boundary. This is especially useful to compute climate indicators spatially aggregated to geographical boundaries, like counties or municipalities.

This package relies on the [exactextractr](https://cran.r-project.org/package=exactextractr) package to compute the zonal statistics, providing helper functions to compute statistics when you have large NetCDF files with layers and several geographical boundaries.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("rfsaldanha/zonalclim")
```

## Usage

To handle large NetCDF files and several geographical boundaries, the package presents a function to create chunks of tasks. For example, we can compute some zonal statistics using the package example dataset.


``` r
library(zonalclim)
nc_list <- system.file("extdata", "2m_temperature_2000-01-01_2000-01-31_day_max.nc", package="zonalclim")
sf_geom <- gadm41_moz
zonal_list <- c("mean", "max", "min", "stdev")
```

The `nc_list` object is a path to a NetCDF file, with layers for different dates for an indicator (maximum temperature from the Copernicus ERA5-Land on Mozambique). This can also be a list of NetCDF files with this structure. The `sf_geom` contains the geographical boundaries (level 3) and `zonal_list` is a vector of summary functions to be computed.

### Zonal tasks

When handling a large number of NetCDF files, each one with a large number of layers, the load to compute zonal statistics can be larger than the available computational capacity. In this situation, we can split the computational task into smaller tasks, using the `zonal_tasks()` function.


``` r
zonal_tasks <- create_zonal_tasks(
  nc_files_list = nc_list,
  nc_chunk_size = 5,
  sf_geom = sf_geom,
  sf_chunck_size = 5,
  zonal_functions = zonal_list
)
```

The function will split the NetCDF file(s) and sf contents into chunks of tasks, based on the arguments `nc_chunk_size` and `sf_chunck_size`. The key is finding a **balance** between chunk sizes, speed, and computational load. Less but larger chunks are faster to compute than several smaller chunks but demand more available memory.


``` r
zonal_tasks
#> # A tibble: 28 × 3
#>    rst                geom           fn   
#>    <list>             <list>         <chr>
#>  1 <SpatRstr[,111,5]> <sf [413 × 4]> mean 
#>  2 <SpatRstr[,111,5]> <sf [413 × 4]> max  
#>  3 <SpatRstr[,111,5]> <sf [413 × 4]> min  
#>  4 <SpatRstr[,111,5]> <sf [413 × 4]> stdev
#>  5 <SpatRstr[,111,5]> <sf [413 × 4]> mean 
#>  6 <SpatRstr[,111,5]> <sf [413 × 4]> max  
#>  7 <SpatRstr[,111,5]> <sf [413 × 4]> min  
#>  8 <SpatRstr[,111,5]> <sf [413 × 4]> stdev
#>  9 <SpatRstr[,111,5]> <sf [413 × 4]> mean 
#> 10 <SpatRstr[,111,5]> <sf [413 × 4]> max  
#> # ℹ 18 more rows
```

In this example, the function combined the 31 raster layers, 413 spatial boundaries, and the four statistics to be computed into 28 computational tasks. Bigger chunk sizes will lead to fewer but heavier tasks.

### Compute tasks

To compute these tasks, use the `compute_zonal_tasks()` function. This function will compute the tasks (sequentially) and store their results in a SQLite database.


``` r
db_file <- tempfile(fileext = ".sqlite")
```


``` r
res <- compute_zonal_tasks(
  zonal_tasks = zonal_tasks,
  g_var = "GID_3",
  db_file = db_file
)
#> ℹ Starting...
#>  ■■■■■■■■■■■                       32% |  ETA:  2s ■■■■■■■■■■■■                      36% |  ETA:  2s ■■■■■■■■■■■■■■■                   46% |  ETA:  2s ■■■■■■■■■■■■■■■■                  50% |  ETA:  2s ■■■■■■■■■■■■■■■■■■■■■■            71% |  ETA:  1s ■■■■■■■■■■■■■■■■■■■■■■■■■         79% |  ETA:  1s ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% |  ETA:  0sNew names:New names: ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% |  ETA:  0sNew names:New names:                                                  ℹ Done!
#> 3.024 sec elapsed
```


Let's check the results.


``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE)
tables <- DBI::dbListTables(conn)
res2 <- dplyr::tbl(conn, tables[1]) %>% dplyr::collect()
```


``` r
res2
#> # A tibble: 51,212 × 4
#>    GID_3       date       name                    value
#>    <chr>       <date>     <chr>                   <dbl>
#>  1 MOZ.1.1.1_1 2000-01-01 2m_temperature_max_mean  306.
#>  2 MOZ.1.1.1_1 2000-01-02 2m_temperature_max_mean  305.
#>  3 MOZ.1.1.1_1 2000-01-03 2m_temperature_max_mean  299.
#>  4 MOZ.1.1.1_1 2000-01-04 2m_temperature_max_mean  302.
#>  5 MOZ.1.1.1_1 2000-01-05 2m_temperature_max_mean  304.
#>  6 MOZ.1.1.2_1 2000-01-01 2m_temperature_max_mean  306.
#>  7 MOZ.1.1.2_1 2000-01-02 2m_temperature_max_mean  305.
#>  8 MOZ.1.1.2_1 2000-01-03 2m_temperature_max_mean  300.
#>  9 MOZ.1.1.2_1 2000-01-04 2m_temperature_max_mean  302.
#> 10 MOZ.1.1.2_1 2000-01-05 2m_temperature_max_mean  304.
#> # ℹ 51,202 more rows
```

