# ## Load your packages, e.g. library(targets).
 source("./packages.R")

 tar_option_set(
   controller = crew_controller_local(workers = 86)
 )
#options(clustermq.scheduler = "multicore")
# ## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

#mi_ex <- reproj::reproj_extent(c(158.75, 158.97, -54.8, -54.45), mi_crs, source = "EPSG:4326")

## MacqIsland
#mi_ex <- c(48232, 62918, 21833, 60965)
#mi_ex <- c(48000, 63000, 50000, 61000)
#loc_crs <- "+proj=laea +lon_0=158 +lat_0=-55"
#loc_ex <- c(54500, 63000, 50000, 58800)

loc_crs <- "+proj=laea +lon_0=73.7 +lat_0=-53.14"
loc_ex <- c(-1, 1, -1, 1) * 5000
#ex <-
#dm <- as.integer(diff(loc_ex)[c(1, 3)]/10)

# im <- vapour::gdal_raster_nara(sds::wms_googlehybrid_tms(), target_ext= loc_ex, target_crs = loc_crs,
#                        target_dim = dm)
#tar_load(spec)
# i <- 1
#
# i <- i + 1
# im <- vapour::gdal_raster_nara(srcs$visual[i], target_ext= spec$ex, target_crs = spec$crs,
#                                target_dim = spec$dimension)
# ximage::ximage(im, asp = 1)


list(
  #tar_target(ex, c(-64, -63.5, -9, -8.5)),
  tar_target(dir, {dir.create("outputs", showWarnings = F); "outputs"}, format = "file"),
  #tar_target(ex, c(73.245288, 73.78457, -53.238303, -52.914329)),
  #tar_target(ex, c(73.6, 73.8, -53.18, -53.10)),
  tar_target(ex, reproj::reproj_extent(loc_ex, "EPSG:4326", source = loc_crs)),
  tar_target(localnoon, ex[1]/15),
  tar_target(dm, as.integer(diff(loc_ex)[c(1, 3)]/10)),
#  tar_target(srcs, hrefs(stacit(ex, date = as.Date(c("2023-01-01-", "2024-05-15"))))),
tar_target(query, stacit(ex, date = c("2024-02-01", "2024-02-29"))),
tar_target(srcs, hrefs(query)),
tar_target(properties, props(query)),
  tar_target(spec0, list(dimension = dm, ex = loc_ex, crs = loc_crs)),
  #
  #   ## we need a template, these might be different so we normalize upfront
  tar_target(ref, warpfun(srcs$aot[1], ext = spec0$ex, dim = spec0$dimension, crs = spec0$crs)),
  tar_target(spec, list(ex = attr(ref, "gis")$bbox[c(1, 3, 2, 4)],
                        dimension = attr(ref, "gis")$dim[1:2], crs = attr(ref, "gis")$srs)),
  tar_target(red, srcs$red),
  tar_target(green, srcs$green),
  tar_target(blue, srcs$blue),
  tar_target(cloud, srcs$cloud),
  tar_target(irow, 1:nrow(srcs)),
  tar_target(path, file.path(dir, sprintf("%06i.parquet", irow))),
  tar_target(files, sclfun(red, green, blue, cloud, ext = spec$ex,
                       crs = spec$crs, dim = spec$dimension[1:2], path = path, i = irow), format = "file",
             pattern = map(red, green, blue, cloud, irow, path)),
  tar_target(nn, c(min(c(nrow(srcs), parallel::detectCores() %/% 2)))),
  #tar_target(masked, calc_med0(files, new_cluster(nn))),
  tar_target(masked, calc_med(files)),  ## DUCKDBFS
  tar_target(scaled, scale_image(masked, spec)),
  tar_target(figure, create_figure(scaled)),
  tar_target(ord, order(file.info(files)$size, decreasing = TRUE)))


