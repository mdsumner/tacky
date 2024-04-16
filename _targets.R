# ## Load your packages, e.g. library(targets).
 source("./packages.R")
#
options(clustermq.scheduler = "multicore")
# ## Load your R files
lapply(list.files("./R", full.names = TRUE), source)


list(
  tar_target(ex, c(-64, -63.5, -9, -8.5)),
  tar_target(srcs, hrefs(stacit(ex, date = "2020-01"))),

  tar_target(spec0, list(dimension = c(1280, 0), ex = ex, crs = "EPSG:4326")),
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
  tar_target(path, sprintf("outputs/%06i.parquet", irow)),
  tar_target(files, sclfun(red, green, blue, cloud, ext = spec$ex,
                       crs = spec$crs, dim = spec$dimension[1:2], path = path, i = irow), format = "file",
             pattern = map(red, green, blue, cloud, irow, path)),
  tar_target(nn, c(min(c(nrow(srcs), parallel::detectCores() %/% 2)))),
  tar_target(res, calc_med(files, new_cluster(nn)))
)


# list(
#   tar_target(red, srcs$red),
#   tar_target(path, unlist(replicate(nrow(srcs), tempfile(fileext = ".parquet")))),
#   tar_target(files, sclfun(red, path), pattern = map(red, path)),
#   tar_files(parquet, files)
#   #tar_target(arrow, arrow::open_dataset(parquet))
# )
