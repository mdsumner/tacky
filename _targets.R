# ## Load your packages, e.g. library(targets).
 source("./packages.R")
#
# ## Load your R files
lapply(list.files("./R", full.names = TRUE), source)
#
# ## tar_plan supports drake-style targets and also tar_target()
# tar_plan(
#   ex = c(-64, -63.5, -9, -8.5),
#   srcs = hrefs(stacit(ex, date = "2020-01")),
#
#   spec = list(dimension = c(1280, 0), ex = ex, crs = "EPSG:4326"),
#
#   ## we need a template, these might be different so we normalize upfront
#   ref = warpfun(srcs$aot[1], ext = spec$ex, dim = spec$dimension, crs = spec$crs),
#
#   #n = nrow(srcs),
#   #n.cpus = min(c(64, n)),
#   #cl = makeCluster(n.cpus),
#   #tst = clusterExport(cl, "warpfun"),
#   d = parLapply(cl, split(srcs, 1:nrow(srcs))[sample(n)], sclfun, ext = attr(ref, "gis")$bbox[c(1, 3, 2, 4)],
#                 crs = attr(ref, "gis")$srs, dim attr(ref, "gis")$dim[1:2]),
#
#   tar_target(d, transpose(srcs), pattern = map(x))
#   #tst2 = stopCluster(cl)
#
#
# )
tar_option_set(
  controller = crew_controller_local(workers = 64)
)

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

  tar_target(d, sclfun(red, green, blue, cloud, ext = spec$ex,
                       crs = spec$crs, dim = spec$dimension[1:2]),
             pattern = map(red, green, blue, cloud),
             iteration = "list")

)
