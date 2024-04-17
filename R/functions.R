
##' internal function to read a stac query and get all its hrefs (every variable)
hrefs0 <- function(x) {
  a <- jsonlite::fromJSON(x)
  l <- lapply(a$features$assets, \(.x) .x$href)
  nms <- names(a$features$assets)

  hrefs <- tibble::as_tibble(setNames(l, nms))
  if(("next" %in% a$links$rel)) {
    idx <- which("next" == a$links$rel)
    hrefs <- rbind(hrefs, Recall(a$links$href[idx]))
  }
  hrefs
}

## vectorized hrefs from a stac query
hrefs <- function(x) {
  out <- NULL
  for (x0 in x) out <- rbind(out, hrefs0(x0))
  out
}


## function to take a single-row of href, this works on red,green,blue,cloud from sentinel-2-c1-l2a
## returns a table of cell, red, green, blue - we mask cloud to 10 atm
warpfun <- function(x,  dim = c(1280, 0), ext = NULL, crs = NULL) {
  if (is.null(crs)) crs <- ""
  cl_arg <- NULL
  if (!is.null(dim)) cl_arg <- c(cl_arg, "-ts", dim[1], dim[2])
  if (!is.null(ext)) cl_arg <- c(cl_arg, "-te", ext[1], ext[3], ext[2], ext[4])
  ## I don't think we have to deal with bands, we get all or 1

  gdalraster::warp(x, tf <- tempfile(fileext = ".tif", tmpdir = "/vsimem"), t_srs = crs, cl_arg = cl_arg, quiet = TRUE)

  ds <- new(gdalraster::GDALRaster, tf)
  dat <- gdalraster::read_ds(ds)
  ds$close()
  gdalraster::vsi_unlink(tf)
  dat

}

## function to run the warp function and trim out the result, we only store valid pixels
## cell is relevant to dim+ext+crs, and used as a grouping for taking median
sclfun <- function(red, green, blue, cloud,  dim = c(1280, 0), ext = NULL, crs = NULL,
                   path = tempfile(fileext = ".parquet"), i = 1) {

  check <- gdalraster::buildVRT(visual <- tempfile(fileext = ".vrt", tmpdir = "/vsimem"),
                                sprintf("/vsicurl/%s", c(red, green, blue, cloud)), cl_arg = "-separate", quiet = TRUE)


  vis <- warpfun(visual, dim = dim, ext = ext, crs = crs)
  mm <- matrix(vis, ncol = 4)
  bad <- mm[,4] > 10
  out <- mm[,1:3]
  if (any(bad)) out[bad,] <- NA
  keep <- rowSums(out, na.rm = TRUE) > 0
  arrow::write_parquet(tibble::tibble(cell = which(keep),
                 red = out[keep,1], green = out[keep, 2], blue = out[keep, 3], i = i), path, compression = "uncompressed")
  path

}


ql <- function(x, dim = NULL) {
  if (!grepl("/vsicurl", x)) x <- sprintf("/vsicurl/%s", x)
  info <- vapour::vapour_raster_info(x)
  dim <- tail(info$overviews, 2)
  ximage::ximage(d <- gdal_raster_data(x[1], target_dim = dim, bands = 1:info$bands), asp = 1)
  invisible(d)
}

clamp <- function(x, rg = c(0, 1)) {x[x < rg[1]] <- rg[1]; x[x > rg[2]] <- rg[2]; x[is.na(x)] <- 0; x}

scale_image <- function(x, spec) {
  scaled <- tibble::as_tibble(lapply(x[c("red", "green", "blue")], scales::rescale, from = c(0, 8000)))
  ref <- list()
  ref[[1]] <- rep(NA_character_, prod(spec$dimension))
  print(str(ref))
  ref[[1]][x$cell] <-  rgb(clamp(scaled$red), clamp(scaled$green), clamp(scaled$blue))
  print(str(ref))
  attr(ref, "dimension") <- spec$dimension[1:2]
  attr(ref, "extent") <- spec$ex
  attr(ref, "crs") <- spec$crs
  ref
}

calc_med0 <- function(files, cl = default_cluster()) {
    arrow::open_dataset(files) %>%    collect() %>%
      group_by(cell) %>%  partition(cl) %>%
      summarize(red = median(red), green = median(green), blue = median(blue)) %>% collect()

}

calc_med <- function(files) {
  duckdbfs::open_dataset(files) |> group_by(cell) |> summarize(red = median(red), green = median(green), blue = median(green)) |> collect()
}
create_figure <- function(x) {
  ## x is a weird raster format
  m <- matrix(x[[1]], attr(x, "dimension")[2L], byrow = TRUE)
  ex <- attr(x, "extent")
  df <- data.frame(xmin = ex[1], xmax = ex[2], ymin = ex[3], ymax = ex[4])
  ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,  ymin = ymin, ymax = ymax)) +
    annotation_raster(m, ex[1], ex[2], ex[3], ex[4]) +
    #coord_fixed(ratio = 1/cos(mean(ex[3:4]) * pi/180)) +
    coord_sf(crs = attr(x, "crs"))

}
