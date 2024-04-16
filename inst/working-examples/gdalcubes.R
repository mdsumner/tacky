## gdalcubes



system.time({
  library(rstac)
  library(gdalcubes)


  Sys.setenv(VSI_CACHE="TRUE")
  Sys.setenv(GDAL_CACHEMAX="30%")
  Sys.setenv(VSI_CACHE_SIZE="10000000")
  Sys.setenv(GDAL_HTTP_MULTIPLEX="YES")
  Sys.setenv(GDAL_INGESTED_BYTES_AT_OPEN="32000")
  Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR")
  Sys.setenv(GDAL_HTTP_VERSION="2")
  Sys.setenv(GDAL_HTTP_MERGE_CONSECUTIVE_RANGES="YES")
  Sys.setenv(GDAL_NUM_THREADS="ALL_CPUS")

  gdalcubes_options(parallel = TRUE)

  s <- stac("https://earth-search.aws.element84.com/v1")

  bbox <- c(-64, -9, -63.5, -8.5)
  datetime <- "2020-01-01T00:00:00Z/2020-01-31T23:59:59Z"
items <- s |>
  stac_search(collections = "sentinel-2-c1-l2a",
              bbox = bbox,
              datetime = datetime,
              limit = 500) |>
  post_request()

items



dx <- diff(bbox[c(1, 3)])/1280
t01 <- format(as.Date(strsplit(datetime, "/")[[1]]))
dt <- unclass(difftime(as.Date(t01)[2], as.Date(t01)[1], units = "days")) + 1
v <-  cube_view(srs = "EPSG:4326",  extent = list(t0 = t01[1], t1 = t01[2],
                                                left = bbox[1], right = bbox[3],
                                                bottom = bbox[2], top = bbox[4]),
              dx = dx, dy = dx,
              dt = sprintf("P%iD", as.integer(dt)),
              aggregation = "median", resampling = "average")

col <- stac_image_collection(items$features)

S2.mask = image_mask("cloud", values=13:100)

cubs <- raster_cube(col, v, mask = S2.mask) %>%
  select_bands(c("red","green","blue")) %>%
  reduce_time(c("median(red)", "median(green)", "median(blue)"))

plot(cubs, rgb = 1:3, zlim = c(0, 8000))

})
