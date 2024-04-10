system.time({
library(targets)
tar_make()

tar_load(d)
library(dplyr)
for (i in seq_along(d)) arrow::write_parquet(arrange(d[[i]], cell),file.path("parquet", sprintf("%03i.parquet", i)))

res <- arrow::open_dataset(fs::dir_ls("parquet")) %>% arrange(cell) %>%  collect() %>% group_by(cell) %>%  partition(cluster) %>% summarize(red = median(red), green = median(green), blue = median(blue)) %>% collect()


tar_load(spec)

library(ximage)
tib <- tibble::as_tibble(lapply(res[c("red", "green", "blue")], scales::rescale, from = c(200, 6500)))


clamp <- function(x, rg = c(0, 1)) {x[x < rg[1]] <- rg[1]; x[x > rg[2]] <- rg[2]; x[is.na(x)] <- 0; x}
ref <- list()
ref[[1]] <- rep(NA, prod(c(1280, 1280)))
ref[[1]][res$cell] <-  rgb(clamp(tib$red), clamp(tib$green), clamp(tib$blue))
attr(ref, "dimension") <- spec$dimension[1:2]
attr(ref, "extent") <- spec$ex
attr(ref, "crs") <- spec$crs
par(mar = rep(0, 4))
ximage(ref)

