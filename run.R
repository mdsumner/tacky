library(targets)
library(crew)
tar_option_set(
  controller = crew_controller_local(workers = 26)
)
system.time({
tar_make()
})





tar_load(spec)
tar_load(res)

library(ximage)
tib <- tibble::as_tibble(lapply(res[c("red", "green", "blue")], scales::rescale, from = c(0, 8000)))


clamp <- function(x, rg = c(0, 1)) {x[x < rg[1]] <- rg[1]; x[x > rg[2]] <- rg[2]; x[is.na(x)] <- 0; x}
ref <- list()
ref[[1]] <- rep(NA, prod(spec$dimension))
ref[[1]][res$cell] <-  rgb(clamp(tib$red), clamp(tib$green), clamp(tib$blue))
attr(ref, "dimension") <- spec$dimension[1:2]
attr(ref, "extent") <- spec$ex
attr(ref, "crs") <- spec$crs
par(mar = rep(0, 4))
ximage(ref, asp = 1)

