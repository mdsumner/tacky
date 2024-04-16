library(targets)
library(crew)

system.time({
tar_make()
})


tar_load(scaled)


library(ximage)
par(mar = rep(0, 4))
ximage(scaled, asp = 1)

