library(oce)
library(dc)
f <- dc.topo(-70, -50, 35, 50, resolution=2, force=TRUE)
topo <- read.topo(f)
summary(topo)
imagep(topo, colormap(name="gmt_globe"))

