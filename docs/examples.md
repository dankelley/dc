---
permalink: examples.html
layout: default
title: Examples
submenu: examples
---

## World Ocean Atlas

The following downloads the 2013 version of the World Ocean Atlas, in both
1-degree and 5-degree versions.

```r
library(dc) # on github as of 2017-09-21
for (field in c("temperature", "salinity", "oxygen", "silicate",
               "phosphate", "nitrate", "density")) {
	download.woa(field=field, res=1)
	download.woa(field=field, res=5)
}
```

