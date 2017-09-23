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
library(dc)
for (field in c("temperature", "salinity", "oxygen", "silicate",
               "phosphate", "nitrate", "density")) {
	dc.woa(field=field, res=1)
	dc.woa(field=field, res=5)
}
```

