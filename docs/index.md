---
permalink: index.html
layout: default
title: Overview
submenu: home
---

Dc is a package for the [R statistical language](http://www.r-project.org) that
downloads files from the web in such a way as to permit caching by filename.
This is accomplished by constructing filenames based on the web queries used to
download the data. For example, when the `dc.topo` function retrieves a
topographic matrix for a particular region, it embeds the location and
resolution in name of the file it downloads, so that a second call to `dc.topo`
with the same parameters yields the existing filename, without causing a second
download.

