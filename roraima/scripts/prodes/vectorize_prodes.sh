#!/bin/bash
# Vectorize PRODES raster.

in_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
out_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/vector/prodes/PDigital2000_2019_AMZ.shp"

gdal_polygonize.py $in_file $out_file

exit 0