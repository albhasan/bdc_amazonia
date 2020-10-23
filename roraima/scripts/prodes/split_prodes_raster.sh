#!/bin/bash
# Crop the PRODES raster to the area of interest.

prodes_file="/http/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
aoi_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/vector/area_of_interest/aoi.shp"
out_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"

gdalwarp -cutline $aoi_file -crop_to_cutline $prodes_file $out_file
exit 0