#!/bin/bash
# Mask forest 2019 from PRODES raster.

echo "DEPRECATED: Use the vectorize_prodes.sh"
exit 1

in_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
out_file="/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif_forest.tif"

gdal_calc.py -A $in_file --outfile=$out_file --calc="A*(A==1)" --NoDataValue=0 --type='Byte' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES' --overwrite --quiet
exit 0
