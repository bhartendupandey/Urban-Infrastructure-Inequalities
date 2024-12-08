Note that the output images Tmustl_*.tif) were further processed in QGIS using (Raster Calculator) tool such that:

1. Landmask generated using ./VIIRSLandMaskScript/VIIRSLandMask.bat script was used to mask non-land area.
2. Negative values were set to zero.
3. Non-lit pixels in VCM_ORM_NTL files were masked using 2015 VCM_ORM_NTL 2015 (b1) and 2016 (b2) files (((b1 eq 0) AND (b2 eq 0)) * 1), i.e., SET background pixels from VCM_ORM_NTL == 0 to -9999 (NODATA)): (b1 eq 1) * (-9999)  + (b1 eq 0) * b2, where b2 are the  Tmustl_*.tif files after 1 and 2 were applied.