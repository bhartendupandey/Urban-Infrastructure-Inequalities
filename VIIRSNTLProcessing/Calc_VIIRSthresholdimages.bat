SET "infile1=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_1"
SET "infile2=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_2"
SET "infile3=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_3"
SET "infile4=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_4"
SET "infile5=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_5"
SET "infile6=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_6"
SET "infile7=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_7"
SET "infile8=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition\Output\Tmustl_landmasktonodata_negativetozero_applyannualmask\Tmustl_8"

SET "outpath=G:\Projects\PhD Research\Paper 2\Analysis\4_VIIRS_STL_Decomposition_Inequality Analysis\Data\VIIRS_STL_Outputs\"

for /l %%x in (25, 50, 50) do (
start gdal_calc.py -A "%infile1%" --outfile "%outpath%VIIRS_mustl_1_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile2%" --outfile "%outpath%VIIRS_mustl_2_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile3%" --outfile "%outpath%VIIRS_mustl_3_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile4%" --outfile "%outpath%VIIRS_mustl_4_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile5%" --outfile "%outpath%VIIRS_mustl_5_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile6%" --outfile "%outpath%VIIRS_mustl_6_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile7%" --outfile "%outpath%VIIRS_mustl_7_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
start gdal_calc.py -A "%infile8%" --outfile "%outpath%VIIRS_mustl_8_%%x.tif" --calc "%%x*(A>=%%x) + A*(A<%%x)" --NoDataValue=-9999 --co "COMPRESS=LZW"
pause
)