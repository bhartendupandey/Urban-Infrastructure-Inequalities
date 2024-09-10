from qgis.core import QgsApplication
from qgis.core import QgsRasterLayer
from qgis.core import QgsVectorLayer
from qgis.gui import *
from qgis.PyQt.QtWidgets import *
import processing
from qgis.analysis import QgsNativeAlgorithms
from processing.core.Processing import Processing
import os
import glob
import sys
from qgis.core import *
import numpy as np

qgs = QgsApplication([], True)
qgs.initQgis()

Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

basedir = "D:/Paper2/"
files = [os.path.basename(x) for x in glob.glob(basedir + "Data/Fishnet_Arc/*.gpkg")]
output = basedir + 'Data/Fishnet_Arc_VIIRS/'
#files = files[3:4]
for i in files:
    inpfile = basedir + "Data/Fishnet_Arc/" + i + "|layername=" + i.split(".gpkg")[0]
    output = basedir + 'Data/Fishnet_Arc_VIIRS/'
    #VIIRS THRESHOLD IMAGES
    for j in [50]:
        rasterFilePath = r"D:\Paper2\Data\VIIRS_STL_Outputs"
        for x in range(1,8):
            xx = x + 1
            output1 = output + i.split(".gpkg")[0] + "_" + str(xx) + "_" + str(j) + '.gpkg'
            fname = "VIIRS_mustl_" + str(xx) + "_" + str(j)  + ".tif"
            ntlimage = os.path.join(rasterFilePath,fname)
            pre = "V_" + str(xx) + "_"+ str(j)+ "_"
            processing.run("native:zonalstatisticsfb",
                           {'INPUT': inpfile, 'INPUT_RASTER': ntlimage, 'COLUMN_PREFIX': pre,
                            'STATISTICS': [0, 1, 2, 4], 'OUTPUT': output1})
            print(i + ': ', str(j)+ ", "+ str(xx))

print("VIIRS: Done")