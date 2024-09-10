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
output = basedir + 'Data/Fishnet_Arc_DMSP/'
rasterFilePath = basedir + "/Data/DMSP_VIIRS"

for i in files:
    inpfile = basedir + "Data/Fishnet_Arc/" + i + "|layername=" + i.split(".gpkg")[0]
    # DMSP
    for j in range(2000,2019):

        if j > 2013:
            fname = "Harmonized_DN_NTL_" + str(j) + "_simVIIRS.tif"
        else:
            fname = "Harmonized_DN_NTL_" + str(j) + "_calDMSP.tif"
        output1 = output + i.split(".gpkg")[0] + "_" + str(j) + '.gpkg'
        if os.path.isfile(output1):
            print(i + ': ' + str(j) + " File Exists")
        else:
            ntlimage = os.path.join(rasterFilePath,fname)
            pre = "DV"+ str(j)
            processing.run("native:zonalstatisticsfb",{'INPUT':inpfile, 'INPUT_RASTER':ntlimage, 'COLUMN_PREFIX':pre,'STATISTICS':[0,1,2,4],'OUTPUT':output1})
            print(i+': '+str(j))

print("DMSP: Done")

#qgs.exec_()
qgs.exitQgis()