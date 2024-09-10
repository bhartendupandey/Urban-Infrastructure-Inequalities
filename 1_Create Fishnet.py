from qgis.core import *
from qgis.gui import *
from qgis.PyQt.QtWidgets import *

import processing
from qgis.analysis import QgsNativeAlgorithms
from processing.core.Processing import Processing
import numpy as np
import os

qgs = QgsApplication([], True)
qgs.initQgis()

Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

for i in np.round(np.linspace(0.1,5,50),2):
    cellsize = float(i)
    crs = "EPSG:4326" #WGS 84 System 
    xmin = -180
    xmax = 180
    ymin = -90
    ymax = 90
    extent = str(xmin)+ ',' + str(xmax)+ ',' +str(ymin)+ ',' +str(ymax)
    cellsizestr= str(i).replace('.','_')
    fname = cellsizestr + "degby" + cellsizestr + "deg.gpkg"
    outputpath = r'D:\Paper2\Data\Fishnet'
    grid = os.path.join(outputpath,fname)
    processing.run('qgis:creategrid',{'TYPE':2,'EXTENT': extent, 'HSPACING':cellsize,'VSPACING':cellsize,'CRS':'EPSG:4326','OUTPUT':grid})
    print(i)
qgs.exec_()
qgs.exitQgis()