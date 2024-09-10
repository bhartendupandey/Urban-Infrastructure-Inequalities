import glob
import os
from qgis.core import *
from qgis.gui import *
from qgis.PyQt.QtWidgets import *

import processing
from qgis.analysis import QgsNativeAlgorithms
from processing.core.Processing import Processing
from processing.tools import dataobjects
from qgis.core import QgsFeatureRequest

basedir = "D:/Paper2/"
files = [os.path.basename(x) for x in glob.glob(basedir + "Data/Fishnet/*.gpkg")]
qgs = QgsApplication([], True)
qgs.initQgis()
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

inputdatapath = "D:/Paper2/Data/Fishnet/"
gadmfile = "D:/Paper2/Data/GADM/gadm36_0_simplified_douglas-Peucker_0_003.shp"
outputdatapath = "D:/Paper2/Data/Fishnet_Arc/"

for i in files:
    lname = i.split(".gpkg")[0]
    inputfile = inputdatapath + i + "|layername=" + lname
    outputfile = outputdatapath + i
    context = dataobjects.createContext()
    context.setInvalidGeometryCheck(QgsFeatureRequest.GeometryNoCheck)
    processing.run("qgis:joinattributesbylocation", {'INPUT': inputfile,'JOIN': gadmfile,'PREDICATE': [0], 'JOIN_FIELDS': [], 'METHOD': 1, 'DISCARD_NONMATCHING': False, 'PREFIX': '','OUTPUT': outputfile},context=context)
    print(i)

qgs.exec_()
qgs.exitQgis()

