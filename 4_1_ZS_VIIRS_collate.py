import os
import glob
import numpy as np
import geopandas as gpd
import pandas as pd

basedir = "D:/Paper2/"
fulldir = basedir + 'Data/Fishnet_Arc_VIIRS/'

files = [os.path.basename(x) for x in glob.glob(fulldir + "*.gpkg")]

spatscale = np.round(np.linspace(0.1,5,50),2)
years = np.arange(8)

for i in spatscale:
    #i = spatscale[10]
    cellsizestr= str(i).replace('.','_')
    #print(cellsizestr)
    fname = cellsizestr + "degby" + cellsizestr + "deg.gpkg"
    fullfname = basedir + 'Data/Fishnet_Arc/' + cellsizestr + "degby" + cellsizestr + "deg"+".gpkg"
    gdf = gpd.read_file(fullfname)
    for j in years:
        j = j+1
        fname = cellsizestr + "degby" + cellsizestr + "deg" + "_" + str(j) + "_50.gpkg"
        fullname1 = fulldir + fname
        gdf1 = gpd.read_file(fullname1)
        df1 = pd.DataFrame(gdf1.drop(columns='geometry'))
        df1 = df1.iloc[:,[0,14,15,16,17]]
        gdf = gdf.merge(df1,on="id")
        print("Spatial Scale:",str(i)," \n","Year: ", str(j))
    gdf.to_file("D:/Paper2/Data/Fishnet_Arc_VIIRS/Compiled/"+cellsizestr + "degby" + cellsizestr + "deg"+"_50.gpkg")
