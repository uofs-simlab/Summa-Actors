from os import listdir
from os.path import isfile, join
from pathlib import Path
import xarray as xr 

numHRU = 1

time = 'time'
scalarSWE = 'scalarSWE'
scalarCanopyWat = 'scalarCanopyWat'
scalarAquiferStorage = 'scalarAquiferStorage'
scalarTotalSoilWat = 'scalarTotalSoilWat'
scalarSenHeatTotal = 'scalarSenHeatTotal'
scalarLatHeatTotal = 'scalarLatHeatTotal'
scalarRainPlusMelt = 'scalarRainPlusMelt'
scalarInfiltration = 'scalarInfiltration'
scalarSurfaceRunoff = 'scalarSurfaceRunoff'
scalarSoilBaseflow = 'scalarSoilBaseflow'
scalarSoilDrainage = 'scalarSoilDrainage'
scalarAquiferBaseflow = 'scalarAquiferBaseflow'
scalarTotalET = 'scalarTotalET'
scalarTotalRunoff = 'scalarTotalRunoff'
scalarNetRadiation = 'scalarNetRadiation'

varList = [time, scalarSWE, scalarCanopyWat, scalarAquiferStorage, scalarTotalSoilWat, \
    scalarSenHeatTotal, scalarLatHeatTotal, scalarRainPlusMelt, scalarInfiltration, \
    scalarSurfaceRunoff, scalarSoilBaseflow, scalarSoilDrainage, scalarAquiferBaseflow, \
    scalarTotalET, scalarTotalRunoff, scalarNetRadiation]

filename = "out.txt"
originalPath = Path('/gladwell/kck540/output/SummaOriginal/failingHRUCheck/SummaOriginal_G079506-079510_day.nc')
actorsPath = Path('/gladwell/kck540/output/SummaActors/FaillingHRUCheck/SummaActorsGRU79500-10_day.nc')

originalDataset = xr.open_dataset(originalPath)
actorsDataset = xr.open_dataset(actorsPath)

allHRUsOriginal = []
allHRUsActors = []

# for ihru in range(0, numHRU):
allHRUsOriginal.append(originalDataset.isel(hru=2).copy())

# for ihru in range(6, numHRU):
allHRUsActors.append(actorsDataset.isel(hru=7).copy())

file = open(filename, "w")
for i in range(0, numHRU):
  file.write("________HRU {}________\n".format(i+1))
  for var in varList:
    file.write("&&&& VAR = {} &&&&\n".format(var))
    dataOrig = []
    dataAct = []
    for data in allHRUsOriginal[i][var].values:
      dataOrig.append(data)
    for data in allHRUsActors[i][var].values:
      dataAct.append(data)
    print("Original", len(dataOrig))
    print("Actors", len(dataAct))
    marginOfError = 0
    if var == time:
      for a in range(0, len(dataAct)):
        if dataOrig[a] != dataAct[a]:
          file.write("{} = Actor and {} = original is different\n".format(dataAct[a], dataOrig[a]))
        # else:
        #   file.write("{} = Actor and {} = original is the same\n".format(dataAct[a], dataOrig[a]))
    else:
      for a in range(0, len(dataAct)):
        diff = dataOrig[a] - dataAct[a]
        if diff < -marginOfError or diff > marginOfError:
          file.write("{}: {} = Actor and {} = original is different:   {}\n".format(a, dataAct[a], dataOrig[a], abs(dataAct[a] - dataOrig[a])))
        # else:
          # file.write("{}: {} = Actor and {} = original is the same\n".format(a, dataAct[a], dataOrig[a]))

        

file.close()