from pathlib import Path
import xarray as xr 


airtemp = "airtemp"

filename = "outputChunked.txt"

datasetPath = Path("/home/kklenk/projects/rpp-kshook/kklenk/forcingChunked/NorthAmerica_remapped_1983-05-01-00-00-00-chunked.nc")

dataset = xr.open_dataset(datasetPath)

data = []

data.append(dataset.isel(hru=492001).copy())

file = open(filename, "w")
for i in data[0][airtemp].values:
    file.write("{}\n".format(i))

file.close()
