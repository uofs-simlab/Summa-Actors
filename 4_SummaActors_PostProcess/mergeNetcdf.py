import subprocess
import os.path

# bashCommand = subprocess.run(["ncrcat"])

listOfFiles = []
listOfFiles.append("ncrcat")
for x in range(1, 519):
    file = "/home/kklenk/scratch/SummaActorsOutput/netcdf/SummaActorsGRU-{}HRU-1_day.nc".format(x)
    listOfFiles.append(file)
    print(os.path.isfile(file))

listOfFiles.append("SummaActors_G000001-000518_day.nc")

bashCommand = subprocess.run(listOfFiles)
print("Exit Code = %d" % bashCommand.returncode)