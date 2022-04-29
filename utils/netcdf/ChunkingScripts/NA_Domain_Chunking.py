import subprocess
import sys

# nccopy -c spechum:744,1000 -c airtemp:744,1000 -c pptrate:744,1000 -c SWRadAtm:744,1000 -c LWRadAtm:744,1000 -c airpres:744,1000 
# -c windspd:744,1000 NorthAmerica_remapped_1979-01-01-00-00-00.nc NorthAmerica_remapped_1979-01-01-00-00-00-chunked.nc
def chunkCommand(timesteps, infile, outfile):
    bashCommand = subprocess.run(["nccopy", "-c", "spechum:{},1000".format(timesteps), "-c", "airtemp:{},1000".format(timesteps), \
        "-c", "pptrate:{},1000".format(timesteps), "-c", "SWRadAtm:{},1000".format(timesteps), "-c", "LWRadAtm:{},1000".format(timesteps), \
        "-c", "airpres:{},1000".format(timesteps), "-c", "windspd:{},1000".format(timesteps), "{}".format(infile), "{}".format(outfile)])
    print("Exit Code = %d" % bashCommand.returncode)


def checkTimeSteps(year, month):
    if month == 1 or month == 3 or month == 5 or month == 7 or month == 8 or \
        month == 10 or month == 12:
        return str(744)
    elif month == 2:
        if year % 4 == 0:
            return str(696)
        else:
            return str(672)
    elif month == 4 or month == 6 or month == 9 or month == 11:
        return str(720)


def chunkYear(year):
    year = sys.argv[1]
    month = 1
    year = int(year)
    while month != 13:
        infile = "/project/6008034/kklenk/forcing/NorthAmerica_remapped_{}-{monthS}-01-00-00-00.nc".format(str(year), monthS=(str(0)+str(month)) if month < 10 else str(month))
        outfile = "/home/kklenk/scratch/corruptionTest/NorthAmerica_remapped_{}-{monthS}-01-00-00-00-chunked.nc".format(str(year), monthS=(str(0)+str(month)) if month < 10 else str(month))

        timesteps = checkTimeSteps(year, month)

        print(infile)
        print(outfile)

        chunkCommand(timesteps, infile, outfile)
        month += 1

def chunkSpecificFile(year, month):
    infile = "/project/6008034/kklenk/forcing/NorthAmerica_remapped_{}-{monthS}-01-00-00-00.nc".format(str(year), monthS=(str(0)+str(month)) if month < 10 else str(month))
    outfile = "/home/kklenk/scratch/forcingData/NorthAmerica_remapped_{}-{monthS}-01-00-00-00-chunked.nc".format(str(year), monthS=(str(0)+str(month)) if month < 10 else str(month))

    timesteps = checkTimeSteps(year, month)
    print(infile)
    print(outfile)
    chunkCommand(timesteps, infile, outfile)

chunkSpecificFile(1983, 5)