import subprocess
import csv
from sys import argv

def seffCommand(jobId, numJobs):
    csvFile = open('SummaActors_jobStatistics.csv', 'w')
    header = ["startHRU", "numHRU", "CPU", "CPU Efficiency", "Wall-Clock Time", "Memory Used"]

    writer = csv.writer(csvFile)

    writer.writerow(header)

    startHRU = 1
    numHRU = 1000
    for i in range(0, int(numJobs)):
        print("Job", i)
        rowData = []
        rowData = [numHRU * i + 1, numHRU]
        cmdString = "seff {}_{}".format(jobId, i)
        cmd = subprocess.Popen(cmdString, shell=True, stdout=subprocess.PIPE)
        for line in cmd.stdout:
            if b'Cores per node:' in line:
                cores = line.decode().split(" ")[-1]
                cores = cores.strip()
            
            if b'CPU Efficiency:' in line:
                effeciency = line.decode().split(" ")[2]
                effeciency = effeciency.strip()

            if b'Job Wall-clock time:' in line:
                wallClock = line.decode().split(" ")[-1]
                wallClock = wallClock.strip()
            
            if b'Memory Utilized:' in line:
                memory = line.decode().split(" ")[2]
                memory = memory.strip()
        
        rowData.append(cores)
        rowData.append(effeciency)
        rowData.append(wallClock)
        rowData.append(memory)
        writer.writerow(rowData)

    csvFile.close()
            
jobId = argv[1]
print(jobId)

numJobs = argv[2]
print(numJobs)

seffCommand(jobId, numJobs)


