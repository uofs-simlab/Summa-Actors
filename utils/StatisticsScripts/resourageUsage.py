import subprocess
import csv
from sys import argv
'''
This is a script that gets the resource usage of jobs and output the stats as a csv.
'''

'''
This function uses the seff command and can get the following data:
 - Start HRU
 - Num HRU
 - num CPUs
 - CPU-Efficiency
 - Wall-Clock Time
 - Memory Used
'''
def seffCommand(jobId, numJobs):
    input_prompt = "SummaActors: a\nSummaOriginal: o\n"
    # Get input from the user
    user_response = input(input_prompt)
    print(user_response)
    if user_response == "a":
        output_csv_name = "SummaActors_jobStats_{}.csv".format(jobId)
    elif user_response == "o":
        output_csv_name = "SummaOriginal_jobStats_{}.csv".format(jobId)
    else:
        raise Exception("Something went wrong")

    csvFile = open(output_csv_name, 'w')
    header = ["startHRU", "numHRU", "#-CPU", "CPU Efficiency", "Wall-Clock Time", "Memory Used"]

    writer = csv.writer(csvFile)

    writer.writerow(header)

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


