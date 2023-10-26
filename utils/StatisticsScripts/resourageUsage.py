import subprocess
import csv
from sys import argv
'''
This is a script that gets the resource usage of jobs and output the stats as a csv.

Usage: python resourceUsage.py <jobId> <numJobs> <gru_per_job>
'''

'''
This function uses the seff command and can get the following data:
 - Start HRU
 - Num HRU
 - num CPUs
 - CPU-Efficiency
 - Wall-Clock Time
 - Memory Used
 - Completion Status
'''
def seffCommand(jobId, numJobs, gru_per_job):
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
    header = ["startHRU", "numHRU", "#-CPU", "CPU Efficiency", "Wall-Clock Time", "CPU-Utilized", "Memory Used", "Status"]

    writer = csv.writer(csvFile)

    writer.writerow(header)

    numHRU = gru_per_job
    for i in range(0, numJobs):
        print("Job", i)
        rowData = []
        rowData = [numHRU * i + 1, numHRU]
        cmdString = "seff {}_{}".format(jobId, i)
        cmd = subprocess.Popen(cmdString, shell=True, stdout=subprocess.PIPE)
        for line in cmd.stdout:
            if b'Cores' in line:
                cores = line.decode().split(" ")[-1]
                cores = cores.strip()
            
            if b'CPU Efficiency:' in line:
                effeciency = line.decode().split(" ")[2]
                effeciency = effeciency.strip()
                effeciency = effeciency.replace('%', '')

            if b'Job Wall-clock time:' in line:
                wallClock = line.decode().split(" ")[-1]
                wallClock = wallClock.strip()
            
            if b'CPU Utilized: ' in line:
                cpuUtilized = line.decode().split(" ")[-1]
                cpuUtilized = cpuUtilized.strip()
            
            if b'Memory Utilized:' in line:
                memory = line.decode().split(" ")[2]
                memory = memory.strip()
            
            if b'State:' in line:
                status = line.decode().split(" ")[1]
                status = status.strip()
        
        rowData.append(cores)
        rowData.append(effeciency)
        rowData.append(wallClock)
        rowData.append(cpuUtilized)
        rowData.append(memory)
        rowData.append(status)
        writer.writerow(rowData)

    csvFile.close()
            
jobId = argv[1]
print("jobID =", jobId)

numJobs = argv[2]
print("Number of Jobs =", numJobs)

gru_per_job = argv[3]
print("GRUs per job =", gru_per_job)

seffCommand(jobId, int(numJobs), int(gru_per_job))


