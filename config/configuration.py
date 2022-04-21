import json
import os
from os.path import exists
from datetime import date

def actor_setting(actor_id, setting_name, setting_value):
    new_dic = {actor_id: {}}


"""
Function to create the inital summa_actors_settings file
"""
def create_init_config():
    Summa_Actor_Settings = ["OutputStructureSize", "maxGRUPerJob"]
    Job_Actor_Settings = ["FileManagerPath", "outputCSV", "csvPath"]
    HRU_Actor_Settings = ["printOutput", "outputFrequency"]

def create_output_path(outputPath):
    print("The output path exists, now seperating this run by today's date")
    today = date.today()
    todays_date = today.strftime("%b-%d-%Y")
    outputPath += "{}/".format(todays_date)
    if not exists(outputPath):
        os.mkdir(outputPath)
    print("Directory Created. Now Creating sub directories for SLURM Data and NetCDF data")
    outputNetCDF = outputPath + "netcdf/"
    outputSlurm = outputPath + "slurm/"
    if not exists(outputNetCDF):
        os.mkdir(outputNetCDF)
    if not exists(outputSlurm):
        os.mkdir(outputSlurm)
    
    return outputNetCDF, outputSlurm


def create_file_manager():
    json_file = open("Summa_Actors_Settings.json")
    fileManagerSettings = json.load(json_file)
    json_file.close()
    
    # add the date for the run
    outputPath = fileManagerSettings["Configuration"]["outputPath"]
    if exists(outputPath):
        outputNetCDF, outputSlurm = create_output_path(outputPath)
        fileManagerSettings["Configuration"]["outputPath"] = outputNetCDF
    else:
        print("Output path does not exist, Ensure it exists before running this setup")
        return -1
    
    fileManager = open("fileManager.txt", "w")
    for key,value in fileManagerSettings["Configuration"].items():
        fileManager.write(key + "    \'{}\'\n".format(value))
    fileManager.close()
    print("File Manager for this job has been created")



"""
Function to create the a list of the jobs will run
This is used for submitting the array job
"""
def create_job_list():
    json_file = open("Summa_Actors_Settings.json")
    SummaSettings = json.load(json_file)
    json_file.close()

    numberOfTasks = SummaSettings["JobSubmissionParams"]["numHRUs"]
    maxGRU = SummaSettings["JobSubmissionParams"]["maxGRUsPerSubmission"]
    numCPUs = SummaSettings["JobSubmissionParams"]["cpus-per-task"]
    print(numberOfTasks)
    print(maxGRU)



"""
Funciton checks if the Summa_Actors_Settings.json file exists.
If yes:
    move on
If no:
    create it
"""
def init_run():
    Summa_Settings_Path = './Summa_Actors_Settings.json'
    if exists('./Summa_Actors_Settings.json'):
        print("File Exists, What do we do next")
        create_file_manager()
        create_job_list()
    else:
        print("File Does not Exist and we need to create it")
        create_init_config()       

init_run()