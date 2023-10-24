from os import listdir
from os.path import isfile, join
from pathlib import Path
import xarray as xr
import numpy as np
import sys


# Get data in usable format
def extract_variable_data(hru_dataset, var):
    hru_variable_data_from_dataset = []

    if (hru_dataset[var].values.ndim > 1):
        # 2D output case
        for list in hru_dataset[var].values:
            for data in list:
                hru_variable_data_from_dataset.append(data)
    else:
        # 1D output case
        for data in hru_dataset[var].values:
            hru_variable_data_from_dataset.append(data)
    return hru_variable_data_from_dataset


# Check Functions
def check_variable_length(hru_from_dataset_1, hru_from_dataset_2, variable):
    if len(hru_from_dataset_1[variable].values) != len(hru_from_dataset_2[variable].values):
        print("ERROR: output variable", variable, "does not contain the same amount of data")
        print("     hru_from_dataset_1 = ", len(hru_from_dataset_1[variable].values))
        print("     hru_from_dataset_2 = ", len(hru_from_dataset_2[variable].values))
        return False
    else:
        return True
def check_data_for_errors(dataset_1, dataset_2, tolerance):
    error_counter = 0
    for i in range(0, len(dataset_1)):
        if abs(dataset_1[i] - dataset_2[i]) > tolerance: 
            error_counter += 1
            # Open an error file and append the error to it
            error_file = open("error_file.txt", "a")
            error_file.write("     dataset_1 = " + str(dataset_1[i]) + "\n")
            error_file.write("     dataset_2 = " + str(dataset_2[i]) + "\n")
            error_file.write("     " + str(i))
            error_file.write("\n")
            error_file.close()


    return error_counter



def verify_data(dataset_1, dataset_2, num_hru, output_variables):
    dataset_1 = xr.open_dataset(dataset_1)
    dataset_2 = xr.open_dataset(dataset_2)
    total_errors = 0
    for i_hru in range(0, num_hru):
        hru_from_dataset_1 = dataset_1.isel(hru=i_hru).copy()
        hru_from_dataset_2 = dataset_2.isel(hru=i_hru).copy()
        # print(hru_from_dataset_1)
        # print("CHECKING HRU", i_hru, "OF", num_hru)

        for var in output_variables:
            # Open an error file and append the error to it
            error_file = open("error_file.txt", "a")
            error_file.write("Checking: " + str(var) + "\n")
            error_file.close()
            if not check_variable_length(hru_from_dataset_1, hru_from_dataset_2, var):
                print("ERROR: output variable", var, "does not contain the same amount of data")

            hru_variable_data_from_dataset_1 = extract_variable_data(hru_from_dataset_1, var)
            hru_variable_data_from_dataset_2 = extract_variable_data(hru_from_dataset_2, var)
            if len(hru_variable_data_from_dataset_1) != len(hru_variable_data_from_dataset_2):
                print("ERROR: output variable", var, "does not contain the same amount of data")
                print("     hru_from_dataset_1 = ", len(hru_variable_data_from_dataset_1))
                print("     hru_from_dataset_2 = ", len(hru_variable_data_from_dataset_2))

            error_tolerance = 0.0
            errors = check_data_for_errors(hru_variable_data_from_dataset_1, hru_variable_data_from_dataset_2, error_tolerance)
            print("Errors for variable", var, ":", errors)        

def get_output_vars(model_output_file):
    model_output_vars = []
    open_file = open(model_output_file, "r")
    lines = open_file.readlines()

    for line in lines:
        var = line.split(" ")[0]
        if var != "!" and var != "\n":
            model_output_vars.append(var)
    
    return model_output_vars




num_hru = 25
print("Checking output for", num_hru, "HRUs")


dataset_1 = sys.argv[1]
dataset_2 = sys.argv[2]

model_output_file = "/project/gwf/gwf_cmt/kck540/domain_NorthAmerica/Summa-Projects/input_data/summa_actors_input/outputControl_state_vars.txt"

output_vars = get_output_vars(model_output_file)
verify_data(dataset_1, dataset_2, num_hru, output_vars)


