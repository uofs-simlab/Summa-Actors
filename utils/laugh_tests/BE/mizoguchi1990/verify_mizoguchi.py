from os import listdir
from os.path import isfile, join
from pathlib import Path
import xarray as xr
import numpy as np


def verify(verified_data_path, data_to_compare_path, output_variables, numHRU):
    try:
        verified_dataset = xr.open_dataset(verified_data_path)
        to_compare_dataset = xr.open_dataset(data_to_compare_path)
    except FileNotFoundError:
        print("Check the variables \'verified_data_path\' and \'data_to_compare_path\'. They may not point to the correct output files or the output filenames may have changed.")
        exit()

    # Get the HRUs from the dataset into a list
    for iHRU in range(0, numHRU):
        verified_hru = verified_dataset.isel(hru=iHRU).copy()
        hru_to_compare = to_compare_dataset.isel(hru=iHRU).copy()

        for var in output_variables:
            try:
                if len(verified_hru[var].values) != len(hru_to_compare[var].values):
                    print("ERROR: output variable", var, "does not contain the same amount of data")
                    print("     verified_hru = ", len(verified_hru[var].values))
                    print("     hru_to_compare = ", len(hru_to_compare[var].values))
                
                verified_data = []
                to_verify_data = []
                if (verified_hru[var].values.ndim > 1):
                    # 2D output case
                    for list in verified_hru[var].values:
                        for data in list:
                            verified_data.append(data)
                    
                    for list in hru_to_compare[var].values:
                        for data in list:
                            to_verify_data.append(data)

                else:
                    # 1D output case
                    for data in verified_hru[var].values:
                        verified_data.append(data)
                    
                    for data in hru_to_compare[var].values:
                        to_verify_data.append(data)

                                    
                # check length
                if len(verified_data) != len(to_verify_data):
                    print("ERROR: output variable", var, "does not contain the same amount of data")
                    print("     verified_hru = ", len(verified_data))
                    print("     hru_to_compare = ", len(to_verify_data))

                # check values
                for elem in range(0, len(verified_data)):
                    if verified_data[elem] != to_verify_data[elem]:
                        print("variable -",var, "has different values at", elem)
                        print("     verified_hru = ", verified_data[elem])
                        print("     hru_to_compare = ", to_verify_data[elem])
                        break

            except TypeError:
                print("variable - ", var, "Cannot be compared with len")
                print("     verified_hru = ",verified_hru[var].values)
                print("     hru_to_compare = ", hru_to_compare[var].values)


numHRU = 1

time = "time" 
nSnow = "nSnow" 
nSoil = "nSoil" 
nLayers = "nLayers" 
mLayerHeight = "mLayerHeight" 
iLayerLiqFluxSoil = "iLayerLiqFluxSoil" 
mLayerDepth = "mLayerDepth" 
mLayerVolFracIce = "mLayerVolFracIce" 
mLayerVolFracLiq = "mLayerVolFracLiq" 
mLayerMatricHead = "mLayerMatricHead" 
mLayerTranspire = "mLayerTranspire" 
mLayerBaseflow = "mLayerBaseflow" 
mLayerCompress = "mLayerCompress" 
iLayerNrgFlux = "iLayerNrgFlux" 
basin__TotalArea = "basin__TotalArea" 
scalarGroundEvaporation = "scalarGroundEvaporation" 
scalarSoilBaseflow = "scalarSoilBaseflow" 
scalarSoilDrainage = "scalarSoilDrainage" 
scalarInfiltration = "scalarInfiltration" 
scalarSnowDrainage = "scalarSnowDrainage" 
scalarSnowSublimation = "scalarSnowSublimation" 
scalarThroughfallRain = "scalarThroughfallRain" 
scalarThroughfallSnow = "scalarThroughfallSnow" 
scalarRainfall = "scalarRainfall" 
scalarSnowfall = "scalarSnowfall" 
scalarRainPlusMelt = "scalarRainPlusMelt" 
pptrate = "pptrate" 
averageRoutedRunoff = "averageRoutedRunoff" 
scalarSWE = "scalarSWE"
fieldCapacity = "fieldCapacity"

output_variables = [time, nSnow, nSoil, nLayers, mLayerHeight, iLayerLiqFluxSoil, \
    mLayerDepth, mLayerVolFracIce, mLayerVolFracLiq, mLayerMatricHead, mLayerTranspire, \
    mLayerBaseflow, mLayerCompress, iLayerNrgFlux, basin__TotalArea, scalarGroundEvaporation, \
    scalarSoilBaseflow, scalarSoilDrainage, scalarInfiltration, scalarSnowDrainage, \
    scalarSnowSublimation, scalarThroughfallRain, scalarThroughfallSnow, scalarRainfall, \
    scalarSnowfall, scalarRainPlusMelt, pptrate, averageRoutedRunoff, \
    scalarSWE, fieldCapacity]

verified_data_path = Path("./verification_data/mizoguchi1990_G1-1_timestep.nc")
data_to_compare_path = Path("./output/mizoguchi1990GRU1-1_timestep.nc")
verify(verified_data_path, data_to_compare_path, output_variables, numHRU)