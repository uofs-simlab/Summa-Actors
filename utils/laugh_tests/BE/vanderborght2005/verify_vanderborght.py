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

nSnow = "nSnow"
nSoil = "nSoil"
pptrate = "pptrate"
airtemp = "airtemp"
scalarRainPlusMelt = "scalarRainPlusMelt"
scalarSWE = "scalarSWE"
scalarThroughfallSnow = "scalarThroughfallSnow"
scalarThroughfallRain = "scalarThroughfallRain"
scalarSnowSublimation = "scalarSnowSublimation"
scalarInfiltration = "scalarInfiltration"
scalarExfiltration = "scalarExfiltration"
scalarSurfaceRunoff = "scalarSurfaceRunoff"
scalarSurfaceTemp = "scalarSurfaceTemp"
scalarSenHeatTotal = "scalarSenHeatTotal"
scalarLatHeatTotal = "scalarLatHeatTotal"
mLayerHeight = "mLayerHeight"
iLayerHeight = "iLayerHeight"
iLayerLiqFluxSoil = "iLayerLiqFluxSoil"
mLayerTemp = "mLayerTemp"
mLayerDepth = "mLayerDepth"
mLayerLiqFluxSoil = "mLayerLiqFluxSoil"
mLayerVolFracIce = "mLayerVolFracIce"
mLayerVolFracLiq = "mLayerVolFracLiq"
mLayerVolFracWat = "mLayerVolFracWat"
mLayerMatricHead = "mLayerMatricHead"
basin__TotalArea = "basin__TotalArea"
basin__SurfaceRunoff = "basin__SurfaceRunoff"
basin__ColumnOutflow = "basin__ColumnOutflow"
basin__AquiferStorage = "basin__AquiferStorage"
basin__AquiferRecharge = "basin__AquiferRecharge"
basin__AquiferBaseflow = "basin__AquiferBaseflow"
basin__AquiferTranspire = "basin__AquiferTranspire"
averageInstantRunoff = "averageInstantRunoff"
averageRoutedRunoff = "averageRoutedRunoff"
fieldCapacity = "fieldCapacity"
scalarLAI = "scalarLAI"
scalarSAI = "scalarSAI"

output_variables = [nSnow,nSoil,pptrate,airtemp,scalarRainPlusMelt,scalarSWE,scalarThroughfallSnow, \
    scalarThroughfallRain,scalarSnowSublimation,scalarInfiltration,scalarExfiltration,scalarSurfaceRunoff, \
    scalarSurfaceTemp,scalarSenHeatTotal,scalarLatHeatTotal,mLayerHeight,iLayerHeight,iLayerLiqFluxSoil, \
    mLayerTemp,mLayerDepth,mLayerLiqFluxSoil,mLayerVolFracIce,mLayerVolFracLiq,mLayerVolFracWat, \
    mLayerMatricHead,basin__TotalArea,basin__SurfaceRunoff,basin__ColumnOutflow,basin__AquiferStorage, \
    basin__AquiferRecharge,basin__AquiferBaseflow,basin__AquiferTranspire,averageInstantRunoff, \
    averageRoutedRunoff,fieldCapacity,scalarLAI,scalarSAI]

verified_data_path = Path("./verification_data/vanderborght2005_exp1_G1-1_timestep.nc")
data_to_compare_path = Path("./output/vanderborght2005_exp1GRU1-1_timestep.nc")
verify(verified_data_path, data_to_compare_path, output_variables, numHRU)

verified_data_path = Path("./verification_data/vanderborght2005_exp2_G1-1_timestep.nc")
data_to_compare_path = Path("./output/vanderborght2005_exp2GRU1-1_timestep.nc")
verify(verified_data_path, data_to_compare_path, output_variables, numHRU)

verified_data_path = Path("./verification_data/vanderborght2005_exp3_G1-1_timestep.nc")
data_to_compare_path = Path("./output/vanderborght2005_exp3GRU1-1_timestep.nc")
verify(verified_data_path, data_to_compare_path, output_variables, numHRU)