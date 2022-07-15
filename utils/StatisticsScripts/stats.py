import numpy as np
import pandas as pd
import statistics as stat
import csv
import matplotlib as mpl
import matplotlib.pyplot as plt

def time_convert(x):
    h,m,s = map(int,x.split(':'))
    return (h*60+m)*60+s

def wallClockTime(data_set_1, data_set_2):
    df1 = pd.DataFrame(data_set_1)
    df2 = pd.DataFrame(data_set_2)

    df1_stat = []
    for time in df1["Wall-Clock Time"].values:
        df1_stat.append(round((time_convert(time) / 60) / 60, 2))
    print("Total Wall Clock for data_set_1 =", sum(df1_stat))
    df2_stat = []
    for time in df2["Wall-Clock Time"].values:
        df2_stat.append(round((time_convert(time) / 60) / 60, 2))
    print("Total Wall Clock for data_set_2 =", sum(df2_stat))

def cpuEfficiency(data_set_1, data_set_2):
    df1 = pd.DataFrame(data_set_1)
    df2 = pd.DataFrame(data_set_2)

    df1_stat = []
    for cpu_e in df1["CPU Efficiency"].values:
        df1_stat.append(cpu_e)
    print("Average CPU Efficiency for data_set_1 =", sum(df1_stat) / len(df1_stat))
    df2_stat = []
    for cpu_e in df2["CPU Efficiency"].values:
        df2_stat.append(cpu_e)
    print("Average CPU Efficiency for data_set_2 =", sum(df2_stat) / len(df1_stat))



data_set_actors = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/Jul-13-2022/SummaActors_jobStats_63221110.csv")
data_set_original = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/Jul-09-2022/SummaOriginal_jobStats_63155456.csv")

wallClockTime(data_set_actors, data_set_original)
print("")
cpuEfficiency(data_set_actors, data_set_original)