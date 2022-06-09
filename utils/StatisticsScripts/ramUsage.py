import numpy as np
import pandas as pd
import statistics as stat
import csv
import matplotlib as mpl
import matplotlib.pyplot as plt

def time_convert(x):
    h,m,s = map(int,x.split(':'))
    return (h*60+m)*60+s


def ramUsage():
    data_set_1 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/Jun-06-2022/SummaActors_jobStats_61721504.csv")
    data_set_2 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/May-13-2022/SummaActors_jobStatistics_60829543.csv")
    data_set_4 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/May-26-2022/SummaActors_jobStats_61263427.csv")

    # data_set_1 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/Apr-28-2022/SummaOrginal-60232429_jobStatistics.csv")
    # data_set_2 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/May-27-2022/SummaOriginal_jobStats_61377500.csv")
    # data_set_4 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/May-30-2022/SummaOriginal_jobStats_61415123.csv")


    df1 = pd.DataFrame(data_set_1)
    df2 = pd.DataFrame(data_set_2)
    df4 = pd.DataFrame(data_set_4)

    usageStat1 = []
    for x in df1["Wall-Clock Time"].values:
        usageStat1.append(round((time_convert(x) / 60) / 60, 2))
    usageStat2 = []
    for x in df2["Wall-Clock Time"].values:
        usageStat2.append(round((time_convert(x) / 60) / 60, 2))
    usageStat4 = []
    for x in df4["Wall-Clock Time"].values:
        usageStat4.append(round((time_convert(x) / 60) / 60, 2))

    totalRam = [sum(usageStat1), sum(usageStat2), sum(usageStat4)]
    print("usageStat1 Total Ram Used = ", sum(usageStat1))
    print("usageStat1 Mean Ram Used = ", stat.mean(usageStat1))
    print("usageStat2 Total Ram Used = ", sum(usageStat2))
    print("usageStat2 Mean Ram Used = ", stat.mean(usageStat2))
    print("usageStat4 Total Ram Used = ", sum(usageStat4))
    print("usageStat4 Mean Ram Used = ", stat.mean(usageStat4))
    print()
    print("variation = ", stat.stdev(totalRam) / stat.mean(totalRam))
    csvFile = open("VarationStats.csv", 'w')
    header = ["relative standard deviation"]

    csvFile.write("{}\n".format("relative standard deviation"))

    for i in range(0, len(usageStat1)):
        l = [usageStat1[i], usageStat2[i], usageStat4[i]]
        csvFile.write("{}\n".format(stat.stdev(l) / stat.mean(l)))


def scatterPlot():
    data_set_1 = pd.read_csv("/home/kklenk/SummaProjects/Summa-Actors/utils/StatisticsScripts/VarationStats.csv")

    df = pd.DataFrame(data_set_1)

    d = df["relative standard deviation"].values
    x = []
    for i in range(1, 515):
        x.append(i)
    print(len(x))
    print(len(d))
    plt.scatter(x, d)
    plt.title("Coefficient of Variation Plot")
    plt.xlabel("Job number")
    plt.ylabel("Relative Standard Deviation")
    plt.savefig("RSD-Actors.pdf", format="pdf", bbox_inches="tight")
    plt.show()

    
# ramUsage()
scatterPlot()