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
    data_set_1_actors = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/Jul-08-2022/SummaActors_jobStats_63007640_Filled_failed.csv")
    # data_set_2_actors = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/Jun-17-2022/SummaActors_jobStats_62270590.csv")
    # data_set_3_actors = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/May-26-2022/SummaActors_jobStats_61263427.csv")

    data_set_1_original = pd.read_csv("/home/kklenk/SummaProjects/Summa-Actors/utils/StatisticsScripts/SummaOriginal_jobStats_63155456.csv")
    # data_set_2_original = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/May-27-2022/SummaOriginal_jobStats_61377500.csv")
    # data_set_3_original = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaOriginalOuput/May-30-2022/SummaOriginal_jobStats_61415123.csv")


    df1_actors = pd.DataFrame(data_set_1_actors)
    # df2_actors = pd.DataFrame(data_set_2_actors)
    # df3_actors = pd.DataFrame(data_set_3_actors)


    df1_original = pd.DataFrame(data_set_1_original)
    # df2_original = pd.DataFrame(data_set_2_original)
    # df3_original = pd.DataFrame(data_set_3_original)


    actors_stat1 = []
    for x in df1_actors["Wall-Clock Time"].values:
        actors_stat1.append(round((time_convert(x) / 60) / 60, 2))
    # actors_stat2 = []
    # for x in df2_actors["Wall-Clock Time"].values:
    #     actors_stat2.append(round((time_convert(x) / 60) / 60, 2))
    # actors_stat3 = []
    # for x in df3_actors["Wall-Clock Time"].values:
    #     actors_stat3.append(round((time_convert(x) / 60) / 60, 2))

    print("SUMMA-Actors Array Job 1 Total Wall-Clock =", sum(actors_stat1))
    # print("SUMMA-Actors Array Job 2 Total Wall-Clock =", sum(actors_stat2))
    # print("SUMMA-Actors Array Job 3 Total Wall-Clock =", sum(actors_stat3))

    original_stat1 = []
    for x in df1_original["Wall-Clock Time"].values:
        original_stat1.append(round((time_convert(x) / 60) / 60, 2))
    # original_stat2 = []
    # for x in df2_original["Wall-Clock Time"].values:
    #     original_stat2.append(round((time_convert(x) / 60) / 60, 2))
    # original_stat3 = []
    # for x in df3_original["Wall-Clock Time"].values:
    #     original_stat3.append(round((time_convert(x) / 60) / 60, 2))
    print()
    print("SUMMA-Original Array Job 1 Total Wall-Clock =", sum(original_stat1))
    # print("SUMMA-Original Array Job 2 Total Wall-Clock =", sum(original_stat2))
    # print("SUMMA-Original Array Job 3 Total Wall-Clock =", sum(original_stat3))



    # usageStat4 = []
    # for x in df4["Wall-Clock Time"].values:
    #     usageStat4.append(round((time_convert(x) / 60) / 60, 2))

    # print("Total Time Actor = ", sum(usageStat1))
    # print("Max Actor = ", max(usageStat1))
    # print("Min Actor = ", min(usageStat1))
    # print("----------------------------------------")
    # print("Total Time Original = ", sum(usageStat2))
    # print("Max Original = ", max(usageStat2))
    # print("Min Original = ", min(usageStat2))

    # # totalRam = [sum(usageStat1), sum(usageStat2), sum(usageStat4)]
    # print("usageStat1 Total Ram Used = ", sum(usageStat1))
    # # print("usageStat1 Mean Ram Used = ", stat.mean(usageStat1))
    # print("usageStat2 Total Ram Used = ", sum(usageStat2))
    # print("usageStat2 Mean Ram Used = ", stat.mean(usageStat2))
    # print("usageStat4 Total Ram Used = ", sum(usageStat4))
    # print("usageStat4 Mean Ram Used = ", stat.mean(usageStat4))
    # print()
    # print("variation = ", stat.stdev(totalRam) / stat.mean(totalRam))
    # csvFile = open("VarationStats.csv", 'w')
    # header = ["relative standard deviation"]

    # csvFile.write("{}\n".format("relative standard deviation"))

    # for i in range(0, len(usageStat1)):
    #     l = [usageStat1[i], usageStat2[i], usageStat4[i]]
    #     csvFile.write("{}\n".format(stat.stdev(l) / stat.mean(l)))


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
    


def initDuration():
    data_set_1 = pd.read_csv("/home/kklenk/projects/rpp-kshook/kklenk/SummaActorsOutput/Jun-06-2022/csv/Success1.csv")
    df = pd.DataFrame(data_set_1)
    print(sum(df["initDuration"].values))

def findRow(df, startHRU):
    bool_val = False
    for row in df.iterrows():
        if row[1].iloc[0] == startHRU:
            bool_val = True
            break
    
    if (bool_val):
        print("found", startHRU)
    else:
        print("did not find", startHRU)
    






def compareCompleted():
    data_actor = pd.read_csv("/home/kklenk/SummaProjects/Summa-Actors/utils/StatisticsScripts/SummaActors_jobStats_62666948.csv", index_col=False)
    data_original = pd.read_csv("/home/kklenk/SummaProjects/Summa-Actors/utils/StatisticsScripts/SummaOriginal_jobStats_62667162.csv", index_col=False)
    df_actors = pd.DataFrame(data_actor)
    df_original = pd.DataFrame(data_original)

    df_actors = df_actors.drop(df_actors[df_actors.Status == "TIMEOUT"].index)
    # df_actors = df_actors.drop(columns=["Status","#-CPU","CPU Efficiency","Memory Used"])
    
    df_original = df_original.drop(df_original[df_original.Status == "TIMEOUT"].index)
    # df_original = df_original.drop(columns=["Status","#-CPU","CPU Efficiency","Memory Used"])
    
    
    for row in df_original.iterrows():
        # print(row[1].iloc[0])
        findRow(df_actors, row[1].iloc[0])

    
    # df_actors.to_csv("actors_no_timeout.csv", index=False)
    # df_original.to_csv("original_no_timeout.csv", index=False)
ramUsage()
# compareCompleted()
# initDuration()
