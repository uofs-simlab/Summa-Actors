import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pylab import cm
# from mpl_toolkits.axes_grid.anchored_artists import AnchoredText

# MATPLOTLIBRC stuff
import matplotlib as mpl
# mpl.use('AGG')  # for systems not running a GUI

font = {'family' : 'serif',
        'weight' : 'normal',
        'size'   : 20
}
mpl.rc('font', **font)

def time_convert(seconds):
    seconds = seconds % (24 * 3600)
    hours = seconds / 3600
    return round(hours, 2)

def add_failures(failed_start_hru):
    failure_data = pd.read_csv('SummaOriginal_rerun_failed_Jul-09.csv')

    failed_wall_clock = failure_data.loc[failure_data["startHRU"] == failed_start_hru, "Wall-Clock Time"]


    return(time_convert(failed_wall_clock.values[0]))


def summa_worst_case():
    # wall clock times of the processes
    batches = ["1", "2", "3", "4", \
        "5", "6", "7", "8"]

    plt_1 = plt.figure(figsize=(12, 8))

    results = [0.87, 0.75, 2.27, 0.87, 0.88, 0.75, 0.76, 1.48]

    cmap = plt.get_cmap('viridis').copy()
    original_color = cmap(0.5)


    plt.bar(batches,results, color=original_color, alpha=0.6, edgecolor=original_color)
    # plt.title('SUMMA Job CPU Distribution', fontsize=20)
    plt.xlabel('Sub-task'
#               , fontsize=16
               )
    plt.ylabel('Hours'
               # , fontsize=16
               )
    # plt.xticks(fontsize=14)
    # plt.yticks(fontsize=14)
    plt.savefig("SUMMA-worst-case.png",bbox_inches="tight")


def summa_best_case():
    batches = []

def cpuComparsion(data_set_1, data_set_2):

    df1 = pd.DataFrame(data_set_1)
    df2 = pd.DataFrame(data_set_2)
    SummaActors = df1["CPU Efficiency"].values
    SummaOriginal = df2["CPU Efficiency"].values
    print("Average CPU-Efficiency SUMMA-Actors =",sum(SummaActors) / len(SummaActors))
    print("Average CPU-Efficiency SUMMA =",sum(SummaOriginal) / len(SummaOriginal))

    summa_count = 0
    actors_count = 0
    for x in range(0, len(SummaOriginal)):
        if SummaActors[x] > SummaOriginal[x]:
            actors_count += 1
        else:
            summa_count += 1

    print("TOTAL ACTORS COUNT =", actors_count)
    print("TOTAL SUMMA COUNT =", summa_count)

    min_summa_actors = min(SummaActors)
    max_summa_actors = max(SummaActors)

    min_summa_original = min(SummaOriginal)
    max_summa_original = max(SummaOriginal)
    string_text = "SUMMA-Actors Range: [{}% - {}%]\nSUMMA Range:            [{}% - {}%]".format(min_summa_actors, max_summa_actors, min_summa_original, max_summa_original)
    # text = AnchoredText(string_text, prop=dict(size=12), frameon=True, loc=2)

    # Create plot
    nbins = 50

    # Setup colors
    cmap = plt.get_cmap('viridis').copy()
    actors_color = cmap(0.02)
    print("Actors color is: ", actors_color)
    original_color = cmap(0.5)

    # Get the binning range of the historgram plots (this is to help us ensure the bins are consistent)
    hist, actors_bins, _ = plt.hist(SummaActors, bins = nbins, color = "red")
    hist, original_bins, _ = plt.hist(SummaOriginal, bins = nbins, color = "pink")

    # This needs to come after the histograms above so they do not get added to our plot.
    fig, ax = plt.subplots(1,1, figsize=(12,8))

    # Get the proper bins
    lower = min(actors_bins[0],original_bins[0])
    upper = max(actors_bins[-1],original_bins[-1])
    linbins = np.linspace(lower,upper,nbins)


    sa_hist = ax.hist(SummaActors, bins=linbins, alpha=0.75, label="SUMMA-Actors", edgecolor=actors_color, linewidth=0.5, color=actors_color)
    so_hist = ax.hist(SummaOriginal, bins=linbins, alpha=0.6, label="SUMMA", edgecolor=original_color, linewidth=0.5, color=original_color)

    # Set Lables
    ax.set_xlabel('CPU Efficiency as Percentage')
    ax.set_ylabel('Count')

    props = dict(boxstyle='round', facecolor="cornflowerblue", alpha=0.5, edgecolor='black')
    ax.text(0.02, 0.8, string_text, transform=ax.transAxes,
        verticalalignment='top', bbox=props)



    # plot legend
    handles, labels = plt.gca().get_legend_handles_labels()
    order = [0,1]
    ax.legend([handles[idx] for idx in order], [labels[idx] for idx in order], loc=0, edgecolor='black')

    plt.savefig("CPU-Efficiency.png", bbox_inches="tight")


def wallClockComparison(data_set_1, data_set_2):
    nbins = 50

    data_set_1_values = data_set_1["job_duration"].values
    data_set_1_reads = list(map(time_convert, data_set_1["read_duration"].values))
    data_set_1_writes = list(map(time_convert, data_set_1["write_duration"].values))

    data_set_2_values = data_set_2["job_duration"].values
    data_set_2_reads = list(map(time_convert, data_set_2["read_duration"].values))
    data_set_2_writes = list(map(time_convert, data_set_2["write_duration"].values))


    print("Total Time For SUMMA-Actors = ", sum(data_set_1_values))
    print("  Total Read Time = ", sum(data_set_1_reads))
    print("  Total Write Time = ", sum(data_set_1_writes))
    print("  Total IO Time = ", sum(data_set_1_reads) + sum(data_set_1_writes))
    print("  Total CPU Time = ", sum(data_set_1_values) - (sum(data_set_1_reads) + sum(data_set_1_writes)))
    print("Total Time For SUMMA = ", sum(data_set_2_values))
    print("  Total Read Time = ", sum(data_set_2_reads))
    print("  Total Write Time = ", sum(data_set_2_writes))
    print("  Total IO Time = ", sum(data_set_2_reads) + sum(data_set_2_writes))
    print("  Total CPU Time = ", sum(data_set_2_values) - (sum(data_set_2_reads) + sum(data_set_2_writes)))

    summa_count = 0
    actors_count = 0
    for x in range(0, len(data_set_1_values)):
        if data_set_1_values[x] < data_set_2_values[x]:
            actors_count += 1
        else:
            summa_count += 1

    print("TOTAL ACTORS COUNT =", actors_count)
    print("TOTAL SUMMA COUNT =", summa_count)


    histActors, sa_bins, _ = plt.hist(data_set_1_values, bins=50, edgecolor='black', linewidth=1.0)
    # Get the binning range from a default histogram plot
    histOriginal, so_bins, _ = plt.hist(data_set_2_values, bins=50, edgecolor='black', linewidth=1.0)

    fig2, ax2 = plt.subplots(1, 1, figsize=(12, 8))


    cmap = plt.get_cmap('viridis').copy()
    sa_color = cmap(0.02)
    so_color = cmap(0.5)

    lower = min(sa_bins[0], so_bins[0])
    upper = max(sa_bins[-1], so_bins[-1])
    linbins = np.linspace(lower,upper,nbins)
    binsForCDF = np.linspace(lower,upper,517)

    pdf1 = histActors / sum(histActors)
    # print(pdf1)
    pdf2 = histOriginal / sum(histOriginal)
    # print(pdf2)
    newList1 = []
    newList2 = []
    newList1[:] = [x / 70 for x in data_set_1_values]
    newList2[:] = [x / 70 for x in data_set_2_values]

    # Create the bar graph
    sa_n, sa_bins, patch = ax2.hist(data_set_1_values, bins=linbins, alpha=0.75, label="SUMMA-Actors", edgecolor=sa_color, linewidth=0.5, color=sa_color)
    so_n, so_bins, patch2 = ax2.hist(data_set_2_values, bins=linbins, alpha=0.6, edgecolor=so_color, linewidth=0.5, label="SUMMA", color=so_color)

    ax2.set_title("Wall-Clock Time Comparison Of Jobs On Each CPU", fontsize=20)
    ax2.set_xlabel('Wall-Clock Time (Hours)'
                   # ,fontsize=16
                   )
    ax2.set_ylabel('Count'
                   # ,fontsize=16
                   )

    # Merge graphs
    ax3 = ax2.twinx()

    ax3.hist(data_set_1_values, bins=sa_bins, linewidth=1.5, histtype='step', cumulative=True, label="SUMMA-Actors", color="#CC00CC")
    ax3.hist(data_set_2_values, bins=so_bins, linewidth=1.5, histtype='step', cumulative=True, label="SUMMA", color="#00CCCC")
    ax3.set_ylabel('Total Number of Bins', labelpad=15)

    # Get rid of line that goes straigh down at the end of this plot
    axpolygons = [poly for poly in ax3.get_children() if isinstance(poly, mpl.patches.Polygon)]
    for poly in axpolygons:
        poly.set_xy(poly.get_xy()[:-1])


    handles1, labels1 = ax2.get_legend_handles_labels()
    handles2, labels2 = ax3.get_legend_handles_labels()

    ax2.legend((*handles1, *handles2), (*len(labels1)*[''], *labels2),
    loc='right', ncol=2, handlelength=3, fontsize=16)
    # ax2.legend(())
    plt.savefig("WallClock.png",bbox_inches="tight")

def boxPlot(data_set_1, data_set_2):

    data_set_1_reads = data_set_1["read_duration"].values
    data_set_1_writes = data_set_1["write_duration"].values

    data_set_2_reads = data_set_2["read_duration"].values
    data_set_2_writes = data_set_2["write_duration"].values


    fig, ax = plt.subplots(1, 1, figsize=(14, 8))

    ax.set_title("Box Plot Of Read And Write Times", fontsize=20)
    ax.set_xlabel('Read/Write', fontsize=14)
    ax.set_ylabel('Time (Seconds)', fontsize=14)

    ax.boxplot([data_set_1_reads, data_set_2_reads, data_set_1_writes, data_set_2_writes], labels=["SUMMA-Actors Read", "SUMMA Read", "SUMMA-Actors Write", "SUMMA Write"], flierprops=dict(markerfacecolor='r', marker='D'), vert=False)
    # set size of tick labels
    ax.tick_params(axis='both', which='major', labelsize=14)
    
    plt.savefig("BoxPlot.png",bbox_inches="tight")




# Assemble Da"a
data_set_1 = pd.read_csv("/scratch/gwf/gwf_cmt/kck540/Single_CPU_Test/actors/logs/_log_summaryActors_sorted.csv")
data_set_2 = pd.read_csv("/scratch/gwf/gwf_cmt/kck540/Single_CPU_Test/non-actors/logs/_log_summaryOriginal_sorted.csv")


# data_set 1 and 2 are used for the paper


wallClockComparison(data_set_1, data_set_2)
# boxPlot(data_set_1, data_set_2)
