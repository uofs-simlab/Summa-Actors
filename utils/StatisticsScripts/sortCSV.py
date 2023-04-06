import pandas as pd

# Read CSV file into a DataFrame
df = pd.read_csv('/home/kklenk/scratch/Single_CPU_TEST/non-actors/logs/_log_summaryOriginal.csv')

# Sort DataFrame by column
df = df.sort_values('start_hru')

# Write sorted DataFrame back to CSV file
df.to_csv('/home/kklenk/scratch/Single_CPU_TEST/non-actors/logs/_log_summaryOriginal_sorted.csv', index=False)