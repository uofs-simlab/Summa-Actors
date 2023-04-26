# Kyle Klenk, (kyle.klenk@usask.ca)
# This file will summarize the files that are outputed by summa
import os
import re
import sys
import csv

output_file = '_log_summaryOriginal.csv'
ext = ".out"


def get_job_stats(folder, file):
    outFile = open(folder + file, 'r')
    print(outFile)
    
    lines = outFile.readlines()
    
    start_hru = int(''.join(filter(str.isdigit, file)))
    
    max_lines_to_read = 40
    lines_read_counter = 1
    max_items_looking_for = 3
    items_found = 0
    row_data = [start_hru, -99, -99, -99]

    for line in reversed(lines):
        if lines_read_counter > max_lines_to_read:
            return row_data
        
        elif items_found == max_items_looking_for:
            return row_data
        
        elif "FATAL ERROR" in line:
            return row_data
        
        elif "h" in line and "or" in line:
            hours = re.findall("\d+\.\d+", line)
            row_data[1] = hours[0]
            lines_read_counter += 1
            items_found += 1

        elif "elapsed read" in line:
            seconds = re.findall("\d+\.\d+", line)
            row_data[2] = seconds[0]
            lines_read_counter += 1
            items_found += 1

        elif "elapsed write" in line:
            seconds = re.findall("\d+\.\d+", line)
            row_data[3] = seconds[0]
            lines_read_counter += 1
            items_found += 1
            
        else:
            lines_read_counter += 1



# Check command line args
if len(sys.argv) == 1:
	sys.exit('Error: no input folder specified')
else:
	folder = sys.argv[1]

try:
	os.remove(folder + "/" + output_file)
except OSError:
	pass

files = []
for file in os.listdir(folder):
	if file.endswith(ext):
		files.append(file)

files.sort()

csv_file = open(folder + '/' + output_file, "w")
writer = csv.writer(csv_file)
csv_header = ["start_hru", "job_duration", "read_duration", "write_duration"]
writer.writerow(csv_header)

for file in files:
	row_data = get_job_stats(folder, file)
	if row_data is None:
		start_hru = int(''.join(filter(str.isdigit, file)))
		row_data = [start_hru, -99, -99, -99]

	writer.writerow(row_data)

csv_file.close()

