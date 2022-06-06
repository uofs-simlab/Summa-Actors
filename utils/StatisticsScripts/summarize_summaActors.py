import os
import re
import sys

summaryFile = '_log_summaryActors.txt'
ext = ".out"

if len(sys.argv) == 1:
	sys.exit('Error: no input folder specified')

else:

	folder = sys.argv[1]

def determine_output(folder,file):
	outFile = open(folder + file, 'r')
	print(outFile)
	try:
		lines = outFile.readlines()
	except UnicodeDecodeError:
		outFile.close()
		outFile = open(folder + file, encoding = "ISO-8859-1")
		lines = outFile.readlines()
	counter = 1
	for line in reversed(lines):
		if counter > 30:
			return -1
		else:
			if "Hours" in line:
				hours = re.findall("\d+\.\d+", line)
				return hours
			counter += 1
		

try:
	os.remove(folder + "/" + summaryFile)
except OSError:
	pass

files = []
for file in os.listdir(folder):
	if file.endswith(ext):
		files.append(file)

files.sort()

total_success = []

computation_time = []

with open(folder + '/' + summaryFile, "w") as sf:
	sf.write('Summarizing log files in ' + folder + '\n \n')
	sf.write('Log files' + '\n')

	for file in files:
		value = determine_output(folder, file)
		if value == -1:
			sf.write("{} - Still Running or Failed\n".format(file))
		else:
			sf.write("{} - Success after {} hours \n".format(file, value[0]))



