import subprocess

cmd = subprocess.Popen('seff 59326149_1', shell=True, stdout=subprocess.PIPE)

for line in cmd.stdout:
    if b'CPU Utilized:' in line:
        print(line)