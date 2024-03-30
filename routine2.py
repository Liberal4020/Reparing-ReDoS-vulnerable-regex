import os
import subprocess
import time

time_sta = time.perf_counter()
fr = open('./REMEDY-SP2022/testcases/sample50.txt', 'r')
readlines_list = fr.readlines()
fr.close()

fw = open('./pure_regex/bin/regex1.txt', 'w')
fw.write(readlines_list[0])
fw.close()

while True:
  time_sta1 = time.perf_counter()
  result = subprocess.run(["timeout","100000000000000000000000000000000","java","-jar","REMEDY-SP2022.jar","repair","testcases/sample50.txt"], cwd="./REMEDY-SP2022", stdout=subprocess.PIPE, text=True)
  fw = open('./pure_regex/bin/regex2.txt', 'w')
  fw.write(result.stdout)
  fw.close()
  time_end1 = time.perf_counter()
  time_delta1 = time_end1 - time_sta1
  print("remedy", time_delta1)

  time_sta2 = time.perf_counter()
  subprocess.run(["./_build/default/bin/main.exe"], cwd="./pure_regex")
  time_end2 = time.perf_counter()
  time_delta2 = time_end2 - time_sta2
  print("pure", time_delta2)

  time_sta3 = time.perf_counter()
  subprocess.run(["node","ui_file1.js"], cwd="./regeq/src")
  time_end3 = time.perf_counter()
  time_delta3 = time_end3 - time_sta3
  print("regeq", time_delta3)

  str_list = [];
  fr = open('./regeq/src/output1.txt', 'r')
  readlines_list = fr.readlines()
  if readlines_list == []:
    str_list = str_list + ["\n\n"]
  else:
    str_list = str_list + readlines_list
  fr.close()

  fr = open('./regeq/src/output2.txt', 'r')
  readlines_list = fr.readlines()
  if readlines_list == []:
    str_list = str_list + ["\n\n"]
  else:
    str_list = str_list + readlines_list
  fr.close()

  if str_list[0] == "Equivalent\n":
    break
  elif 'Couldn\'t parse' in str_list[0]:
    print("bug")
    break
  else:
    with open('./REMEDY-SP2022/testcases/sample50.txt', 'r+') as file:
      line = file.readlines()
      if str_list[0] != "\n\n":
        line.insert(line.index('+++\n'), str_list[0])
        print(str_list)
      if str_list[1] != "\n\n":
        line.insert(line.index('---\n'), str_list[1])
      file.truncate(0)
      file.seek(0, os.SEEK_SET)
      file.writelines(line)

fr = open('./pure_regex/bin/regex2.txt', 'r')
print(fr.read().rstrip())
fr.close()

time_end = time.perf_counter()

time_delta = time_end - time_sta


# remedy
# subprocess.run()
