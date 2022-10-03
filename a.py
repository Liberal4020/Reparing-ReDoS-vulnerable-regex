import subprocess
import os

# subprocess.run(["./main.exe"], cwd="./pure_regex")

# fr = open('./regeq/src/output.txt', 'r')
# str_list = fr.readlines()
# fr.close()
# if str_list[0] == "Equivalent\n":
#   print("a")
# elif 'Couldn\'t parse' in str_list[0]:
#   print("bug")
# else:
#   with open('./REMEDY-SP2022/testcases/sample.txt', 'r+') as file:
#     line = file.readlines()
#     line.insert(line.index('+++\n'), str_list[0])
#     line.insert(line.index('---\n'), str_list[1])
#     file.truncate(0)
#     file.seek(0, os.SEEK_SET)
#     file.writelines(line)

str_list = [];

fr = open('./regeq/src/output1.txt', 'r')
readlines_list = fr.readlines()
if readlines_list == []:
  str_list = str_list + ["\n\n"]
else:
  print(fr.readlines())
  str_list = str_list + readlines_list
  print(str_list)
fr.close()

fr = open('./regeq/src/output2.txt', 'r')
readlines_list = fr.readlines()
print(readlines_list)
if readlines_list == []:
  str_list = str_list + ["\n\n"]
else:
  str_list = str_list + readlines_list
fr.close()

if str_list[0] == "Equivalent\n":
  print("a")
elif 'Couldn\'t parse' in str_list[0]:
  print("bug")
else:
  with open('./REMEDY-SP2022/testcases/sample.txt', 'r+') as file:
    line = file.readlines()
    if str_list[0] != "\n\n":
      line.insert(line.index('+++\n'), str_list[0])
      print(str_list)
    if str_list[1] != "\n\n":
      line.insert(line.index('---\n'), str_list[1])
    file.truncate(0)
    file.seek(0, os.SEEK_SET)
    file.writelines(line)