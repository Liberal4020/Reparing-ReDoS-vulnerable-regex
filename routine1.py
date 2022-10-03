import os
import subprocess

while True:
  result = subprocess.run(["timeout","30","java","-jar","REMEDY-SP2022.jar","repair","testcases/sample.txt"], cwd="./REMEDY-SP2022", stdout=subprocess.PIPE, text=True)
  fw = open('./pure_regex/bin/regex2.txt', 'w')
  fw.write(result.stdout)
  fw.close()

  subprocess.run(["./_build/default/bin/main.exe"], cwd="./pure_regex")
  subprocess.run(["node","ui_file1.js"], cwd="./regeq/src")

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

fr = open('./pure_regex/bin/regex2.txt', 'r')
print(fr.read())
fr.close()


# remedy
# subprocess.run()
