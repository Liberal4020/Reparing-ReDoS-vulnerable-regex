import os
import subprocess

while True:
  result = subprocess.run(["timeout","30","java","-jar","REMEDY.jar","repair","testcases/sample.txt"], cwd="./REMEDY-SP2022", stdout=subprocess.PIPE, text=True)
  fw = open('./pure_regex/bin/regex2.txt', 'w')
  # ここのディレクトリ構成どうするか
  fw.write(result.stdout)
  fw.close()

  subprocess.run(["./_build/default/bin/main.exe"], cwd="./pure_regex")
  subprocess.run(["node","ui_file.js"], cwd="./regeq/src")

  fr = open('./regeq/src/output.txt', 'r')
  str_list = fr.readlines()
  fr.close()
  if str_list[0] == "Equivalent\n":
    break
  elif "Couldn't parse" in str_list[0]:
    print("bug")
    break
  else:
    with open('./REMEDY-SP2022/testcases/sample.txt', 'r+') as file:
      # 全行取得
      line = file.readlines()
      # 途中に挿入
      line.insert(line.index('+++'), str_list[0])
      line.insert(line.index('---'), str_list[1])
      # ファイルデータ全削除
      file.truncate(0)
      # 先頭にストリームを移動
      file.seek(0, os.SEEK_SET)
      # 書き込み
      file.writelines(line)

fr = open('./pure_regex/_build/default/bin/regex2.txt', 'r')
print(fr.read())
fr.close()


# remedy
# subprocess.run()