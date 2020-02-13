#! /bin/python
import fileinput, re

lineLst = []
for line in fileinput.input():
    cm = re.search('Map\((.*)\)', line)
    cs = re.search('Set\((.*)\)', line)
    if cm:
        newContList = cm.group(1).split(', ')
        newContList.sort()
        newCont = ', '.join(newContList)
        line = line.replace(cm.group(1), newCont)
    if cs:
        newContList = cs.group(1).split(', ')
        newContList.sort()
        newCont = ', '.join(newContList)
        line = line.replace(cs.group(1), newCont)
    lineLst.append(line)
    
lineLst.sort()

for line in lineLst:
    print(line, end='')