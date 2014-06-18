#!/usr/bin/python
# on windows, you can use cx-freeze to make an exe for this

import os
import os.path
import sys

if len(sys.argv)<3:
    sys.stderr.write("Usage: ediff file1 file2")
else:   
    file1 = os.path.abspath(sys.argv[1])
    file2 = os.path.abspath(sys.argv[2])
    if os.sep=="/":   # windows
        file1=file1.replace("\\", "/")
        file2=file2.replace("\\", "/")
    cmd = """emacsclient --eval "(ediff-files \\"%s\\" \\"%s\\")" """  % (file1, file2)
    print cmd
    os.system(cmd)