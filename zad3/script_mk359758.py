import os
import sys
import subprocess

if len(sys.argv) != 4:
    print ("USAGE: python script_mk359758.py path_to_train_set path_to_test_set path_to_output")
    sys.exit()

subprocess.call(["Rscript", "zad3.R", sys.argv[1], sys.argv[2], sys.argv[3]])