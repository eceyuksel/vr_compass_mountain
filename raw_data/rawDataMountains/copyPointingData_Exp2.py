"""
Created on Mon Feb 17 10:53:55 2020

@author: stevenweisberg

Consolidate all pointing files to one directory

"""
import os
import glob
from shutil import copy

print(os.getcwd())

# Set the base directory where this file is located
base_dir = os.path.join(os.path.dirname(os.path.abspath('__file__')),os.path.pardir)

# Point to the output directory and make it if it doesn't exist
output_dir = os.path.join(base_dir,'processedData_Exp2','pointingData','rawPointing')
os.makedirs(output_dir,exist_ok=True)

# Get a list of 'out' files from each subject
files = glob.glob(os.path.join(base_dir,'rawDataMountains','Sub*','out*'))

# Copy them to the output directory
for file in files:
    copy(file,output_dir)
