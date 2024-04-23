# -*- coding: utf-8 -*-
"""
Created on Thu Mar 17 15:33:02 2022

@author: smwei
"""

import pandas as pd
import os

data_dir = 'G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Raw_Data\\processedData_Exp2\\pointingData\\processedPointing'
pattern = '.csv'

print(os.getcwd())

def get_files(data_dir,pattern):
# Get all pointing files within data directory
    dir_files = os.listdir(data_dir)
    file_names = [x for x in dir_files if pattern in x]
    full_file_names = []
    for file in file_names:
        full_file_names.append(data_dir + os.path.sep + file)

    return full_file_names

data_files = get_files(data_dir,pattern)



# For all data files
for i,file in enumerate(data_files):
    
    # For the first file, initialize the dataframe
    if i == 0:
        df = pd.read_csv(file)
        
    # For every other file, append that file to the previous df
    else:
        tempdf = pd.read_csv(file)

        df = pd.concat([df,tempdf])
        
# Save out the file as a csv
df.to_csv('G:\\.shortcut-targets-by-id\\1iQQpKU1ZuIM9lonWw4rXjIcMOMTundDC\\SCANN Lab\\EceYuksel\\VR_CompassMountain\\Raw_Data\\processedData_Exp2\\pointingData\\longform_pointing_Exp2.csv')
        
    
# Note that in our longform data file, within route pointing judgments 
# have two different names for whether they appear on Route A or Route B. 
# This means you might need to code a new variable that is just: within or between. 
    
    
    