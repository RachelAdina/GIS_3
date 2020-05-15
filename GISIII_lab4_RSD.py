# -*- coding: utf-8 -*-
"""
Created on Thu May  7 16:12:23 2020

@author: Rachel Steiner-Dillon
"""

#conda install contextily --channel conda-forge

import pandas
import matplotlib.pyplot as plt
import geopandas as gpd


#Pandas
data = pandas.read_csv(r'c:\users\rache\Documents\GitHub\GIS_3\CAINC1__ALL_STATES_1969_2017.csv', encoding='latin-1', skipfooter=3, engine='python')

pandas.set_option('display.max_columns', 500)
small = data[data.LineCode.isin( [2, 3] )]

for year in range(1969, 2018):
    small = small[small[str(year)] != "(NA)"] #drop all records with NA

convert_dict = dict([(str(year), int) for year in range (1969, 2018)])
small = small.astype(convert_dict)

geofips = pandas.unique(small.GeoFIPS)
small['GeoFIPS'] = [fips.replace("\"", "").strip() for fips in small.GeoFIPS]
pc_inc = small[small.LineCode==3]

max_ids = pc_inc.iloc[:, 8:].idxmax() 
for y, max_id in enumerate(max_ids):
    year = y + 1969
    name = pc_inc.loc[max_id].GeoName
    pci = pc_inc.loc[max_id, str(year)]
    print(year, pci, name)


'''
Exercise:
Identify the area with the lowest per-capita income each year.
'''
min_ids = pc_inc.iloc[:, 8:].idxmin() 
for y, min_id in enumerate(min_ids):
    year = y + 1969
    name = pc_inc.loc[min_id].GeoName
    pci = pc_inc.loc[min_id, str(year)]
    print(year, pci, name)
    

'''
Exercise:
As a percentage of the minimum per-captia income, calculate the relative income gap between the extremes of the income distribution each year.
Identify the year with the maximum relative income gap.
'''
max_pci = []
for y, max_id in enumerate(max_ids):
    year = y + 1969
    pci = pc_inc.loc[max_id, str(year)]
    max_pci.append(pci)

year = []
for y, max_id in enumerate(max_ids):
    yr = y+1969
    year.append(yr)
    
min_pci = []
for y, min_id in enumerate(min_ids):
    year = y + 1969
    pci = pc_inc.loc[min_id, str(year)]
    min_pci.append(pci)

pci_df = pandas.DataFrame({'year': year,
                      'maximum': max_pci,
                      'minimum': min_pci})

pci_df['gap'] = (pci_df.maximum - pci_df.minimum)/pci_df.minimum

pci_df.head()

pci_df.loc[pci_df['gap'].idxmax()]


#Visualizations 
db = geopandas.read_file(r'c:\users\rache\Documents\GitHub\GIS_3\texas.shp')

fig, axs = plt.subplots(1, 2, figsize=(12, 5))
db['BLK90'].plot.hist(color = 'purple', ax=axs[0])
db['BLK90'].plot.kde(color = 'black', linewidth = 0.5, ax=axs[1])
fig.suptitle("BLK90")
plt.show()



#Geopandas
'''
In R, dataframes are structured as a list of vector variables.
In Python, dataframes are structured as dictionaries, with each
column containing a series object. 

Both R and Python store spatial data as dataframes with a geometry column. 
However, this column is less "sticky" in Python. Python can store several
geometry columns per observation, and can easily switch between them as the 
column from which spatial calculations are run. 

Additional differences:
    1) Indices in pandas begin with 0, rather than 1.
    2) Python does not have the same "base" data analytics capabilities as R.
       It requires a package like pandas. 
    3) The syntax df.col_name makes functions somewhat easier to string
       together in python than in R, where it's clunkier to refer to individual 
       columns.
'''





