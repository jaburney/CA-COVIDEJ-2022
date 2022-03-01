# Disparate air pollution reductions during California’s COVID-19 economic shutdown

This repository contains replication code and data availability for "Disparate air pollution reductions during California’s COVID-19 economic shutdown" published in Nature Sustainability, 2022. (https://doi.org/10.1038/s41893-022-00856-1)

Please direct questions to Prof. Jennifer Burney at jburney@ucsd.edu, who may pass them to the appropriate coauthor(s).

## Repository organization
* **data**: contains small data and processed data used for models  
  * Several data files, especially larger ones, are stored on our project Dataverse at https://doi.org/10.7910/DVN/ZXVB7A and will need to be moved into this directory to run scripts  
* **data/PM_CARB**: contains raw CARB particulate matter data  
* **data/CA_blockgroup_metadata_shp**: contains census block group shapefiles  
* **code**: contains scripts for analysis, figures, and tables  
* **figures**: contains figures and tables  

## Data
#### PM2.5
We use PurpleAir PM_{2.5} data and California Air Resources Board (CARB) PM_{2.5} data. These are publicly available and were downloaded from their respective sources. The raw data files are available at https://www.purpleair.com/sensorlist and  https://www.arb.ca.gov/aqmis2/aqdselect.php.  
  
The raw CARB data is available in data/PM_CARB and the raw PurpleAir data is available at the dataverse. These station data are cleaned and merged (along with census and climate data) using the script ./code/ingest_combine_CARB_PurpleAir_EJ_FINAL.R  
  
This script produces CALIFORNIA_PM.daily.EJ.Rdata and CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata which are available on the dataverse.  
  
#### Mobility  
We also use mobility data from Safegraph. These are not publicly available, but can be accessed with permission for non-commercial work. Permission can be requested here: https://www.safegraph.com/academics. We use the variable median_home_dwell_time from the Social Distancing Metrics - Documentation available here. We convert this metric to the (median) fraction of time spent away from home via 1 - median_home_dwell_time/(60*24).

Data are formatted using ./code/aggregate_SafeGraph_SocialDistancing.R, which outputs data/SafeGraph_CA_SocialDistancing_cbg.rds. The output file cannot be made publicly available. Instead, we provide a perturbed version of the data at the block group level, where we add a uniform shock distributed over the interval  [-.1,1] to each observed fraction of time spent away from home  (see perturb_SafeGraph_for_publication.R). This file outputs SafeGraph_CA_SocialDistancing_cbg_pertb.rds which is available in the dataverse and can be used to approximately reproduce our mobility results. Note that results using the perturbed mobility variable as an outcome or regressor will be slightly different (and some of these differences will spill over to other coefficients).

#### Census
We downloaded census block groups (CBG) level demographic “metadata” information (origin, race, and income measures) from the U.S. Census Bureau 2018 5-year American Community Survey (ACS) for all CBGs in California using the tidycensus package for the R programming environment (see manuscript Methods). We then joined CBG metadata with road density measures derived from The Global Roads Inventory Project (GRIP4) vector dataset for North America (see manuscript Methods). The script used to do so is ./code/metadata_blockgroup_CA.R. As the GRIP4 vector data are not saved in the data directory due to their public availability and their size, GRIP4 download instructions (for subsequent import and analysis using code) are provided in the ./code/metadata_blockgroup_CA.R script. That script generates and exports the following: (i) formatted CBG metadata as data/CA_blockgroup_metadata_6.29.20 files in both .csv and .Rdata formats, and vector shapefiles (.shp) of all CBGs used in the analysis in data/; and (ii) both (left and right) panels of supplementary Figure S1 (CBG Hispanic ethnicity and race statistics) in figures/.

#### Climate Data
Climate data (precipitation, temperatures, relative humidity) for the PM 2.5 stations were downloaded through Google Earth Engine for all locations. Path to the python script is ./code/DownloadAerosol4stations _ for repository.ipynb  – it is also made available as a Google Colab script at https://colab.research.google.com/drive/1GrSjg7W6RUsiScz2JCkKPZxsaYmDvGoM. The code generates a file Climate_perStation.csv which we then integrate into the file ./code/ingest_combine_CARB_PurpleAir_EJ_FINAL.R, which is available on the dataverse.

#### TROPOMI (satellite NOx)
The supplementary satellite analysis requires satellite data, which was downloaded using Google Earth Engine for all census block groups. Download script (Python) is ./code/DownloadAerosol4Blockgroups _ for repository.ipynb or at https://colab.research.google.com/drive/1SNFliNNNm6Vobb6MziJWMbM4xLd3DKOT as a Google Colab script accessing Google Earth Engine. This code also generates all climate data needed (temperature, precipitation, relative humidity). The code exports a file called CA_BlockGroup_OFFL.csv which is integrated into the file CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata, which is available on the dataverse. 

## Analysis
The statistical analysis is implemented in R. The script ./code/Selection.Rmd contains the complete analysis.

## Figures
Figure 1 maps were created in ArcGIS. Data for Fig1A are from the table monitor_sites.csv  generated by ./code/Prep.R. Fig1B is data exported by the Google Earth Engine Code https://code.earthengine.google.com/23163cc6dec2525beeec4c182240bb82 (NO2_pre_COVID.tif). Fig 1C to 1F are based on the shapefiles of the census data (CBG metadata). Fig 1G symbols courtesy of Noun Project: Automobile by Symbolon; Income and Highway by Vectors Point; Urban by Eucalup; weather by asianson.design; List by Richard Kunák; inequality by b farias.

Figure 2A was also created in ArcGIS. Figure 2B was created using the csv version of SafeGraph_CA_SocialDistancing_cbg.rds in ./code/F2_mobility.py

Figure 3: Created panel by panel in Selection.Rmd

## Supplementary Figures, Tables, & Data

Figure ED1: None

Figure ED2: Fig ED2A is based on file monitor_sites.csv generated by ./code/Prep.R. Fig ED2B is based on the file weights.csv generated by ./code/Selection.Rmd. Fig ED2C uses weights.Rdata on the dataverse

Figure ED3: Created panel by panel in ./code/Selection.Rmd

Figure ED4: Created panel by panel in ./code/Selection.Rmd

Figure ED5: Created in ./code/GenED5_S9_S10.R

Figure ED6: Created from CALIFORNIA_PM.daily.EJ.Rdata (on dataverse) using ./code/Figs_ED6_ED10.R

Figure ED7: Made in ArcGIS using data exported from Google Earth Engine at https://code.earthengine.google.com/23163cc6dec2525beeec4c182240bb82 which exports the number of Tropomi observations pre and post Shutdown 2019 and 2020 (count2019_pre.tif, count2019_post.tif, count2020_pre.tif, count2020_post.tif).  

Figure ED8: Created from csv version of SafeGraph_CA_SocialDistancing_cbg.rds using ./code/FS11_regional_mobility.py

Figure ED9:  Created panel by panel in ./code/Selection.Rmd

Figure ED10: Created from  CALIFORNIA_PM.daily.EJ.Rdata & CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata (on dataverse) using ./code/Figs_ED6_ED10.R

Figure S1: Created directly from census metadata; see ./code/metadata_blockgroup_CA.R

Figure S2: Created using FS2_PA_CARB_comp.py

Table S1: Same as Fig 2B

Table S2-8: All created in ./code/Selection.Rmd

Table S9-10: Created in ./code/GenED5_S9_S10.R
