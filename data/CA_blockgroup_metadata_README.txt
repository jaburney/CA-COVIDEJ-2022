README

# Variables -----------------------------------------------------

block_group: FIPS, tract, and block group; unique block group ID.

tract: census tract number of block group

county, state: county of block group; state of block group

area_block_group: area of block group (m2)

medincome_block_group: median income estimate at census block group level from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*.

race_total_block_group: total count of any race classification from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. These are also the total population numbers of the block group.

race_[race]_block_group: [race] count from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. Divide by race_total_block_group to get percentage. [race] includes white, black, and asian.

hisp_total_block_group: total count of any hispanic classification from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. Hispanic classification is separate from race in the ACS; any race can also be hispanic. These are also the total population numbers of the county or block group (same as race_total_block_group).

hisp_yes_block_group: hispanic count from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. Divide by hisp_total_block_group to get percentage. Hispanic classification is separate from race in the ACS; any race can also be hispanic.

hisp_yes_[race]_block_group: count of any hispanic AND [race] alone classification from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. Divide by hisp_yes_block_group to get percentage of hispanic; divide by hisp_total_block_group to get percentage of block group population. [race] includes white, black, and asian.

hisp_no_[race]_block_group: count of any NON-hispanic AND [race] alone classification from the US Census Bureau’s 2014-2018 5-year ACS (American Community Survey)*, in the block group. Divide by (hisp_total_block_group - hisp_yes_block_group) to get percentage of non-hispanic;  divide by hisp_total_block_group to get percentage of block group population. [race] includes white, black, and asian.

grip4_block_group_dens_m_km2: custom exact road density measure (m/km^2) extracted from GRIP** road vector data over the block group area.

pop_density_block_group [NOT pre-calculated in data] = race_total_block_group / area_block_group.

# Data Sources -----------------------------------------------------

* Census data downloaded using tidycensus package in R.

** GRIP: Meijer et al 2018 Environ. Res. Lett. https://doi.org/10.1088/1748-9326/aabd42. GRIP data downloaded at: https://www.globio.info/download-grip-dataset. In the US, GRIP appears to only contain what would be classified as primary or secondary roads in US Census TIGER.
