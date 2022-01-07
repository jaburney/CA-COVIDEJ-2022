# TITLE: ingest_combine_CARB_PurpleAir_EJ_FINAL.R
# PURPOSE: Ingest, clean, and quality flag CARB and PurpleAir PM data. Join with other data streams.

# PREP ------------------------------------------------------
wd_path <- "./"

pkgs <- c('tidyverse','lubridate','ggplot2','vroom','fs','estimatr','scales','sf')
lapply(pkgs, require, character.only = TRUE)

# LAND COVER DATA ---------------------------------------------------------------------
# 0= water, 1= forest, 2= shrub, 3= agriculture, 4 = urban
landcover_perc_cbg <- vroom(paste0(wd_path,'/data/CBG_LCpercentag.csv'), # FILE IN GIT /data/
                            col_select = list(block_group='GEOID',prc_ag = 'prc_agriculture',prc_dev='prc_developed'))

# CLIMATE DATA ------------------------------------------------
climate_data<- vroom(paste0(wd_path,'/data/Climate_Sep2020.csv'), # FILE TOO LARGE FOR GIT /data/
                     col_select = list(site_ID='site',RH='RH',TEMP='Temp',PRECIP='pr',day,month,year)) %>%
    mutate(DOY = yday(make_date(year, month, day)))

# CARB PM DATA -------------------------------------------------

        # Read data and make columns, distinct
CARB_PM <- vroom(dir_ls(paste0(wd_path,"/data/PM_CARB/")), # FILE IN GIT /data/
                col_select = list('date',time='start_hour','site','name',lat='latitude',lon='longitude',
                                  parameter='variable',sample_measurement='value','units','quality'),
                comment='Quality Flag Definition') %>%
                filter(!is.na(date)|!is.na(time)|!is.na(name))
CARB_PM$date <- parse_date_time(CARB_PM$date, orders = c('ymd', 'mdy'))  # parse two different data string types
CARB_PM <- CARB_PM %>% mutate(date=update(date,hour=time)) %>% # combine
  mutate(date_local=force_tz(date,tz='US/Pacific'),
         date_UTC=with_tz(date_local,tz='UTC'),
         date_lastchange=as.POSIXct(max(date_local,na.rm=TRUE)),
         state='California',
         DOY=yday(date_local),
         year=year(date_local),
         wday=wday(date_local),
         wk = week(date_local),
         mth = month(date_local),
         Source='CARB',
         site_ID=paste(Source,site,sep=""),
         Uptime = NA,
         RSSI = NA) %>%
  select(-time,-date,-site) %>%
  filter(!is.na(date_local)) %>%
  distinct(site_ID,date_local,parameter,.keep_all=TRUE)
        
CARB_PM <- separate(CARB_PM,col=parameter, into = c("parameter", "parameter_type"), sep = 4,convert=TRUE) %>%
    mutate(parameter=recode(parameter,'PM25'="PM2.5"))

        # Hourly data to Daily mean
CARB_PM.daily <- CARB_PM %>% mutate(date=as.Date(date_local,tz='US/Pacific')) %>% filter(parameter_type!='_LHR') %>% # for now, remove sometimes redundant _LHR
  group_by(site_ID, name, lat, lon, parameter, parameter_type, Source, Uptime, RSSI, wk, mth, wday, date, DOY, year) %>%
  summarize(sample_mean=mean(sample_measurement,na.rm=T)) %>% 
  rename(date_local=date) %>% 
  ungroup()

        # Quality filters and flagging
CARB_PM.daily_QAfilter <- CARB_PM.daily %>%
    mutate(day_flag_high = case_when(sample_mean > 500 ~ TRUE, !is.na(sample_mean)  ~ F), # Cutoff of 500 as a maximum value
           day_flag_zero = case_when(sample_mean<=0 ~ TRUE, !is.na(sample_mean)  ~ F)) %>% # Flag exact zeros as unrealistic
    group_by(site_ID,parameter,parameter_type,year) %>%
    mutate(site_flag_totbad = sum(sample_mean > 500 | sample_mean <= 0)) %>%
    add_tally(name = "site_flag_totdays") %>%
    ungroup() %>%
    mutate(goodsite = case_when(site_flag_totbad/site_flag_totdays<0.1 & site_flag_totdays>=.8*121 ~ TRUE, # good site = less than 10% of a sites days are 'bad', and it has more than 80% of the total days
                                TRUE ~ FALSE),
                                EPA_flag = FALSE)

        # add climate
CARB_PM.daily_QAfilter <- CARB_PM.daily_QAfilter %>% left_join(climate_data,by=c('site_ID','DOY','year'))

        # add identical PM2.5 rows to allow for filtering final data product by Purple Air correction
CARB_PM2.5_RH <- filter(CARB_PM.daily_QAfilter,parameter=='PM2.5') %>% mutate(parameter='PM2.5_RH') # duplicated, only difference is name of parameter, to align with PurpleAir correction options
CARB_PM2.5_EPA <- filter(CARB_PM.daily_QAfilter,parameter=='PM2.5') %>% mutate(parameter='PM2.5_EPA')
CARB_PM.daily_bind <- bind_rows(CARB_PM.daily_QAfilter,CARB_PM2.5_RH,CARB_PM2.5_EPA)

# percent loss with QA filter
print(paste0(round(((count(CARB_PM.daily_QAfilter)-count(subset(CARB_PM.daily_QAfilter,goodsite==T&day_flag_high==F&day_flag_zero==F))))/count(CARB_PM.daily_QAfilter)*100,2), 
             " percent PM data loss with QA filter"))

# PURPLEAIR PM DATA ------------------------------------------------------

        # Read data and make columns, distinct
PurpleAir_PM.daily <- vroom(paste0(wd_path,"/data/PurpleAir_AllCounties_AB_daily_2020-04-30.csv"), # FILE TOO BIG FOR GIT /data/
                                col_select = list(date = 'time',site='ID',name='Label','lat','lon','Uptime','RSSI',
                                                 'PM2.5','PM2.5_A','PM2.5_B')) %>%
  distinct(site,date,.keep_all=TRUE) %>%
  mutate(date_local=force_tz(date,tz='US/Pacific'), 
         DOY=yday(date_local),
         year=year(date_local),
         wday=wday(date_local),
         wk = week(date_local),
         mth = month(date_local),
         Source='PurpleAir',
         site_ID=paste(Source,site,sep=""),
         parameter_type=NA) %>%
  select(-date,-site) %>%
  mutate(PM2.5_diff=abs(PM2.5_A-PM2.5_B),
         PM2.5_perdiff = PM2.5_diff/PM2.5_A*100) %>%
  mutate(EPA_flag = case_when(is.na(PM2.5_diff) | is.na(PM2.5_perdiff) ~ FALSE,
                                    PM2.5_diff > 5 & PM2.5_perdiff > 16 ~ TRUE,
                                    TRUE ~ FALSE)) %>%
  rename(sample_mean='PM2.5') %>%
  mutate(parameter='PM2.5') %>%
  dplyr::select(-PM2.5_perdiff,-PM2.5_diff)

        # Quality filters and flagging
PurpleAir_PM.daily_QAfilter <- PurpleAir_PM.daily %>%
  mutate(day_flag_high = case_when(sample_mean > 500 ~ TRUE, !is.na(sample_mean) ~ FALSE), # Cutoff of 500 as a maximum value
         day_flag_zero = case_when(sample_mean<=0 ~ TRUE, !is.na(sample_mean)  ~ FALSE)) %>% # Flag exact zeros as unrealistic
  group_by(site_ID,year) %>%
  mutate(site_flag_totbad = sum(sample_mean > 500 | sample_mean <= 0 | EPA_flag)) %>% # site is bad 
  add_tally(name = "site_flag_totdays") %>%
  ungroup() %>%
  mutate(goodsite = case_when(site_flag_totbad/site_flag_totdays<0.1 & site_flag_totdays>=.8*121 ~ TRUE, # good site = less than 10% of a sites days are 'bad', and it has more than 80% of the total days
                              TRUE ~ FALSE))

        # percent loss with QA filter
print(paste0(round(((count(PurpleAir_PM.daily_QAfilter)-count(subset(PurpleAir_PM.daily_QAfilter,goodsite==TRUE&day_flag_high==FALSE&day_flag_zero==FALSE))))/count(PurpleAir_PM.daily_QAfilter)*100,2), 
             " percent PM data loss with QA filter"))

        # correction factors
PurpleAir_PM.daily.corr <- as.data.frame(PurpleAir_PM.daily_QAfilter) %>%   # correction factors 
    left_join(climate_data,by=c('site_ID','DOY','year')) %>%
    #drop_na(RH_mean,Temp_mean) %>% # otherwise we crash!
    mutate(PM2.5_RH=sample_mean*(1 / (1+((0.25*((RH/100)^2)) / (1-(RH/100))))),
           PM2.5_EPA = (0.39*sample_mean) + (0.0024*TEMP*((9/5)+32)) - (0.05*RH) + 5.19)
PurpleAir_PM.daily.corrpivot <- PurpleAir_PM.daily.corr %>% 
    rename(PM2.5='sample_mean') %>%
    dplyr::select(-parameter) %>%
    pivot_longer(cols=c('PM2.5','PM2.5_A','PM2.5_B','PM2.5_RH','PM2.5_EPA'),names_to='parameter',values_to = 'sample_mean') # select correct columns

    # Add EJ and climate landcover
PurpleAir_PM.daily_bind <- PurpleAir_PM.daily.corrpivot


# COMBINE --------------------------------------------------
        # Bind PurpleAir and CARB PM together
CALIFORNIA_PM.daily <- bind_rows(CARB_PM.daily_bind,PurpleAir_PM.daily_bind) 
       
         # all site metadata creation, connvert to geometry
CALIFORNIA_PM.metadata <- select(CALIFORNIA_PM.daily,site_ID,lat,lon) %>% distinct(site_ID,lat,lon) %>% 
    st_as_sf(coords = c('lon', 'lat')) %>% st_set_crs(4326)
save(CALIFORNIA_PM.metadata,file=(paste0(wd_path,'/data/CALIFORNIA_PM.metadata.Rdata')))

# INTEGRATE CENSUS DATA ---------------------------------------------------------------------
        # Import cbg census data
load(paste0(wd_path,'/data/CA_blockgroup_metadata_6.29.20.Rdata'))  # FILE IN GIT /data/
# RB replaced this by a join to speed things up
#EJ_cbg <- st_intersection(CALIFORNIA_PM.metadata,scp) %>% as.data.frame() %>% dplyr::select(-geometry) 
EJ_cbg <- st_join(scp,CALIFORNIA_PM.metadata) %>% as.data.frame() %>% dplyr::select(-geometry) 
        # join
CALIFORNIA_PM.daily.EJ <- CALIFORNIA_PM.daily %>% 
    left_join(EJ_cbg, by='site_ID')
save(CALIFORNIA_PM.daily.EJ,file=(paste0(wd_path,'/data/CALIFORNIA_PM.daily.EJ.Rdata')))

        # AOD_NOx adjoin with cbg census and landcover
CALIFORNIA_AOD_NO2_cbg.weekly.EJ <- vroom(paste0(wd_path,'/data/CA_BlockGroup_OFFL.csv'), # FILE TOO BIG FOR GIT /data/
                                          col_select = list(block_group='GEOID',tract_ID='tract','AOD','NO2',wk='week','year',
                                                            Prc_wk='Prc',RH_wk='RH',Temp_wk='Temp')) %>%
    left_join(EJ_cbg,by='block_group') %>%
    left_join(landcover_perc_cbg,by='block_group')
save(CALIFORNIA_AOD_NO2_cbg.weekly.EJ,file=(paste0(wd_path,'/data/CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata')))


