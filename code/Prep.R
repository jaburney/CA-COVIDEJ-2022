# Prep data for models

# required, install if missing
lop <- c("tidyverse", "lubridate", "data.table", "sf")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

#path to your working directory
wd_path <- "./"

# original
sg_cbg_mbl <- read_rds(paste0(wd_path,"/data/SafeGraph_CA_SocialDistancing_cbg.rds"))  # FILE TOO LARGE FOR GIT /data/
#perturbed
#sg_cbg_mbl <- read_rds(paste0(wd_path,"/data/SafeGraph_CA_SocialDistancing_cbg_pertb.rds"))  # FILE TOO LARGE FOR GIT /data/

setkey(sg_cbg_mbl,fips_12,date)

sg_cbg_mbl <- sg_cbg_mbl[, fips_5 := substr(fips_12,1,5)]
sg_cbg_mbl <- sg_cbg_mbl[, fips_11 := substr(fips_12,1,11)]

# original data
sg_cbg_mbl <- sg_cbg_mbl[, pct_away := 1-SD_dwellHome/(60*24)]
# use perturbed data for public repository
#sg_cbg_mbl <- sg_cbg_mbl[, pct_away := pct_away_pertb_cens]

# satellite data at CBGs

load(paste0(wd_path,"/data/CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata"))  # FILE TOO LARGE FOR GIT /data/

# a few convenience edits
CALIFORNIA_AOD_NO2_cbg.weekly.EJ <- CALIFORNIA_AOD_NO2_cbg.weekly.EJ %>% 
    mutate(date_first = ifelse(year==2020, ymd( "2020-01-01" ) + weeks(wk - 1 ), ymd( "2019-01-01" ) + weeks( wk - 1 ))) %>% 
    mutate(date_first = as_date(date_first), date_last = date_first + weeks(1) - 1) %>% 
    select(date_first, date_last, everything()) %>% 
    dplyr::rename(TEMP = Temp_wk, RH = RH_wk, PRECIP = Prc_wk) %>%
    mutate(ca_emergency = date("2020-03-04"), ca_shelter = date("2020-03-19")) %>% 
    mutate(adj_phase = (yday(date_last) >= yday(ca_emergency) & yday(date_first) < yday(ca_shelter))) %>% 
    mutate(post = as.numeric(date_last > ca_emergency),
           wk = week(date_first), mth = month(date_first) ,
           NO2_1e6 = NO2*1e6,
           ln_inc_cbg = log(medincome_block_group),
           share_nonwhite = 1 -( race_white_block_group/race_total_block_group),
           share_nonwhite_alone = 1 - (hisp_no_white_block_group/hisp_total_block_group),
           share_hisp = hisp_yes_block_group/hisp_total_block_group,
           share_black =  race_black_block_group/race_total_block_group,
           share_asian = race_asian_block_group/race_total_block_group,
           ln_road_dens =  log(1 + (grip4_block_group_dens_m_km2)) ,
           ln_pop_dens =   log( 1 + race_total_block_group/(as.numeric(area_block_group)/1000^2)), 
           fips_5  = substr(block_group,1,5)
    ) 


## filter to same period
fe_sat_data_cbg_weekly <- CALIFORNIA_AOD_NO2_cbg.weekly.EJ %>%  filter(mth < 5)

## add week to mobility data and merge to satellites

sg_cbg_mbl_weekly <- sg_cbg_mbl %>% mutate(wk = week(date) , year= year(date),
                                    fips = as.numeric(substr(fips_12, 2,5)),
                                    block_group = fips_12) %>% as.data.table()
setkey(sg_cbg_mbl_weekly, NAME, fips, fips_12, block_group, wk, year)

# for weekly there is no need to weight by pop at CBG level
sg_cbg_mbl_weekly <- sg_cbg_mbl_weekly[, 
                                       list(pct_away_cbg = mean(pct_away, na.rm=T)), 
                                       by = list(NAME, fips, fips_12, block_group, wk, year)
                                       ] %>% as_tibble()

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% left_join(sg_cbg_mbl_weekly %>% select(-NAME))

#Load CA PM data
load(paste0(wd_path,"/data/CALIFORNIA_PM.daily.EJ.Rdata"))

# a few convenience edits
CALIFORNIA_PM.daily.EJ <- CALIFORNIA_PM.daily.EJ %>% dplyr::rename(date = date_local) %>% 
    select(date, everything())  %>% 
    dplyr::rename(site=site_ID) %>% 
    mutate(ca_emergency = date("2020-03-04"), ca_shelter = date("2020-03-19")) %>% 
    mutate(site= as.character(site)) %>% mutate(TEMP = TEMP, RH = RH, PRECIP = PRECIP) %>%  
    mutate(adj_phase = (yday(date) >= yday(ca_emergency) & yday(date) < yday(ca_shelter))) %>% 
    mutate(post=as.integer((yday(date)>=yday(ca_shelter))), 
           wk = week(date), mth = month(date) ,
           ln_inc_cbg = log(medincome_block_group),
           share_nonwhite_cbg = 1 -( race_white_block_group/race_total_block_group),
           share_hisp_cbg = hisp_yes_block_group/hisp_total_block_group,
           share_black_cbg =  race_black_block_group/race_total_block_group,
           share_asian_cbg = race_asian_block_group/race_total_block_group,
           ln_road_dens_cbg =  log(1 + (grip4_block_group_dens_m_km2)) ,
           ln_pop_dens_cbg =   log( 1 + race_total_block_group/(as.numeric(area_block_group)/1000^2)), 
           site_by_cbg = interaction(site, block_group),           
           fips_5  = substr(block_group,1,5)
    ) 

# work with PM2.5 after EPA correction
dependent = "PM2.5_EPA"

## filter
fe_data <- CALIFORNIA_PM.daily.EJ %>%    
    filter(parameter==dependent, goodsite==TRUE&day_flag_high==FALSE&day_flag_zero==FALSE&EPA_flag==FALSE) %>% 
    filter(mth < 5) %>% 
    mutate(ln_sample_mean = log(sample_mean)) %>%  dplyr::rename(source = Source)

# QA filters works
fe_data %>% filter(TEMP < 0) %>%  pull(TEMP) %>%  summary()
fe_data %>% filter(sample_mean > 500) %>%  pull(sample_mean) %>%  length()
fe_data %>% filter(sample_mean == 0) %>%  pull(sample_mean) %>%  length()

site_names<-fe_data %>% filter(year == 2019, mth %in% c(1,4)) %>% select(site, lat, lon) %>% distinct() %>% pull(site)

sites <- fe_data %>% filter(site %in% site_names, year == 2020, date < ca_emergency) %>%
    group_by(site,lat,lon,source) %>% summarise(sample_mean = mean(sample_mean, na.rm=T))
    
write_csv(sites, file = paste0(wd_path,"/data/monitor_sites.csv"))

sg_cbg_mbl$fips <- as.numeric(substr(sg_cbg_mbl$fips_12, 2,5))
table(nchar(sg_cbg_mbl$fips))
sg_cbg_mbl$block_group <- sg_cbg_mbl$fips_12
table(nchar(sg_cbg_mbl$block_group))
table(nchar(fe_data$block_group))

names(sg_cbg_mbl) <- paste0(names(sg_cbg_mbl), "_cbg")
sg_cbg_mbl <- sg_cbg_mbl %>%  dplyr::rename(fips_12 = fips_12_cbg, fips = fips_cbg, date = date_cbg, fips_5 = fips_5_cbg,
                                     block_group = block_group_cbg)

fe_data <- fe_data %>% left_join(sg_cbg_mbl %>% select(-NAME_cbg))

## aggregate to CBG
fe_data_cbg <- as.data.table(fe_data)
setkey(fe_data_cbg, state, county, tract, block_group, year, date, wk, mth, ca_shelter,ca_emergency, adj_phase)

fe_data_cbg <- fe_data_cbg[, 
                           list(medincome_cbg = first(medincome_block_group), 
                                race_total_cbg = first(race_total_block_group),
                                race_asian_cbg = first(race_asian_block_group),
                                race_white_cbg = first(race_white_block_group),
                                grip4_cbg_dens_m_km2 = first(grip4_block_group_dens_m_km2),
                                area_cbg = first(area_block_group),
                                TEMP = mean(TEMP, na.rm=T),
                                RH = mean(RH, na.rm=T),
                                PRECIP = mean(PRECIP, na.rm=T),
                                ln_inc_cbg = first(ln_inc_cbg), 
                                hisp_total_cbg = first(hisp_total_block_group), 
                                hisp_yes_cbg = first(hisp_yes_block_group), 
                                non_hisp_white_cbg = first(hisp_no_white_block_group), 
                                race_black_cbg = first(race_black_block_group), 
                                pct_away_cbg = first(pct_away_cbg),
                                post = first(post),
                                sample_mean = mean(sample_mean, na.rm=T)),
                           by = list(state, county, tract, block_group ,year, date, DOY, wday, wk, mth, ca_shelter,ca_emergency, adj_phase)
                           ]

fe_data_cbg <- fe_data_cbg %>% as_tibble()

fe_data_cbg <- fe_data_cbg %>%  mutate(ln_inc_cbg = log(medincome_cbg),
                                       share_nonwhite = 1 -( race_white_cbg/race_total_cbg),
                                       share_nonwhite_alone = 1 - (non_hisp_white_cbg/hisp_total_cbg),
                                       share_hisp = hisp_yes_cbg/hisp_total_cbg,
                                       share_black =  race_black_cbg/race_total_cbg,
                                       share_asian =  race_asian_cbg/race_total_cbg,
                                       ln_road_dens =  log(1 + (grip4_cbg_dens_m_km2)) ,
                                       ln_pop_dens =   log( 1 + race_total_cbg/(as.numeric(area_cbg)/1000^2))) 

fe_data_cbg <- fe_data_cbg %>% left_join(fe_sat_data_cbg_weekly %>%  group_by(block_group) %>% select(starts_with("prc")) %>% slice(1))

### weekly aggregation, after dropping the adjustment period
fe_data_weekly_cbg <- as.data.table(fe_data)
setkey(fe_data_weekly_cbg,  state, county, tract, block_group, year, wk, mth, ca_shelter,ca_emergency)

fe_data_weekly_cbg <- fe_data_weekly_cbg[, 
                                         list(medincome_cbg = first(medincome_block_group), 
                                              race_total_cbg = first(race_total_block_group),
                                              race_asian_cbg = first(race_asian_block_group),
                                              race_white_cbg = first(race_white_block_group),
                                              grip4_cbg_dens_m_km2 = first(grip4_block_group_dens_m_km2),
                                              area_cbg = first(area_block_group),
                                              TEMP = mean(TEMP, na.rm=T),
                                              RH = mean(RH, na.rm=T),
                                              PRECIP = mean(PRECIP, na.rm=T),
                                              ln_inc_cbg = first(ln_inc_cbg), 
                                              hisp_total_cbg = first(hisp_total_block_group), 
                                              hisp_yes_cbg = first(hisp_yes_block_group), 
                                              non_hisp_white_cbg = first(hisp_no_white_block_group), 
                                              race_black_cbg = first(race_black_block_group), 
                                              post = first(post),
                                              sample_mean = mean(sample_mean, na.rm=T),
                                              date_first = first(date),
                                              date_last = last(date)),
                                         by = list(state, county ,tract, block_group, year, wk, mth, ca_shelter,ca_emergency)
                                         ]

fe_data_weekly_cbg <- fe_data_weekly_cbg %>% as_tibble()

fe_data_weekly_cbg <- fe_data_weekly_cbg %>% 
    mutate(post=as.integer((date_first>=ca_shelter)), 
           ln_inc_cbg = log(medincome_cbg),
           share_nonwhite = 1 -( race_white_cbg/race_total_cbg),
           share_nonwhite_alone = 1 - (non_hisp_white_cbg/hisp_total_cbg),
           share_hisp = hisp_yes_cbg/hisp_total_cbg,
           share_asian = race_asian_cbg/hisp_total_cbg,
           share_black =  race_black_cbg/race_total_cbg,
           ln_road_dens =  log(1 + (grip4_cbg_dens_m_km2)) ,
           ln_pop_dens =   log( 1 + race_total_cbg/(as.numeric(area_cbg)/1000^2)),
           ln_sample_mean = log(sample_mean))

fe_data_weekly_cbg <- fe_data_weekly_cbg %>% left_join(sg_cbg_mbl_weekly %>% select(-NAME))
fe_data_weekly_cbg <- fe_data_weekly_cbg %>% left_join(fe_sat_data_cbg_weekly %>%  group_by(block_group) %>% select(starts_with("prc")) %>% slice(1))


### aggregate to CBG by source
fe_data_cbg_source <- as.data.table(fe_data)
setkey(fe_data_cbg_source, source, state, county, tract, block_group, year, date, wk, mth, ca_shelter,ca_emergency, adj_phase)

fe_data_cbg_source <- fe_data_cbg_source[, 
                                         list(medincome_cbg = first(medincome_block_group), 
                                              race_total_cbg = first(race_total_block_group),
                                              race_asian_cbg = first(race_asian_block_group),
                                              race_white_cbg = first(race_white_block_group),
                                              grip4_cbg_dens_m_km2 = first(grip4_block_group_dens_m_km2),
                                              area_cbg = first(area_block_group),
                                              TEMP = mean(TEMP, na.rm=T),
                                              RH = mean(RH, na.rm=T),
                                              PRECIP = mean(PRECIP, na.rm=T),
                                              ln_inc_cbg = first(ln_inc_cbg), 
                                              hisp_total_cbg = first(hisp_total_block_group), 
                                              hisp_yes_cbg = first(hisp_yes_block_group), 
                                              non_hisp_white_cbg = first(hisp_no_white_block_group), 
                                              race_black_cbg = first(race_black_block_group), 
                                              pct_away_cbg = first(pct_away_cbg),
                                              post = first(post),
                                              sample_mean = mean(sample_mean, na.rm=T)),
                                         by = list(source , state, county, tract, block_group ,year, date, DOY, wday, wk, mth, ca_shelter,ca_emergency, adj_phase)
                                         ]

fe_data_cbg_source <- fe_data_cbg_source %>% as_tibble()

fe_data_cbg_source <- fe_data_cbg_source %>%  mutate(ln_inc_cbg = log(medincome_cbg),
                                                     share_nonwhite = 1 -( race_white_cbg/race_total_cbg),
                                                     share_nonwhite_alone = 1 - (non_hisp_white_cbg/hisp_total_cbg),
                                                     share_hisp = hisp_yes_cbg/hisp_total_cbg,
                                                     share_black =  race_black_cbg/race_total_cbg,
                                                     share_asian =  race_asian_cbg/race_total_cbg,
                                                     ln_road_dens =  log(1 + (grip4_cbg_dens_m_km2)) ,
                                                     ln_pop_dens =   log( 1 + race_total_cbg/(as.numeric(area_cbg)/1000^2))) 

