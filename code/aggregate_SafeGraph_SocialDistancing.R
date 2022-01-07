# Get census data and merge with Safegraph

# required, install if missing
lop <- c("tidycensus", "tidyverse", "data.table", "foreach")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

#path to your working directory
wd_path <- "./"
setwd(wd_path)

# other options
options(tigris_use_cache = TRUE)
census_api_key("00000000000000000000000000000000000000") # your key here

## start timer for all operations
start.time <- Sys.time()

### CBG version, key'ing over fips_12
ca.pop.df <- get_acs(state = "CA", geography = "block group", 
                     variables = "B01001_001", geometry = F)
head(ca.pop.df)

ca.pop.df <- ca.pop.df %>% select(fips_12=GEOID, NAME, pop=estimate)
ca.pop.dt <- as.data.table(ca.pop.df)
setkey(ca.pop.dt,fips_12)

## start timer for all operations
start.time <- Sys.time()

# use data.table for efficient processing of large csv.gz
# runs from Jan 2019/2020 to April 2019/2020, every day
# careful: code does not work with two digit months
all.df <- foreach(y=2019:2020, .combine=rbind) %:%  foreach(m=1:4, .combine=rbind) %:% 
    foreach(d=1:31, 
            .packages = c("data.table"), 
            .combine = rbind, .errorhandling = "remove") %do% {
                
                if(nchar(d) == 1) d <- paste0("0",d)
                dat <- fread(file.path("/data/",y, paste("0", m, "/", d, "/",y,"-0",m,"-",d,"-social-distancing.csv.gz",sep="")))
                
                dat <- dat[, origin_census_block_group := as.character(origin_census_block_group)]
                dat <- dat[, nchar_id := nchar(origin_census_block_group)]
                
                # everyone to 12 digits with leading zeros
                dat$origin_census_block_group[dat$nchar_id == 11] <- paste0("0", dat$origin_census_block_group[dat$nchar_id == 11])
                dat <- dat[, fips_2 := substr(origin_census_block_group,1,2)]
                
                dat <- dat[fips_2 == "06"]
                dat <- dat[, fips_12 := substr(origin_census_block_group,1,12)]
                dat <- dat[, fips_11 := substr(origin_census_block_group,1,11)]
                dat <- dat[, fips_5 := substr(origin_census_block_group,1,5)]
                
                
                # ## rescale the devices, not needed for median time away
                # setkey(dat,fips_12)
                # dat <- merge(ca.pop.dt, dat, all.x = T)
                # 
                # i = which(names(dat)=="device_count")
                # set(dat, i=which(is.na(dat[[i]])), j=i, value=0)
                # 
                # # data is by day
                # dat <- dat[, totalPop := sum(pop)]
                # dat <- dat[, totalDev := sum(device_count)]
                # 
                # dat <- dat[, observedDev := device_count / totalDev]
                # dat <- dat[, expectedDev := pop / totalPop]
                # 
                # plot(device_count_weighted ~ expectedDev, data=dat, col = alpha("blue",0.2))
                # abline(lm(device_count_weighted ~ expectedDev, data=dat), col="black")
                # summary(lm(device_count_weighted ~ expectedDev, data=dat))
                # 
                # dat <- dat[, adj_factor := expectedDev * (1/observedDev)]
                # dat <- dat[, device_count_weighted := device_count * adj_factor]
                
                dat <- dat[, totalDev := sum(device_count), by = fips_12]
                dat <- dat[, weight := device_count/totalDev]
                
                dat <- dat[, completely_home_share := completely_home_device_count / totalDev]
                dat <- dat[, full_time_work_share := full_time_work_behavior_devices / totalDev]
                dat <- dat[, part_time_work_share := part_time_work_behavior_devices / totalDev]
                
                ## aggregate to census tract
                dat <- dat[, 
                           list(SD_distHome = median(distance_traveled_from_home,na.rm=T), 
                                device_count = sum(device_count,na.rm=T),
                                SD_dwellHome = median(median_home_dwell_time,na.rm=T),
                                SD_allHomeCount = sum(completely_home_device_count,na.rm=T), 
                                SD_pctHome = sum(completely_home_share, na.rm=T),
                                SD_partTimers = sum(part_time_work_behavior_devices,na.rm=T),
                                SD_fullTimers = sum(full_time_work_behavior_devices,na.rm=T),
                                SD_fullTimersShare =  sum(full_time_work_share,na.rm=T),
                                SD_partTimersShare =  sum(part_time_work_share,na.rm=T) ),
                           by = list(fips_12)
                           ]

                dat <- merge(ca.pop.dt, dat, all.x = T)
                setkey(dat,fips_12)
                dat <- dat[, date := paste(y,"-","0",m,"-",d,sep="")]
                dat <- dat[, date := as.Date(date, "%Y-%m-%d")]
                return(dat)
            }

## time code
time.taken <- Sys.time() - start.time
print("the complete code took")
print(time.taken)
write_rds(all.df, "./data/SafeGraph_CA_SocialDistancing_cbg.rds")

