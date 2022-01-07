# Add random errors to derived Safegraph data to satisfy TOS

# required, install if missing
lop <- c("tidyverse", "data.table")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

#path to your working directory
wd_path <- "./"
setwd(wd_path)
sg_cbg_mbl <- read_rds("./data/SafeGraph_CA_SocialDistancing_cbg.rds") 

cols <- c("fips_12", "NAME", "date", "SD_dwellHome")
sg_cbg_mbl <- sg_cbg_mbl[ , ..cols]
setkey(sg_cbg_mbl,fips_12, NAME,date)

sg_cbg_mbl <- sg_cbg_mbl[, pct_away := 1-SD_dwellHome/(60*24)]

sg_cbg_mbl <- sg_cbg_mbl[, error := runif(.N, -.1,.1)]
sg_cbg_mbl <- sg_cbg_mbl[, pct_away_pertb := pct_away + error]
sg_cbg_mbl <- sg_cbg_mbl[, pct_away_pertb_cens := fifelse(pct_away_pertb < 0, 0, pct_away_pertb)]
sg_cbg_mbl <- sg_cbg_mbl[, pct_away_pertb_cens := fifelse(pct_away_pertb_cens >1 , 1, pct_away_pertb_cens)]

cols <- c("fips_12", "NAME", "date", "pct_away_pertb_cens")
sg_cbg_mbl <- sg_cbg_mbl[ , ..cols]

sg_cbg_mbl
summary(sg_cbg_mbl)

write_rds(sg_cbg_mbl, "./data/SafeGraph_CA_SocialDistancing_cbg_pertb.rds", compress = "gz") 
