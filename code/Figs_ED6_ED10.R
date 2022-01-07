# ANALYZE: AIR QUALITY and EJ due to COVID
  # Figures ED6, ED10

# PREP ------------------------------------------------------

wd_path=".."

pkgs <- c('tidyverse','lubridate','ggplot2','vroom','fs','lfe','estimatr','margins','scales','slider','scales','patchwork')
lapply(pkgs, require, character.only = TRUE)

load(paste0(wd_path,'.data/CALIFORNIA_PM.daily.EJ.Rdata'))
load(paste0(wd_path,'.data/CALIFORNIA_AOD_NO2_cbg.weekly.EJ.Rdata'))

# ---------------------- PLOTS --------------------------

      # Fig ED6: PM 2.5 Tile, by year and Source, QA'ed
FigED6 <- ggplot(data=subset(CALIFORNIA_PM.daily.EJ,goodsite==T&day_flag_high==F&day_flag_zero==F&parameter=='PM2.5')) +
  geom_tile(aes(DOY,site_ID,fill=sample_mean)) +
  scale_fill_continuous(low='deepskyblue2',high='red4',limits=c(0,50),oob=squish) +
  facet_grid(vars(year),vars(Source)) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  labs(fill=expression(daily~PM[2.5]~(ug~m^-3)),y='Site')
save(FigED6,paste0(wd_path,'.figures/FigED6.pdf'))

  # Fig ED10

#   QA filter, percent, and pivot
PM <- subset(CALIFORNIA_PM.daily.EJ,goodsite==T&day_flag_high==F&day_flag_zero==F) %>% # filter bad data
  mutate(perc_black_cbg = ((race_black_block_group)/race_total_block_group*100),
         perc_hisp_cbg = ((hisp_yes_block_group)/hisp_total_block_group*100),
         perc_asian_cbg = (race_asian_block_group/race_total_block_group*100),
         medincome_cbg = log(medincome_block_group),
         DOY = as.numeric(DOY),
         DOY = case_when(year==2019 ~ DOY-1, # move 2019 DOY back one to line up day of weeks
                                TRUE ~ DOY))
PMlong_bins_all <- PM %>% pivot_longer(cols=c(perc_black_cbg,  perc_hisp_cbg, medincome_cbg, perc_asian_cbg),names_to = 'EJ_name', values_to = 'EJ_value')
PMlong_bins_all$EJ_name <- recode(PMlong_bins_all$EJ_name,medincome_cbg='ln(Median Income)',perc_black_cbg='Share Black (%)',perc_hisp_cbg='Share Hispanic/Latinx (%)',
                                  perc_asian_cbg='Share Asian (%)') 

NOX <- CALIFORNIA_AOD_NO2_cbg.weekly.EJ %>% # filter bad data
  mutate(perc_black_cbg = ((race_black_block_group)/race_total_block_group*100),
         perc_hisp_cbg = ((hisp_yes_block_group)/hisp_total_block_group*100),
         perc_asian_cbg = (race_asian_block_group/race_total_block_group*100),
         medincome_cbg = log(medincome_block_group),
         NO2_umolm2 = NO2*1e6); 
NOXlong_bins_all <- NOX %>% pivot_longer(cols=c(perc_black_cbg,  perc_hisp_cbg, medincome_cbg, perc_asian_cbg),names_to = 'EJ_name', values_to = 'EJ_value')
NOXlong_bins_all$EJ_name <- recode(NOXlong_bins_all$EJ_name,medincome_cbg='ln(Median Income)',perc_black_cbg='Share Black (%)',perc_hisp_cbg='Share Hispanic/Latinx (%)',
                                  perc_asian_cbg='Share Asian (%)') 

# Plot
    # 2D histogram with PM2.5
a <- ggplot(subset(PMlong_bins_all, parameter=='PM2.5'&year==2019),aes(x=EJ_value,y=sample_mean)) +
  geom_bin2d(aes(fill=log(..count..)),bins=150) +
  geom_smooth(method='lm',color='red',se=T) +
  coord_cartesian(ylim=c(0,150)) +
  facet_wrap(~EJ_name,scales='free',strip.position="bottom") +
  theme(strip.background = element_blank(),strip.placement = "outside",strip.text.x = element_text(size = 10),legend.position="none") +
  labs(y=expression(daily~mean~PM[2.5]~(ug~m^-3)),x='',title='January-April 2019')#,fill=ln(count))

# 2D histogram with NO2
b <- ggplot(subset(NOXlong_bins_all, year==2019),aes(x=EJ_value,y=NO2_umolm2)) +
  geom_bin2d(aes(fill=log(..count..)),bins=150) +
  geom_smooth(method='lm',color='red',se=T) +
  facet_wrap(~EJ_name,scales='free',strip.position="bottom") +
  theme(strip.background = element_blank(),strip.placement = "outside",strip.text.x = element_text(size = 10)) +
  labs(y=expression(weekly~NO[2]~(umol~m^-2)),x='')

FigED10 <- (a+b)
save(FigED10,paste0(wd_path,'.figures/FigED10.pdf'))

    # lm fits
# PM2.5
PMlong_bins_all_income <- filter(PMlong_bins_all, parameter=='PM2.5'&year==2019&EJ_name=='ln(Median Income)') %>% lm(sample_mean~EJ_value,data=.)
PM_lnincome <- summary(PMlong_bins_all_income)
PMlong_bins_all_asian <- filter(PMlong_bins_all, parameter=='PM2.5'&year==2019&EJ_name=='Share Asian (%)') %>% lm(sample_mean~EJ_value,data=.)
PM_asian<-summary(PMlong_bins_all_asian)
PMlong_bins_all_black <- filter(PMlong_bins_all, parameter=='PM2.5'&year==2019&EJ_name=='Share Black (%)') %>% lm(sample_mean~EJ_value,data=.)
PM_black<-summary(PMlong_bins_all_black)
PMlong_bins_all_hisp <- filter(PMlong_bins_all, parameter=='PM2.5'&year==2019&EJ_name=='Share Hispanic/Latinx (%)') %>% lm(sample_mean~EJ_value,data=.)
PM_hisp<-summary(PMlong_bins_all_hisp)
# NOX
NOXlong_bins_all_income <- filter(AODlong_bins_all, year==2019&EJ_name=='ln(Median Income)') %>% lm(NO2_umolm2~EJ_value,data=.)
NOX_lnincome<-summary(NOXlong_bins_all_income)
NOXlong_bins_all_asian <- filter(AODlong_bins_all, year==2019&EJ_name=='Share Asian (%)') %>% lm(NO2_umolm2~EJ_value,data=.)
NOX_asian<-summary(NOXlong_bins_all_asian)
NOXlong_bins_all_black <- filter(AODlong_bins_all, year==2019&EJ_name=='Share Black (%)') %>% lm(NO2_umolm2~EJ_value,data=.)
NOX_black<-summary(NOXlong_bins_all_black)
NOXlong_bins_all_hisp <- filter(AODlong_bins_all, year==2019&EJ_name=='Share Hispanic/Latinx (%)') %>% lm(NO2_umolm2~EJ_value,data=.)
NOX_hisp<-summary(NOXlong_bins_all_hisp)

lm_fits <- list('PM_lnincome',PM_lnincome,'PM_asian',PM_asian,'PM_black',PM_black,'PM_hisp',PM_hisp,
                'NOX_lnincome',NOX_lnincome,'NOX_asian',NOX_asian,'NOX_black',NOX_black,'NOX_hisp',NOX_hisp)
capture.output(lm_fits, file = "./figures/lmfits.txt")

