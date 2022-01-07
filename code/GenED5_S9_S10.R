# ++++++++++++++++++++++++++++++++++
# Code to produce figure ED5, tables S10 and S11
# ++++++++++++++++++++++++++++++++++

# Load packages
library(dplyr)
library(lfe)
library(ggplot2)
library(broom)
library(fixest)
library(splines)
library(ggridges)
library(Rmisc)
library(xtable)
library(dotwhisker)

#path to your working directory
wd_path <- "."

# ++++++++++++++++++++++++++++++++++
# Panel B, left side  --------
# ++++++++++++++++++++++++++++++++++



# Source prep file
source(paste0(wd_path,"/code/Prep.R"))

# Correct weekday
fe_data_cbg_source$DOY[fe_data_cbg_source$year==2019] <- fe_data_cbg_source$DOY[fe_data_cbg_source$year==2019] -1
table(fe_data_cbg_source$wday[fe_data_cbg_source$DOY==1],fe_data_cbg_source$year[fe_data_cbg_source$DOY==1])
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(yr = year-2019) %>% drop_na(TEMP,RH, PRECIP)



# make amny versions of FEs

# By 1
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts = cut(TEMP, breaks = seq(-20,40,1)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts = cut(RH, breaks = seq(0,100,1)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts = cut(PRECIP, breaks = seq(-1,200,1)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs = interaction(T_cuts, RH_cuts,PRECIP_cuts))



# By 2
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_2 = cut(TEMP, breaks = seq(-20,40,2)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_2 = cut(RH, breaks = seq(0,100,2)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_2 = cut(PRECIP, breaks = seq(-1,200,2)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_2 = interaction(T_cuts_2, RH_cuts_2,PRECIP_cuts_2))


# By 5
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_5 = cut(TEMP, breaks = seq(-20,40,5)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_5 = cut(RH, breaks = seq(0,100,5)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_5 = cut(PRECIP, breaks = seq(-1,200,5)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_5 = interaction(T_cuts_5, RH_cuts_5,PRECIP_cuts_5))


# By 10
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_10 = cut(TEMP, breaks = seq(-20,40,10)))
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_10 = cut(RH, breaks = seq(0,100,10)))
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_10 = cut(PRECIP, breaks = seq(-1,200,10)))

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_10 = interaction(T_cuts_10, RH_cuts_10,PRECIP_cuts_10))

# By 20
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_20 = cut(TEMP, breaks = seq(-20,40,20)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_20 = cut(RH, breaks = seq(0,100,20)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_20 = cut(PRECIP, breaks = seq(-1,200,20)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_20 = interaction(T_cuts_20, RH_cuts_20,PRECIP_cuts_20))

# By vigintile
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_vigintile = cut(TEMP, breaks = quantile(fe_data_cbg_source$TEMP,probs = seq(0,1,0.05))))
summary(fe_data_cbg_source$T_cuts_vigintile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_vigintile = cut(RH, breaks = quantile(fe_data_cbg_source$RH,probs = seq(0,1,0.05))))
summary(fe_data_cbg_source$RH_cuts_vigintile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_vigintile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_data_cbg_source$PRECIP,probs = seq(0,1,0.05))))))
summary(fe_data_cbg_source$PRECIP_cuts_vigintile)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_vigintile = interaction(T_cuts_vigintile, RH_cuts_vigintile,PRECIP_cuts_vigintile))


# By decile
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_decile = cut(TEMP, breaks = quantile(fe_data_cbg_source$TEMP,probs = seq(0,1,0.1))))
summary(fe_data_cbg_source$T_cuts_decile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_decile = cut(RH, breaks = quantile(fe_data_cbg_source$RH,probs = seq(0,1,0.1))))
summary(fe_data_cbg_source$RH_cuts_decile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_decile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_data_cbg_source$PRECIP,probs = seq(0,1,0.1))))))
summary(fe_data_cbg_source$PRECIP_cuts_decile)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_decile = interaction(T_cuts_decile, RH_cuts_decile,PRECIP_cuts_decile))

# Set up difference and 2019 levels variables
fe_data_cbg_source_diff <- fe_data_cbg_source %>% filter(year==2020)
fe_data_cbg_source_2019 <- fe_data_cbg_source %>% filter(year==2019) %>% 
    select(block_group, DOY, county, tract, sample_mean_19 = sample_mean, 
           T_cuts_19 = T_cuts, RH_cuts_19 = RH_cuts, PRECIP_cuts_19 = PRECIP_cuts, T_RH_PRECIP_FEs_19 = T_RH_PRECIP_FEs,
           T_RH_PRECIP_FEs_2_19 = T_RH_PRECIP_FEs_2,
           T_RH_PRECIP_FEs_5_19 = T_RH_PRECIP_FEs_5,
           T_RH_PRECIP_FEs_10_19 = T_RH_PRECIP_FEs_10,
           T_RH_PRECIP_FEs_20_19 = T_RH_PRECIP_FEs_20,
           T_RH_PRECIP_FEs_decile_19 = T_RH_PRECIP_FEs_decile,
           T_RH_PRECIP_FEs_vigintile_19 = T_RH_PRECIP_FEs_vigintile,
           TEMP_19 = TEMP,
           PRECIP_19 = PRECIP,
           RH_19 =RH)
fe_data_cbg_source_diff <- fe_data_cbg_source_diff %>%  left_join(fe_data_cbg_source_2019) %>% 
    mutate(sample_mean_diff = sample_mean - sample_mean_19)   %>%  filter(adj_phase==F)

# Count number of fixed effects
a <- fe_data_cbg_source_diff %>% group_by(T_cuts) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(RH_cuts) %>% dplyr::summarise(nb = n()) %>% ungroup()
c <- fe_data_cbg_source_diff %>% group_by(PRECIP_cuts) %>% dplyr::summarise(nc = n()) %>% ungroup()
d <- fe_data_cbg_source_diff %>% group_by(T_cuts_19) %>% dplyr::summarise(nd = n()) %>% ungroup()
e <- fe_data_cbg_source_diff %>% group_by(RH_cuts_19) %>% dplyr::summarise(ne = n()) %>% ungroup()
f <- fe_data_cbg_source_diff %>% group_by(PRECIP_cuts_19) %>% dplyr::summarise(nf = n()) %>% ungroup()

# Calculate how many observations are lost (assigned to their own fixed effect)
lost_uninteracted_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% 
    left_join(c) %>% summarise(total = sum(na==1))
lost_uninteracted <- fe_data_cbg_source_diff %>% left_join(a) %>% 
    left_join(b) %>% 
    left_join(c) %>%
    left_join(d) %>% 
    left_join(e) %>% 
    left_join(f) %>% mutate(lost = (na==1 | nb==1 | nc==1 | nd==1 | ne==1 | nf==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_1_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_1 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_2) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_2_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_2_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_2 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_5) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_5_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_5_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_5 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_10) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_10_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_10_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_10 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_20) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_20_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_20_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_decile) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_decile_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_decile_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_decile <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_vigintile) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_data_cbg_source_diff %>% group_by(T_RH_PRECIP_FEs_vigintile_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_vigintile_20 <- fe_data_cbg_source_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_vigintile <- fe_data_cbg_source_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

# Make training and testing data with 70% split
training_ids <- sample(unique(fe_data_cbg_source_diff$block_group),size = 0.7*length(unique(fe_data_cbg_source_diff$block_group)),replace = F)
training<-fe_data_cbg_source_diff %>% filter(block_group %in% training_ids) %>% drop_na(sample_mean_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
testing <- fe_data_cbg_source_diff %>% filter(!(block_group %in% training_ids)) %>% drop_na(sample_mean_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)

# Get degrees of freedom consumed by each control specification, get MSE on testing set across models

model_feols <- feols(data = training, sample_mean_diff ~ 1)
testing$predictions0 <- predict(model_feols, newdata = testing)
base_df <- model_feols$nparams
base <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_cuts + RH_cuts + PRECIP_cuts + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 # FE:")
uninteracted_df <- model_feols$nparams
uninteracted_df
uninteracted <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs + T_RH_PRECIP_FEs_19)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 # FE:")
by_1_df <- model_feols$nparams
by_1_df
by_1 <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_2 + T_RH_PRECIP_FEs_2_19)
testing$predictions2 <- predict(model_feols, newdata = testing)
print("by 2 # FE:")
by_2_df <- model_feols$nparams
by_2_df
by_2 <- mean((testing$sample_mean_diff-testing$predictions2)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_5 + T_RH_PRECIP_FEs_5_19)
testing$predictions5 <- predict(model_feols, newdata = testing)
print("by 5 # FE:")
by_5_df <- model_feols$nparams
by_5_df
by_5 <- mean((testing$sample_mean_diff-testing$predictions5)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_10 + T_RH_PRECIP_FEs_10_19)
testing$predictions10 <- predict(model_feols, newdata = testing)
print("by 10 # FE:")
by_10_df <- model_feols$nparams
by_10_df
by_10 <- mean((testing$sample_mean_diff-testing$predictions10)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_20 + T_RH_PRECIP_FEs_20_19)
testing$predictions20 <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_20_df <- model_feols$nparams
by_20_df
by_20 <- mean((testing$sample_mean_diff-testing$predictions20)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_decile + T_RH_PRECIP_FEs_decile_19)
testing$predictionsdecile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_decile_df <- model_feols$nparams
by_decile_df
by_decile <- mean((testing$sample_mean_diff-testing$predictionsdecile)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_vigintile + T_RH_PRECIP_FEs_vigintile_19)
testing$predictionsvigintile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_vigintile_df <- model_feols$nparams
by_vigintile_df
by_vigintile <- mean((testing$sample_mean_diff-testing$predictionsvigintile)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 2020 # FE:")
by_1_2020_df <- model_feols$nparams
by_1_2020_df
by_1_2020 <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_2)
testing$predictions2 <- predict(model_feols, newdata = testing)
print("by 2 2020 # FE:")
by_2_2020_df <- model_feols$nparams
by_2_2020_df
by_2_2020 <- mean((testing$sample_mean_diff-testing$predictions2)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_5)
testing$predictions5 <- predict(model_feols, newdata = testing)
print("by 5 2020 # FE:")
by_5_2020_df <- model_feols$nparams
by_5_2020_df
by_5_2020 <- mean((testing$sample_mean_diff-testing$predictions5)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_10)
testing$predictions10 <- predict(model_feols, newdata = testing)
print("by 10 2020 # FE:")
by_10_2020_df <- model_feols$nparams
by_10_2020_df
by_10_2020 <- mean((testing$sample_mean_diff-testing$predictions10)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_20)
testing$predictions20 <- predict(model_feols, newdata = testing)
print("by 20 2020 # FE:")
by_20_2020_df <- model_feols$nparams
by_20_2020_df
by_20_2020 <- mean((testing$sample_mean_diff-testing$predictions20)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_decile)
testing$predictionsdecile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_decile_2020_df <- model_feols$nparams
by_decile_2020_df
by_decile_2020 <- mean((testing$sample_mean_diff-testing$predictionsdecile)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_vigintile)
testing$predictionsvigintile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_vigintile_2020_df <- model_feols$nparams
by_vigintile_2020_df
by_vigintile_2020 <- mean((testing$sample_mean_diff-testing$predictionsvigintile)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 1))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 1 2019 # FE:")
Poly_1_2020_df <- model_feols$nparams
Poly_1_2020_df
Poly_1_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 2))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 2 2019 # FE:")
Poly_2_2020_df <- model_feols$nparams
Poly_2_2020_df
Poly_2_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 3 2019 # FE:")
Poly_3_2020_df <- model_feols$nparams
Poly_3_2020_df
Poly_3_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 1))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 1 both # FE:")
Poly_1_df <- model_feols$nparams
Poly_1_df
Poly_1 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19, degree = 2))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 2 both # FE:")
Poly_2_df <- model_feols$nparams
Poly_2_df
Poly_2 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 3 both # FE:")
Poly_3_df <- model_feols$nparams
Poly_3_df
Poly_3 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)


model_feols <- feols(data = training, sample_mean_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("splines # FE:")
cubic_spline_2020_df <- model_feols$nparams
cubic_spline_2020_df
cubic_spline_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, sample_mean_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3) + bs(TEMP_19,3)*bs(PRECIP_19, 3)*bs(RH_19,3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("splines # FE:")
cubic_spline_df <- model_feols$nparams
cubic_spline_df
cubic_spline <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)

Results <- data.frame(base = base, uninteracted, by_1 = by_1, by_2 = by_2, by_5 = by_5, 
                      by_10 = by_10, by_20 = by_20,by_decile = by_decile, by_vigintile = by_vigintile,
                      by_1_2020 = by_10_2020, by_2_2020 = by_2_2020, by_5_2020 = by_5_2020, 
                      by_10_2020 = by_10_2020, by_20_2020 = by_20_2020,by_decile_2020 = by_decile_2020,
                      by_vigintile_2020 = by_vigintile_2020,
                      Poly_1_2020 = Poly_1_2020, Poly_2_2020 = Poly_2_2020, 
                      Poly_3_2020 = Poly_3_2020,  Poly_1 = Poly_1, Poly_2 = Poly_2, 
                      Poly_3 = Poly_3, cubic_spline = cubic_spline,
                      cubic_spline_2020 = cubic_spline_2020)

# Re-split data 100 times, record MSE for each split and each model

for(i in 1:99) {
    training_ids <- sample(unique(fe_data_cbg_source_diff$block_group),size = 0.7*length(unique(fe_data_cbg_source_diff$block_group)),replace = F)
    training<-fe_data_cbg_source_diff %>% filter(block_group %in% training_ids) %>% drop_na(sample_mean_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
    testing <- fe_data_cbg_source_diff %>% filter(!(block_group %in% training_ids)) %>% drop_na(sample_mean_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1)
    testing$predictions0 <- predict(model_feols, newdata = testing)
    
    base <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_cuts + RH_cuts + PRECIP_cuts + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    uninteracted <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs + T_RH_PRECIP_FEs_19)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs)) + length(unique(training$T_RH_PRECIP_FEs_19))
    by_1 <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_2 + T_RH_PRECIP_FEs_2_19)
    testing$predictions2 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_2)) + length(unique(training$T_RH_PRECIP_FEs_2_19))
    by_2 <- mean((testing$sample_mean_diff-testing$predictions2)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_5 + T_RH_PRECIP_FEs_5_19)
    testing$predictions5 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_5)) + length(unique(training$T_RH_PRECIP_FEs_5_19))
    by_5 <- mean((testing$sample_mean_diff-testing$predictions5)^2, na.rm=T)
    
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_10 + T_RH_PRECIP_FEs_10_19)
    testing$predictions10 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_10)) + length(unique(training$T_RH_PRECIP_FEs_10_19))
    by_10 <- mean((testing$sample_mean_diff-testing$predictions10)^2, na.rm=T)
    
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_20 + T_RH_PRECIP_FEs_20_19)
    testing$predictions20 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_20)) + length(unique(training$T_RH_PRECIP_FEs_20_19))
    by_20 <- mean((testing$sample_mean_diff-testing$predictions20)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_decile + T_RH_PRECIP_FEs_decile_19)
    testing$predictionsdecile <- predict(model_feols, newdata = testing)
    by_decile <- mean((testing$sample_mean_diff-testing$predictionsdecile)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_vigintile + T_RH_PRECIP_FEs_vigintile_19)
    testing$predictionsvigintile <- predict(model_feols, newdata = testing)
    by_vigintile <- mean((testing$sample_mean_diff-testing$predictionsvigintile)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    by_1_2020 <- mean((testing$sample_mean_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_2)
    testing$predictions2 <- predict(model_feols, newdata = testing)
    by_2_2020 <- mean((testing$sample_mean_diff-testing$predictions2)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_5)
    testing$predictions5 <- predict(model_feols, newdata = testing)
    by_5_2020 <- mean((testing$sample_mean_diff-testing$predictions5)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_10)
    testing$predictions10 <- predict(model_feols, newdata = testing)
    by_10_2020 <- mean((testing$sample_mean_diff-testing$predictions10)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_20)
    testing$predictions20 <- predict(model_feols, newdata = testing)
    by_20_2020 <- mean((testing$sample_mean_diff-testing$predictions20)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_decile)
    testing$predictionsdecile <- predict(model_feols, newdata = testing)
    by_decile_2020 <- mean((testing$sample_mean_diff-testing$predictionsdecile)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ 1 | T_RH_PRECIP_FEs_vigintile)
    testing$predictionsvigintile <- predict(model_feols, newdata = testing)
    by_vigintile_2020 <- mean((testing$sample_mean_diff-testing$predictionsvigintile)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 1))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_1_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 2))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_2_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,  degree = 3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_3_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 1))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_1 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19, degree = 2))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_2 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_3 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    library(splines)
    
    model_feols <- feols(data = training, sample_mean_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    cubic_spline_2020 <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, sample_mean_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3) + bs(TEMP_19,3)*bs(PRECIP_19, 3)*bs(RH_19,3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    cubic_spline <- mean((testing$sample_mean_diff-testing$predictions0)^2, na.rm=T)
    
    Results<-rbind(Results,c(base,uninteracted,by_1,by_2,by_5,by_10,by_20,by_decile,by_vigintile,
                             by_1_2020,by_2_2020,by_5_2020,by_10_2020,by_20_2020,by_decile_2020,by_vigintile_2020,
                             Poly_1_2020,
                             Poly_2_2020,Poly_3_2020,Poly_1,Poly_2,
                             Poly_3,cubic_spline,cubic_spline_2020))
}

# Plot ED5_B left side

to_plot<-Results %>% pivot_longer(cols = base:cubic_spline_2020, names_to = "model", values_to = "MSE") %>% mutate_if(is.character,str_replace_all,pattern = "_", replacement = " ")
to_plot$model <- factor(to_plot$model, 
                        levels = c("base","uninteracted","by 1","by 2","by 5","by 10","by 20","by decile","by vigintile","by 1 2020","by 2 2020",
                                   "by 5 2020","by 10 2020","by 20 2020","by decile 2020","by vigintile 2020","Poly 1 2020","Poly 2 2020",
                                   "Poly 3 2020","Poly 1","Poly 2","Poly 3","cubic spline",
                                   "cubic spline 2020"))

means <- to_plot %>% group_by(model) %>% dplyr::summarise(means = mean(MSE)) %>% arrange(means) %>% mutate(model=factor(as.character(model), levels=model))

means %>% left_join(to_plot) %>% ggplot( aes(x = model, y = MSE)) + geom_point(aes(alpha=0.1))

means %>% left_join(to_plot) %>% ggplot(aes(x = MSE, y = model, fill = ..x..)) +  
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+ 
    theme_bw() + theme(legend.position = "none")
ggsave(file = paste0(wd_path,"figures/FigED5_B1.pdf"))

# ++++++++++++++++++++++++++++++++++
# Table S10  --------
# ++++++++++++++++++++++++++++++++++
summarySE(to_plot, measurevar="MSE", groupvars=c("model")) %>% mutate(df =
                                                                          c(base_df,uninteracted_df, by_1_df, by_2_df, by_5_df, by_10_df,
                                                                            by_20_df, by_decile_df, by_vigintile_df, by_1_2020_df, by_2_2020_df, by_5_2020_df,
                                                                            by_10_2020_df, by_20_2020_df, by_decile_2020_df, by_vigintile_2020_df,
                                                                            Poly_1_2020_df,
                                                                            Poly_2_2020_df,Poly_3_2020_df,Poly_1_df,Poly_2_df,
                                                                            Poly_3_df,cubic_spline_2020_df,cubic_spline_df),
                                                                      lost = c(0,lost_uninteracted,lost_by_1,lost_by_2,lost_by_5,lost_by_10,lost_by_20,lost_by_decile,lost_by_vigintile,
                                                                               lost_by_1_20,lost_by_2_20,lost_by_5_20,lost_by_10_20,lost_by_20_20,lost_by_decile_20,lost_by_vigintile_20,rep(NA,8))
) %>% arrange(MSE) %>% select(-c("N","se","ci")) %>% mutate(model = as.character(model)) %>% print()

summary_stats<- summarySE(to_plot, measurevar="MSE", groupvars=c("model")) %>% mutate(df =
                                                                                          c(base_df,uninteracted_df, by_1_df, by_2_df, by_5_df, by_10_df,
                                                                                            by_20_df, by_decile_df, by_vigintile_df, by_1_2020_df, by_2_2020_df, by_5_2020_df,
                                                                                            by_10_2020_df, by_20_2020_df, by_decile_2020_df, by_vigintile_2020_df,
                                                                                            Poly_1_2020_df,
                                                                                            Poly_2_2020_df,Poly_3_2020_df,Poly_1_df,Poly_2_df,
                                                                                            Poly_3_df,cubic_spline_2020_df,cubic_spline_df),
                                                                                      lost = c(0,lost_uninteracted,lost_by_1,lost_by_2,lost_by_5,lost_by_10,lost_by_20,lost_by_decile,lost_by_vigintile,
                                                                                               lost_by_1_20,lost_by_2_20,lost_by_5_20,lost_by_10_20,lost_by_20_20,lost_by_decile_20,lost_by_vigintile_20,rep(NA,8))
) %>% arrange(MSE) %>% select(-c("N","se","ci")) %>% mutate(model = as.character(model)) %>%
    mutate(MSE = round(MSE,1),sd = round(sd,1))


tab_s11<-xtable(summary_stats, auto=T, format = "s")
print(tab_s11, file=paste0(wd_path,"figures/table_S10.tex"), sep="\n" )

# ++++++++++++++++++++++++++++++++++
# Panel B, right side  --------
# ++++++++++++++++++++++++++++++++++

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly  %>% mutate(wkf = as.factor(wk), post_emer = as.numeric(wk>9) ) 

# make coarser FEs

# By 1
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts = cut(TEMP, breaks = seq(-13,35,1)))
summary(fe_sat_data_cbg_weekly$T_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts = cut(RH, breaks = seq(0,100,1)))
summary(fe_sat_data_cbg_weekly$RH_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts = cut(PRECIP, breaks = seq(-1,61,1)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs = interaction(T_cuts, RH_cuts,PRECIP_cuts))
lost_by_1 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

# By 2
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_2 = cut(TEMP, breaks = seq(-13,35,2)))
summary(fe_sat_data_cbg_weekly$T_cuts_2)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_2 = cut(RH, breaks = seq(0,100,2)))
summary(fe_sat_data_cbg_weekly$RH_cuts_2)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_2 = cut(PRECIP, breaks = seq(-1,61,2)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_2)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_2 = interaction(T_cuts_2, RH_cuts_2,PRECIP_cuts_2))

lost_by_2 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_2) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

# By 5
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_5 = cut(TEMP, breaks = seq(-13,35,5)))
summary(fe_sat_data_cbg_weekly$T_cuts5)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_5 = cut(RH, breaks = seq(0,100,5)))
summary(fe_sat_data_cbg_weekly$RH_cuts5)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_5 = cut(PRECIP, breaks = seq(-1,61,5)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts5)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_5 = interaction(T_cuts_5, RH_cuts_5,PRECIP_cuts_5))

lost_by_5 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_5) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

# By 10
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_10 = cut(TEMP, breaks = seq(-13,35,10)))
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_10 = cut(RH, breaks = seq(0,100,10)))
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_10 = cut(PRECIP, breaks = seq(-1,61,10)))

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_10 = interaction(T_cuts_10, RH_cuts_10,PRECIP_cuts_10))

lost_by_10 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_10) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


# By 20
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_20 = cut(TEMP, breaks = seq(-13,35,20)))
summary(fe_sat_data_cbg_weekly$T_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_20 = cut(RH, breaks = seq(0,100,20)))
summary(fe_sat_data_cbg_weekly$RH_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_20 = cut(PRECIP, breaks = seq(-1,61,20)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_20 = interaction(T_cuts_20, RH_cuts_20,PRECIP_cuts_20))

lost_by_20 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_20) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


# By vigintile
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_vigintile = cut(TEMP, breaks = quantile(fe_sat_data_cbg_weekly$TEMP,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_sat_data_cbg_weekly$T_cuts_vigintile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_vigintile = cut(RH, breaks = quantile(fe_sat_data_cbg_weekly$RH,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_sat_data_cbg_weekly$RH_cuts_vigintile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_vigintile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_sat_data_cbg_weekly$PRECIP,probs = seq(0,1,0.05),na.rm=T)))))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_vigintile)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_vigintile = interaction(T_cuts_vigintile, RH_cuts_vigintile,PRECIP_cuts_vigintile))

lost_by_vigintile <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_vigintile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


# By decile
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_decile = cut(TEMP, breaks = quantile(fe_sat_data_cbg_weekly$TEMP,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_sat_data_cbg_weekly$T_cuts_decile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_decile = cut(RH, breaks = quantile(fe_sat_data_cbg_weekly$RH,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_sat_data_cbg_weekly$RH_cuts_decile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_decile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_sat_data_cbg_weekly$PRECIP,probs = seq(0,1,0.1),na.rm=T)))))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_decile)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_decile = interaction(T_cuts_decile, RH_cuts_decile,PRECIP_cuts_decile))

lost_by_decile <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_decile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

# Set up difference and 2019 levels variables
fe_sat_data_cbg_weekly_diff <- fe_sat_data_cbg_weekly %>% filter(year==2020)
fe_sat_data_cbg_weekly_2019 <- fe_sat_data_cbg_weekly %>% filter(year==2019) %>% 
    select(block_group, wk, county, tract, NO2_19 = NO2, T_cuts_19 = T_cuts, RH_cuts_19 = RH_cuts, PRECIP_cuts_19=PRECIP_cuts,
           T_RH_PRECIP_FEs_19 = T_RH_PRECIP_FEs,
           T_RH_PRECIP_FEs_2_19 = T_RH_PRECIP_FEs_2,
           T_RH_PRECIP_FEs_5_19 = T_RH_PRECIP_FEs_5,
           T_RH_PRECIP_FEs_10_19 = T_RH_PRECIP_FEs_10,
           T_RH_PRECIP_FEs_20_19 = T_RH_PRECIP_FEs_20,
           T_RH_PRECIP_FEs_decile_19 = T_RH_PRECIP_FEs_decile,
           T_RH_PRECIP_FEs_vigintile_19 = T_RH_PRECIP_FEs_vigintile,
           TEMP_19 = TEMP,
           PRECIP_19 = PRECIP,
           RH_19 =RH,
           NO2_19 = NO2)


fe_sat_data_cbg_weekly_diff <- fe_sat_data_cbg_weekly_diff %>%  left_join(fe_sat_data_cbg_weekly_2019) %>% 
    mutate(NO2_diff = NO2 - NO2_19) %>% 
    filter(adj_phase==F)

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_cuts) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(RH_cuts) %>% dplyr::summarise(nb = n()) %>% ungroup()
c <- fe_sat_data_cbg_weekly_diff %>% group_by(PRECIP_cuts) %>% dplyr::summarise(nc = n()) %>% ungroup()
d <- fe_sat_data_cbg_weekly_diff %>% group_by(T_cuts_19) %>% dplyr::summarise(nd = n()) %>% ungroup()
e <- fe_sat_data_cbg_weekly_diff %>% group_by(RH_cuts_19) %>% dplyr::summarise(ne = n()) %>% ungroup()
f <- fe_sat_data_cbg_weekly_diff %>% group_by(PRECIP_cuts_19) %>% dplyr::summarise(nf = n()) %>% ungroup()

lost_uninteracted_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% 
    left_join(c) %>% summarise(total = sum(na==1))

# Count number of fixed effects

lost_uninteracted <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% 
    left_join(b) %>% 
    left_join(c) %>%
    left_join(d) %>% 
    left_join(e) %>% 
    left_join(f) %>% mutate(lost = (na==1 | nb==1 | nc==1 | nd==1 | ne==1 | nf==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_1_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_1 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_2) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_2_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_2_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_2 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_5) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_5_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_5_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_5 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_10) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_10_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_10_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_10 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_20) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_20_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_20_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_decile) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_decile_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_decile_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_decile <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))

a <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_vigintile) %>% dplyr::summarise(na = n()) %>% ungroup()
b <- fe_sat_data_cbg_weekly_diff %>% group_by(T_RH_PRECIP_FEs_vigintile_19) %>% dplyr::summarise(nb = n()) %>% ungroup()
lost_by_vigintile_20 <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% summarise(total = sum(na==1))
lost_by_vigintile <- fe_sat_data_cbg_weekly_diff %>% left_join(a) %>% left_join(b) %>% mutate(lost = (na==1 | nb==1)) %>% summarise(total = sum(lost))


# Make training and testing data with 70% split

training_ids <- sample(unique(fe_sat_data_cbg_weekly_diff$block_group),size = 0.7*length(unique(fe_sat_data_cbg_weekly_diff$block_group)),replace = F)
training<-fe_sat_data_cbg_weekly_diff %>% filter(block_group %in% training_ids) %>% drop_na(NO2_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
testing <- fe_sat_data_cbg_weekly_diff %>% filter(!(block_group %in% training_ids)) %>% drop_na(NO2_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)

# Get degrees of freedom consumed by each control specification, get MSE on testing set

model_feols <- feols(data = training, NO2_diff ~ 1)
testing$predictions0 <- predict(model_feols, newdata = testing)
base_df <- model_feols$nparams
base <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_cuts + RH_cuts + PRECIP_cuts + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 # FE:")
uninteracted_df <- model_feols$nparams
uninteracted_df
uninteracted <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs + T_RH_PRECIP_FEs_19)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 # FE:")
by_1_df <- model_feols$nparams
by_1_df
by_1 <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_2 + T_RH_PRECIP_FEs_2_19)
testing$predictions2 <- predict(model_feols, newdata = testing)
print("by 2 # FE:")
by_2_df <- model_feols$nparams
by_2_df
by_2 <- mean((testing$NO2_diff-testing$predictions2)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_5 + T_RH_PRECIP_FEs_5_19)
testing$predictions5 <- predict(model_feols, newdata = testing)
print("by 5 # FE:")
by_5_df <- model_feols$nparams
by_5_df
by_5 <- mean((testing$NO2_diff-testing$predictions5)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_10 + T_RH_PRECIP_FEs_10_19)
testing$predictions10 <- predict(model_feols, newdata = testing)
print("by 10 # FE:")
by_10_df <- model_feols$nparams
by_10_df
by_10 <- mean((testing$NO2_diff-testing$predictions10)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_20 + T_RH_PRECIP_FEs_20_19)
testing$predictions20 <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_20_df <- model_feols$nparams
by_20_df
by_20 <- mean((testing$NO2_diff-testing$predictions20)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_decile + T_RH_PRECIP_FEs_decile_19)
testing$predictionsdecile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_decile_df <- model_feols$nparams
by_decile_df
by_decile <- mean((testing$NO2_diff-testing$predictionsdecile)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_vigintile + T_RH_PRECIP_FEs_vigintile_19)
testing$predictionsvigintile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_vigintile_df <- model_feols$nparams
by_vigintile_df
by_vigintile <- mean((testing$NO2_diff-testing$predictionsvigintile)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs)
testing$predictions1 <- predict(model_feols, newdata = testing)
print("by 1 2020 # FE:")
by_1_2020_df <- model_feols$nparams
by_1_2020_df
by_1_2020 <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_2)
testing$predictions2 <- predict(model_feols, newdata = testing)
print("by 2 2020 # FE:")
by_2_2020_df <- model_feols$nparams
by_2_2020_df
by_2_2020 <- mean((testing$NO2_diff-testing$predictions2)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_5)
testing$predictions5 <- predict(model_feols, newdata = testing)
print("by 5 2020 # FE:")
by_5_2020_df <- model_feols$nparams
by_5_2020_df
by_5_2020 <- mean((testing$NO2_diff-testing$predictions5)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_10)
testing$predictions10 <- predict(model_feols, newdata = testing)
print("by 10 2020 # FE:")
by_10_2020_df <- model_feols$nparams
by_10_2020_df
by_10_2020 <- mean((testing$NO2_diff-testing$predictions10)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_20)
testing$predictions20 <- predict(model_feols, newdata = testing)
print("by 20 2020 # FE:")
by_20_2020_df <- model_feols$nparams
by_20_2020_df
by_20_2020 <- mean((testing$NO2_diff-testing$predictions20)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_decile)
testing$predictionsdecile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_decile_2020_df <- model_feols$nparams
by_decile_2020_df
by_decile_2020 <- mean((testing$NO2_diff-testing$predictionsdecile)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_vigintile)
testing$predictionsvigintile <- predict(model_feols, newdata = testing)
print("by 20 # FE:")
by_vigintile_2020_df <- model_feols$nparams
by_vigintile_2020_df
by_vigintile_2020 <- mean((testing$NO2_diff-testing$predictionsvigintile)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 1))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 1 2019 # FE:")
Poly_1_2020_df <- model_feols$nparams
Poly_1_2020_df
Poly_1_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 2))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 2 2019 # FE:")
Poly_2_2020_df <- model_feols$nparams
Poly_2_2020_df
Poly_2_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 3 2019 # FE:")
Poly_3_2020_df <- model_feols$nparams
Poly_3_2020_df
Poly_3_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 1))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 1 both # FE:")
Poly_1_df <- model_feols$nparams
Poly_1_df
Poly_1 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19, degree = 2))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 2 both # FE:")
Poly_2_df <- model_feols$nparams
Poly_2_df
Poly_2 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("poly 3 both # FE:")
Poly_3_df <- model_feols$nparams
Poly_3_df
Poly_3 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)


model_feols <- feols(data = training, NO2_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("splines # FE:")
cubic_spline_2020_df <- model_feols$nparams
cubic_spline_2020_df
cubic_spline_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

model_feols <- feols(data = training, NO2_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3) + bs(TEMP_19,3)*bs(PRECIP_19, 3)*bs(RH_19,3))
testing$predictions0 <- predict(model_feols, newdata = testing)
print("splines # FE:")
cubic_spline_df <- model_feols$nparams
cubic_spline_df
cubic_spline <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)

Results <- data.frame(base = base, uninteracted, by_1 = by_1, by_2 = by_2, by_5 = by_5, 
                      by_10 = by_10, by_20 = by_20,by_decile = by_decile, by_vigintile = by_vigintile,
                      by_1_2020 = by_10_2020, by_2_2020 = by_2_2020, by_5_2020 = by_5_2020, 
                      by_10_2020 = by_10_2020, by_20_2020 = by_20_2020,by_decile_2020 = by_decile_2020,
                      by_vigintile_2020 = by_vigintile_2020,
                      Poly_1_2020 = Poly_1_2020, Poly_2_2020 = Poly_2_2020, 
                      Poly_3_2020 = Poly_3_2020,  Poly_1 = Poly_1, Poly_2 = Poly_2, 
                      Poly_3 = Poly_3, cubic_spline = cubic_spline,
                      cubic_spline_2020 = cubic_spline_2020)

# Re-split data 100 times, record MSE for each split and each model

for(i in 1:99) {
    print(i)
    training_ids <- sample(unique(fe_sat_data_cbg_weekly_diff$block_group),size = 0.7*length(unique(fe_sat_data_cbg_weekly_diff$block_group)),replace = F)
    training<-fe_sat_data_cbg_weekly_diff %>% filter(block_group %in% training_ids) %>% drop_na(NO2_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
    testing <- fe_sat_data_cbg_weekly_diff %>% filter(!(block_group %in% training_ids)) %>% drop_na(NO2_diff, T_RH_PRECIP_FEs, T_RH_PRECIP_FEs_19)
    
    model_feols <- feols(data = training, NO2_diff ~ 1)
    testing$predictions0 <- predict(model_feols, newdata = testing)
    
    base <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_cuts + RH_cuts + PRECIP_cuts + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    uninteracted <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs + T_RH_PRECIP_FEs_19)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs)) + length(unique(training$T_RH_PRECIP_FEs_19))
    by_1 <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_2 + T_RH_PRECIP_FEs_2_19)
    testing$predictions2 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_2)) + length(unique(training$T_RH_PRECIP_FEs_2_19))
    by_2 <- mean((testing$NO2_diff-testing$predictions2)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_5 + T_RH_PRECIP_FEs_5_19)
    testing$predictions5 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_5)) + length(unique(training$T_RH_PRECIP_FEs_5_19))
    by_5 <- mean((testing$NO2_diff-testing$predictions5)^2, na.rm=T)
    
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_10 + T_RH_PRECIP_FEs_10_19)
    testing$predictions10 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_10)) + length(unique(training$T_RH_PRECIP_FEs_10_19))
    by_10 <- mean((testing$NO2_diff-testing$predictions10)^2, na.rm=T)
    
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_20 + T_RH_PRECIP_FEs_20_19)
    testing$predictions20 <- predict(model_feols, newdata = testing)
    length(unique(training$T_RH_PRECIP_FEs_20)) + length(unique(training$T_RH_PRECIP_FEs_20_19))
    by_20 <- mean((testing$NO2_diff-testing$predictions20)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_decile + T_RH_PRECIP_FEs_decile_19)
    testing$predictionsdecile <- predict(model_feols, newdata = testing)
    by_decile <- mean((testing$NO2_diff-testing$predictionsdecile)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_vigintile + T_RH_PRECIP_FEs_vigintile_19)
    testing$predictionsvigintile <- predict(model_feols, newdata = testing)
    by_vigintile <- mean((testing$NO2_diff-testing$predictionsvigintile)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs)
    testing$predictions1 <- predict(model_feols, newdata = testing)
    by_1_2020 <- mean((testing$NO2_diff-testing$predictions1)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_2)
    testing$predictions2 <- predict(model_feols, newdata = testing)
    by_2_2020 <- mean((testing$NO2_diff-testing$predictions2)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_5)
    testing$predictions5 <- predict(model_feols, newdata = testing)
    by_5_2020 <- mean((testing$NO2_diff-testing$predictions5)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_10)
    testing$predictions10 <- predict(model_feols, newdata = testing)
    by_10_2020 <- mean((testing$NO2_diff-testing$predictions10)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_20)
    testing$predictions20 <- predict(model_feols, newdata = testing)
    by_20_2020 <- mean((testing$NO2_diff-testing$predictions20)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_decile)
    testing$predictionsdecile <- predict(model_feols, newdata = testing)
    by_decile_2020 <- mean((testing$NO2_diff-testing$predictionsdecile)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ 1 | T_RH_PRECIP_FEs_vigintile)
    testing$predictionsvigintile <- predict(model_feols, newdata = testing)
    by_vigintile_2020 <- mean((testing$NO2_diff-testing$predictionsvigintile)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 1))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_1_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 2))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_2_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,  degree = 3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_3_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 1))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_1 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19, degree = 2))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_2 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ polym(TEMP, RH, PRECIP,TEMP_19, RH_19, PRECIP_19,  degree = 3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    Poly_3 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    library(splines)
    
    model_feols <- feols(data = training, NO2_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    cubic_spline_2020 <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    model_feols <- feols(data = training, NO2_diff ~ bs(TEMP,3)*bs(PRECIP, 3)*bs(RH,3) + bs(TEMP_19,3)*bs(PRECIP_19, 3)*bs(RH_19,3))
    testing$predictions0 <- predict(model_feols, newdata = testing)
    cubic_spline <- mean((testing$NO2_diff-testing$predictions0)^2, na.rm=T)
    
    Results<-rbind(Results,c(base,uninteracted,by_1,by_2,by_5,by_10,by_20,by_decile,by_vigintile,
                             by_1_2020,by_2_2020,by_5_2020,by_10_2020,by_20_2020,by_decile_2020,by_vigintile_2020,
                             Poly_1_2020,
                             Poly_2_2020,Poly_3_2020,Poly_1,Poly_2,
                             Poly_3,cubic_spline_2020,cubic_spline))
}

to_plot<-Results %>% pivot_longer(cols = base:cubic_spline_2020, names_to = "model", values_to = "MSE") %>% mutate_if(is.character,str_replace_all,pattern = "_", replacement = " ")
to_plot$model <- factor(to_plot$model, 
                        levels = c("base","uninteracted","by 1","by 2","by 5","by 10","by 20","by decile","by vigintile","by 1 2020","by 2 2020",
                                   "by 5 2020","by 10 2020","by 20 2020","by decile 2020","by vigintile 2020","Poly 1 2020","Poly 2 2020",
                                   "Poly 3 2020","Poly 1","Poly 2","Poly 3","cubic spline",
                                   "cubic spline 2020"))

means <- to_plot %>% group_by(model) %>% dplyr::summarise(means = mean(MSE)) %>% arrange(means) %>% mutate(model=factor(as.character(model), levels=model))

means %>% left_join(to_plot) %>% ggplot( aes(x = model, y = MSE)) + geom_point(aes(alpha=0.1))

means %>% left_join(to_plot) %>% ggplot(aes(x = MSE, y = model, fill = ..x..)) +  
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+ 
    theme_bw() + theme(legend.position = "none")
ggsave(file = paste0(wd_path,"figures/FigED5_B2.pdf"))

# ++++++++++++++++++++++++++++++++++
# Table S9  --------
# ++++++++++++++++++++++++++++++++++

summarySE(to_plot, measurevar="MSE", groupvars=c("model")) %>% mutate(df =
                                                                          c(base_df,uninteracted_df, by_1_df, by_2_df, by_5_df, by_10_df,
                                                                            by_20_df, by_decile_df, by_vigintile_df, by_1_2020_df, by_2_2020_df, by_5_2020_df,
                                                                            by_10_2020_df, by_20_2020_df, by_decile_2020_df, by_vigintile_2020_df,
                                                                            Poly_1_2020_df,
                                                                            Poly_2_2020_df,Poly_3_2020_df,Poly_1_df,Poly_2_df,
                                                                            Poly_3_df,cubic_spline_2020_df,cubic_spline_df),
                                                                      lost = c(0,lost_uninteracted,lost_by_1,lost_by_2,lost_by_5,lost_by_10,lost_by_20,lost_by_decile,lost_by_vigintile,
                                                                               lost_by_1_20,lost_by_2_20,lost_by_5_20,lost_by_10_20,lost_by_20_20,lost_by_decile_20,lost_by_vigintile_20,rep(NA,8))
) %>% arrange(MSE) %>% select(-c("N","se","ci")) %>% mutate(model = as.character(model)) %>% print()

to_plot<-to_plot %>% mutate(MSE = MSE * 1e11)
summary_stats<- summarySE(to_plot, measurevar="MSE", groupvars=c("model")) %>% mutate(df =
                                                                                          c(base_df,uninteracted_df, by_1_df, by_2_df, by_5_df, by_10_df,
                                                                                            by_20_df, by_decile_df, by_vigintile_df, by_1_2020_df, by_2_2020_df, by_5_2020_df,
                                                                                            by_10_2020_df, by_20_2020_df, by_decile_2020_df, by_vigintile_2020_df,
                                                                                            Poly_1_2020_df,
                                                                                            Poly_2_2020_df,Poly_3_2020_df,Poly_1_df,Poly_2_df,
                                                                                            Poly_3_df,cubic_spline_2020_df,cubic_spline_df),
                                                                                      lost = c(0,lost_uninteracted,lost_by_1,lost_by_2,lost_by_5,lost_by_10,lost_by_20,lost_by_decile,lost_by_vigintile,
                                                                                               lost_by_1_20,lost_by_2_20,lost_by_5_20,lost_by_10_20,lost_by_20_20,lost_by_decile_20,lost_by_vigintile_20,rep(NA,8))
) %>% arrange(MSE) %>% select(-c("N","se","ci")) %>% mutate(model = as.character(model)) %>%
    mutate(MSE = round(MSE,1),sd = round(sd,1))

tab_s10<-xtable(summary_stats, auto = T, digits = 2)
print(tab_s10, file=paste0(wd_path,"figures/table_S9.tex"), sep="\n" )
# ++++++++++++++++++++++++++++++++++
# Panel A, right  --------
# ++++++++++++++++++++++++++++++++++
wd_path <- "."
source(paste0(wd_path,"/code/Prep.R"))

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% drop_na(TEMP,RH, PRECIP)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts = cut(TEMP, breaks = seq(-13,35,1)))
summary(fe_sat_data_cbg_weekly$T_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts = cut(RH, breaks = seq(0,100,1)))
summary(fe_sat_data_cbg_weekly$RH_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts = cut(PRECIP, breaks = seq(-1,61,1)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs = interaction(T_cuts, RH_cuts,PRECIP_cuts))
lost_by_1 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_2 = cut(TEMP, breaks = seq(-13,35,2)))
summary(fe_sat_data_cbg_weekly$T_cuts_2)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_2 = cut(RH, breaks = seq(0,100,2)))
summary(fe_sat_data_cbg_weekly$RH_cuts_2)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_2 = cut(PRECIP, breaks = seq(-1,61,2)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_2)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_2 = interaction(T_cuts_2, RH_cuts_2,PRECIP_cuts_2))

lost_by_2 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_2) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_5 = cut(TEMP, breaks = seq(-13,35,5)))
summary(fe_sat_data_cbg_weekly$T_cuts5)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_5 = cut(RH, breaks = seq(0,100,5)))
summary(fe_sat_data_cbg_weekly$RH_cuts5)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_5 = cut(PRECIP, breaks = seq(-1,61,5)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts5)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_5 = interaction(T_cuts_5, RH_cuts_5,PRECIP_cuts_5))

lost_by_5 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_5) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_10 = cut(TEMP, breaks = seq(-13,35,10)))
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_10 = cut(RH, breaks = seq(0,100,10)))
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_10 = cut(PRECIP, breaks = seq(-1,61,10)))

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_10 = interaction(T_cuts_10, RH_cuts_10,PRECIP_cuts_10))

lost_by_10 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_10) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_20 = cut(TEMP, breaks = seq(-13,35,20)))
summary(fe_sat_data_cbg_weekly$T_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_20 = cut(RH, breaks = seq(0,100,20)))
summary(fe_sat_data_cbg_weekly$RH_cuts)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_20 = cut(PRECIP, breaks = seq(-1,61,20)))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_20 = interaction(T_cuts_20, RH_cuts_20,PRECIP_cuts_20))

lost_by_20 <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_20) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_vigintile = cut(TEMP, breaks = quantile(fe_sat_data_cbg_weekly$TEMP,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_sat_data_cbg_weekly$T_cuts_vigintile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_vigintile = cut(RH, breaks = quantile(fe_sat_data_cbg_weekly$RH,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_sat_data_cbg_weekly$RH_cuts_vigintile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_vigintile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_sat_data_cbg_weekly$PRECIP,probs = seq(0,1,0.05),na.rm=T)))))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_vigintile)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_vigintile = interaction(T_cuts_vigintile, RH_cuts_vigintile,PRECIP_cuts_vigintile))

lost_by_vigintile <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_vigintile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_cuts_decile = cut(TEMP, breaks = quantile(fe_sat_data_cbg_weekly$TEMP,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_sat_data_cbg_weekly$T_cuts_decile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(RH_cuts_decile = cut(RH, breaks = quantile(fe_sat_data_cbg_weekly$RH,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_sat_data_cbg_weekly$RH_cuts_decile)
fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(PRECIP_cuts_decile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_sat_data_cbg_weekly$PRECIP,probs = seq(0,1,0.1),na.rm=T)))))
summary(fe_sat_data_cbg_weekly$PRECIP_cuts_decile)

fe_sat_data_cbg_weekly <- fe_sat_data_cbg_weekly %>% mutate(T_RH_PRECIP_FEs_decile = interaction(T_cuts_decile, RH_cuts_decile,PRECIP_cuts_decile))

lost_by_decile <- fe_sat_data_cbg_weekly %>% group_by(T_RH_PRECIP_FEs_decile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

fe_sat_data_cbg_weekly_diff <- fe_sat_data_cbg_weekly %>% filter(year==2020)%>% 
    mutate(post = as.numeric(date_last > ca_emergency)) %>%
    filter(!(date_last > ca_emergency & date_first < ca_shelter))
fe_sat_data_cbg_weekly_2019 <- fe_sat_data_cbg_weekly %>% filter(year==2019) %>% 
    select(block_group, wk, county, tract, pct_away_cbg_19 = pct_away_cbg, 
           NO2_1e6_19 = NO2_1e6, T_cuts_19 = T_cuts, RH_cuts_19 = RH_cuts, PRECIP_cuts_19=PRECIP_cuts,
           T_RH_PRECIP_FEs_19 = T_RH_PRECIP_FEs,
           T_RH_PRECIP_FEs_2_19 = T_RH_PRECIP_FEs_2,
           T_RH_PRECIP_FEs_5_19 = T_RH_PRECIP_FEs_5,
           T_RH_PRECIP_FEs_10_19 = T_RH_PRECIP_FEs_10,
           T_RH_PRECIP_FEs_20_19 = T_RH_PRECIP_FEs_20,
           T_RH_PRECIP_FEs_decile_19 = T_RH_PRECIP_FEs_decile,
           T_RH_PRECIP_FEs_vigintile_19 = T_RH_PRECIP_FEs_vigintile,
           TEMP_19 = TEMP,
           PRECIP_19 = PRECIP,
           RH_19 =RH)

fe_sat_data_cbg_weekly_diff <- fe_sat_data_cbg_weekly_diff %>%  left_join(fe_sat_data_cbg_weekly_2019) %>%
    mutate(NO2_diff = NO2_1e6 - NO2_1e6_19, pct_away_diff = pct_away_cbg  - pct_away_cbg_19) %>%
    drop_na(NO2_diff, pct_away_diff, share_hisp, share_asian, share_black,ln_inc_cbg, ln_road_dens, TEMP, RH, PRECIP)
touse <- c( "post:share_hisp", "post:share_asian", "post:share_black", "post:ln_inc_cbg","post:ln_road_dens","post:ln_pop_dens","pct_away_diff")

none <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                      post:share_asian + 
                                                      post:share_black  + 
                                                      post:ln_inc_cbg + 
                                                      post:ln_road_dens + 
                                                      post:ln_pop_dens + 
                                                      pct_away_diff| 
                                                      block_group + wk  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "none", order=5L)

poly3 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff + 
                                                       polym(TEMP, RH, PRECIP,  degree = 3) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 3) | 
                                                       block_group + wk  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly3", order=5L)


poly2 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff + 
                                                       polym(TEMP, RH, PRECIP,  degree = 2) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 2) | 
                                                       block_group + wk  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly2", order=5L)

poly1 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff + 
                                                       polym(TEMP, RH, PRECIP,  degree = 1) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 1) | 
                                                       block_group + wk  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly1", order=5L)

uninteracted <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                              post:share_asian + 
                                                              post:share_black  + 
                                                              post:ln_inc_cbg + 
                                                              post:ln_road_dens + 
                                                              post:ln_pop_dens + 
                                                              pct_away_diff| 
                                                              block_group + wk + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19 + T_cuts + RH_cuts + PRECIP_cuts | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "uninteracted", order=5L)

by_1 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                      post:share_asian + 
                                                      post:share_black  + 
                                                      post:ln_inc_cbg + 
                                                      post:ln_road_dens + 
                                                      post:ln_pop_dens + 
                                                      pct_away_diff| 
                                                      block_group + wk + T_RH_PRECIP_FEs_19 + T_RH_PRECIP_FEs | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_1", order=5L)

by_2 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                      post:share_asian + 
                                                      post:share_black  + 
                                                      post:ln_inc_cbg + 
                                                      post:ln_road_dens + 
                                                      post:ln_pop_dens + 
                                                      pct_away_diff| 
                                                      block_group + wk + T_RH_PRECIP_FEs_2_19 + T_RH_PRECIP_FEs_2 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_2", order=5L)

by_5 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                      post:share_asian + 
                                                      post:share_black  + 
                                                      post:ln_inc_cbg + 
                                                      post:ln_road_dens + 
                                                      post:ln_pop_dens + 
                                                      pct_away_diff| 
                                                      block_group + wk + T_RH_PRECIP_FEs_5_19 + T_RH_PRECIP_FEs_5 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_5", order=5L)

by_10 <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff| 
                                                       block_group + wk + T_RH_PRECIP_FEs_10_19 + T_RH_PRECIP_FEs_10 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_10", order=5L)

by_decile <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                           post:share_asian + 
                                                           post:share_black  + 
                                                           post:ln_inc_cbg + 
                                                           post:ln_road_dens + 
                                                           post:ln_pop_dens + 
                                                           pct_away_diff| 
                                                           block_group + wk + T_RH_PRECIP_FEs_decile_19 + T_RH_PRECIP_FEs_decile | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_decile", order=5L)

by_vigintile <- tidy(fe_sat_data_cbg_weekly_diff %>% felm(data = ., NO2_diff ~ post:share_hisp + 
                                                              post:share_asian + 
                                                              post:share_black  + 
                                                              post:ln_inc_cbg + 
                                                              post:ln_road_dens + 
                                                              post:ln_pop_dens + 
                                                              pct_away_diff| 
                                                              block_group + wk + T_RH_PRECIP_FEs_vigintile_19 + T_RH_PRECIP_FEs_vigintile | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_vigintile", order=5L)

coefs<-rbind(none,uninteracted,poly3,poly2,poly1,by_1,by_2,by_5,by_10,by_decile,by_vigintile)

dwplot(coefs) %>% relabel_predictors(c("post:share_hisp"  = "% Hispanic/Latinx",                       
                                       "post:share_asian" = "% Asian", 
                                       "post:share_black" = "% Black",
                                       "post:ln_inc_cbg" = "Income (ln)",
                                       "post:ln_pop_dens" = "Population density (ln)",
                                       "post:ln_road_dens" = "Road density (ln)",
                                       "pct_away_diff" = "Mobility")) + theme_bw()
ggsave(paste0(wd_path,"figures/FigED5_A3.pdf"))

# ++++++++++++++++++++++++++++++++++
# Panel A, center  --------
# ++++++++++++++++++++++++++++++++++

fe_data_cbg_source$DOY[fe_data_cbg_source$year==2019] <- fe_data_cbg_source$DOY[fe_data_cbg_source$year==2019] -1

fe_data_cbg_source <- fe_data_cbg_source %>% drop_na(TEMP,RH, PRECIP)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts = cut(TEMP, breaks = seq(-20,40,1)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts = cut(RH, breaks = seq(0,100,1)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts = cut(PRECIP, breaks = seq(-1,200,1)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs = interaction(T_cuts, RH_cuts,PRECIP_cuts))
lost_by_1 <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_2 = cut(TEMP, breaks = seq(-20,40,2)))
summary(fe_data_cbg_source$T_cuts_2)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_2 = cut(RH, breaks = seq(0,100,2)))
summary(fe_data_cbg_source$RH_cuts_2)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_2 = cut(PRECIP, breaks = seq(-1,200,2)))
summary(fe_data_cbg_source$PRECIP_cuts_2)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_2 = interaction(T_cuts_2, RH_cuts_2,PRECIP_cuts_2))

lost_by_2 <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_2) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_5 = cut(TEMP, breaks = seq(-20,40,5)))
summary(fe_data_cbg_source$T_cuts_5)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_5 = cut(RH, breaks = seq(0,100,5)))
summary(fe_data_cbg_source$RH_cuts_5)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_5 = cut(PRECIP, breaks = seq(-1,200,5)))
summary(fe_data_cbg_source$PRECIP_cuts_5)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_5 = interaction(T_cuts_5, RH_cuts_5,PRECIP_cuts_5))

lost_by_5 <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_5) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)


fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_10 = cut(TEMP, breaks = seq(-20,40,10)))
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_10 = cut(RH, breaks = seq(0,100,10)))
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_10 = cut(PRECIP, breaks = seq(-1,200,10)))

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_10 = interaction(T_cuts_10, RH_cuts_10,PRECIP_cuts_10))

lost_by_10 <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_10) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_20 = cut(TEMP, breaks = seq(-20,40,20)))
summary(fe_data_cbg_source$T_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_20 = cut(RH, breaks = seq(0,100,20)))
summary(fe_data_cbg_source$RH_cuts)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_20 = cut(PRECIP, breaks = seq(-1,200,20)))
summary(fe_data_cbg_source$PRECIP_cuts)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_20 = interaction(T_cuts_20, RH_cuts_20,PRECIP_cuts_20))

lost_by_20 <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_20) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_vigintile = cut(TEMP, breaks = quantile(fe_data_cbg_source$TEMP,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_data_cbg_source$T_cuts_vigintile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_vigintile = cut(RH, breaks = quantile(fe_data_cbg_source$RH,probs = seq(0,1,0.05),na.rm=T)))
summary(fe_data_cbg_source$RH_cuts_vigintile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_vigintile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_data_cbg_source$PRECIP,probs = seq(0,1,0.05),na.rm=T)))))
summary(fe_data_cbg_source$PRECIP_cuts_vigintile)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_vigintile = interaction(T_cuts_vigintile, RH_cuts_vigintile,PRECIP_cuts_vigintile))

lost_by_vigintile <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_vigintile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)



fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_cuts_decile = cut(TEMP, breaks = quantile(fe_data_cbg_source$TEMP,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_data_cbg_source$T_cuts_decile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(RH_cuts_decile = cut(RH, breaks = quantile(fe_data_cbg_source$RH,probs = seq(0,1,0.1),na.rm=T)))
summary(fe_data_cbg_source$RH_cuts_decile)
fe_data_cbg_source <- fe_data_cbg_source %>% mutate(PRECIP_cuts_decile = cut(PRECIP, breaks = c(-0.01,unique(quantile(fe_data_cbg_source$PRECIP,probs = seq(0,1,0.1),na.rm=T)))))
summary(fe_data_cbg_source$PRECIP_cuts_decile)

fe_data_cbg_source <- fe_data_cbg_source %>% mutate(T_RH_PRECIP_FEs_decile = interaction(T_cuts_decile, RH_cuts_decile,PRECIP_cuts_decile))

lost_by_decile <- fe_data_cbg_source %>% group_by(T_RH_PRECIP_FEs_decile) %>% dplyr::summarise(n = n()) %>% group_by(n) %>% tally(name = "obs_in_cell") %>% filter(n == 1) %>% select(obs_in_cell)

fe_data_cbg_source_diff <- fe_data_cbg_source %>% filter(year==2020) %>%
    drop_na(sample_mean, pct_away_cbg, share_hisp, share_asian, share_black,ln_inc_cbg, ln_road_dens, TEMP, RH, PRECIP)
fe_data_cbg_source_2019 <- fe_data_cbg_source %>% filter(year==2019) %>% 
    select(block_group, DOY, county, tract, pct_away_cbg_19 = pct_away_cbg, 
           sample_mean_19 = sample_mean, T_cuts_19 = T_cuts, RH_cuts_19 = RH_cuts, PRECIP_cuts_19=PRECIP_cuts,
           T_RH_PRECIP_FEs_19 = T_RH_PRECIP_FEs,
           T_RH_PRECIP_FEs_2_19 = T_RH_PRECIP_FEs_2,
           T_RH_PRECIP_FEs_5_19 = T_RH_PRECIP_FEs_5,
           T_RH_PRECIP_FEs_10_19 = T_RH_PRECIP_FEs_10,
           T_RH_PRECIP_FEs_20_19 = T_RH_PRECIP_FEs_20,
           T_RH_PRECIP_FEs_decile_19 = T_RH_PRECIP_FEs_decile,
           T_RH_PRECIP_FEs_vigintile_19 = T_RH_PRECIP_FEs_vigintile,
           TEMP_19 = TEMP,
           PRECIP_19 = PRECIP,
           RH_19 =RH) %>% 
    drop_na(sample_mean_19, pct_away_cbg_19, TEMP_19, RH_19, PRECIP_19)

fe_data_cbg_source_diff <- fe_data_cbg_source_diff %>%  left_join(fe_data_cbg_source_2019,.) %>%
    mutate(sample_mean_diff = sample_mean - sample_mean_19, pct_away_diff = pct_away_cbg  - pct_away_cbg_19) %>%
    drop_na(sample_mean_diff, pct_away_diff, share_hisp, share_asian, share_black,ln_inc_cbg, ln_road_dens, TEMP, RH, PRECIP)
load(paste0(wd_path,"/data/weights.Rdata"))
fe_data_cbg_source_diff<- fe_data_cbg_source_diff %>%  left_join(sample %>% dplyr::select(block_group, tract, county, state, post_weights)) %>%
    drop_na()

# Run models

touse <- c( "post:share_hisp", "post:share_asian", "post:share_black", "post:ln_inc_cbg","post:ln_road_dens","post:ln_pop_dens","pct_away_diff")

none <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date  | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "none", order=5L)

poly3 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 3) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 3) | 
                                                   block_group+ date  | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly3", order=5L)


poly2 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 2) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 2) | 
                                                   block_group+ date  | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly2", order=5L)

poly1 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 1) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 1) | 
                                                   block_group+ date  | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly1", order=5L)

uninteracted <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                          post:share_asian + 
                                                          post:share_black  + 
                                                          post:ln_inc_cbg + 
                                                          post:ln_road_dens + 
                                                          post:ln_pop_dens + 
                                                          pct_away_diff| 
                                                          block_group+ date + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19 + T_cuts + RH_cuts + PRECIP_cuts | 0 | county, 
                                                      weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "uninteracted", order=5L)

by_1 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_19 + T_RH_PRECIP_FEs | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_1", order=5L)

by_2 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_2_19 + T_RH_PRECIP_FEs_2 | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_2", order=5L)

by_5 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_5_19 + T_RH_PRECIP_FEs_5 | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_5", order=5L)

by_10 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff| 
                                                   block_group+ date + T_RH_PRECIP_FEs_10_19 + T_RH_PRECIP_FEs_10 | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_10", order=5L)

by_decile <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff| 
                                                       block_group+ date + T_RH_PRECIP_FEs_decile_19 + T_RH_PRECIP_FEs_decile | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_decile", order=5L)

by_vigintile <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                          post:share_asian + 
                                                          post:share_black  + 
                                                          post:ln_inc_cbg + 
                                                          post:ln_road_dens + 
                                                          post:ln_pop_dens + 
                                                          pct_away_diff| 
                                                          block_group+ date + T_RH_PRECIP_FEs_vigintile_19 + T_RH_PRECIP_FEs_vigintile | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by_vigintile", order=5L)

paper_reg<- fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                 post:share_asian + 
                                                 post:share_black  + 
                                                 post:ln_inc_cbg + 
                                                 post:ln_road_dens + 
                                                 post:ln_pop_dens + 
                                                 pct_away_diff| 
                                                 block_group+ date + T_RH_PRECIP_FEs_vigintile_19 + T_RH_PRECIP_FEs_vigintile | 0 | county, weights = fe_data_cbg_source_diff$post_weights)

stargazer(paper_reg, type="text")

coefs<-rbind(none,uninteracted,poly3,poly2,poly1,by_1,by_2,by_5,by_10,by_decile,by_vigintile)

# Plot 
library(dotwhisker)
dwplot(coefs) %>% relabel_predictors(c("post:share_hisp"  = "% Hispanic/Latinx",                       
                                       "post:share_asian" = "% Asian", 
                                       "post:share_black" = "% Black",
                                       "post:ln_inc_cbg" = "Income (ln)",
                                       "post:ln_pop_dens" = "Population density (ln)",
                                       "post:ln_road_dens" = "Road density (ln)",
                                       "pct_away_diff" = "Mobility")) + theme_bw()

ggsave(paste0(wd_path,"figures/FigED5_A2.pdf"))

# ++++++++++++++++++++++++++++++++++
# Panel A, right  --------
# ++++++++++++++++++++++++++++++++++

touse <- c( "post:share_hisp", "post:share_asian", "post:share_black", "post:ln_inc_cbg","post:ln_road_dens","post:ln_pop_dens","pct_away_diff")

none <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "none", order=5L)

poly3 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 3) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 3) | 
                                                   block_group+ date  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly 3", order=5L)



poly2 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 2) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 2) | 
                                                   block_group+ date  | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly 2", order=5L)

poly1 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff + 
                                                   polym(TEMP, RH, PRECIP,  degree = 1) + polym(TEMP_19, RH_19, PRECIP_19,  degree = 1) | 
                                                   block_group+ date  | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "Poly1", order=5L)

uninteracted <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                          post:share_asian + 
                                                          post:share_black  + 
                                                          post:ln_inc_cbg + 
                                                          post:ln_road_dens + 
                                                          post:ln_pop_dens + 
                                                          pct_away_diff| 
                                                          block_group+ date + T_cuts_19 + RH_cuts_19 + PRECIP_cuts_19 + T_cuts + RH_cuts + PRECIP_cuts | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "uninteracted", order=5L)

by_1 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_19 + T_RH_PRECIP_FEs | 0 | county, weights = fe_data_cbg_source_diff$post_weights)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by 1", order=5L)

by_2 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_2_19 + T_RH_PRECIP_FEs_2 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by 2", order=5L)

by_5 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                  post:share_asian + 
                                                  post:share_black  + 
                                                  post:ln_inc_cbg + 
                                                  post:ln_road_dens + 
                                                  post:ln_pop_dens + 
                                                  pct_away_diff| 
                                                  block_group+ date + T_RH_PRECIP_FEs_5_19 + T_RH_PRECIP_FEs_5 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by 5", order=5L)

by_10 <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                   post:share_asian + 
                                                   post:share_black  + 
                                                   post:ln_inc_cbg + 
                                                   post:ln_road_dens + 
                                                   post:ln_pop_dens + 
                                                   pct_away_diff| 
                                                   block_group+ date + T_RH_PRECIP_FEs_10_19 + T_RH_PRECIP_FEs_10 | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by 10", order=5L)

by_decile <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                       post:share_asian + 
                                                       post:share_black  + 
                                                       post:ln_inc_cbg + 
                                                       post:ln_road_dens + 
                                                       post:ln_pop_dens + 
                                                       pct_away_diff| 
                                                       block_group+ date + T_RH_PRECIP_FEs_decile_19 + T_RH_PRECIP_FEs_decile | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by decile", order=5L)

by_vigintile <- tidy(fe_data_cbg_source_diff %>% felm(data = ., sample_mean_diff ~ post:share_hisp + 
                                                          post:share_asian + 
                                                          post:share_black  + 
                                                          post:ln_inc_cbg + 
                                                          post:ln_road_dens + 
                                                          post:ln_pop_dens + 
                                                          pct_away_diff| 
                                                          block_group+ date + T_RH_PRECIP_FEs_vigintile_19 + T_RH_PRECIP_FEs_vigintile | 0 | county)) %>%
    filter(term %in% touse) %>% 
    mutate(model = "by vigintile", order=5L)

coefs<-rbind(none,uninteracted,poly3,poly2,poly1,by_1,by_2,by_5,by_10,by_decile,by_vigintile)

dwplot(coefs) %>% relabel_predictors(c("post:share_hisp"  = "% Hispanic/Latinx",                       
                                       "post:share_asian" = "% Asian", 
                                       "post:share_black" = "% Black",
                                       "post:ln_inc_cbg" = "Income (ln)",
                                       "post:ln_pop_dens" = "Population density (ln)",
                                       "post:ln_road_dens" = "Road density (ln)",
                                       "pct_away_diff" = "Mobility")) + theme_bw()

ggsave(paste0(wd_path,"figures/FigED5_A1.pdf"))
