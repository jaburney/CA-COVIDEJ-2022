# ABOUT: This script: (i) loads and formats US Census metadata at the census block group (CBG) level; (ii) summaries origin and race CBG metadata in exported figures; (iii) generates CBG road density summaries and appends those data to the formatted CBG data; (iv) exports formatted CBG data (.csv, .Rdata) and associated CBG spatial features (.shp).

# setup -----------------------------------------------------

library(tidyverse)
library(sf)

# NOTE: Paths in this script rely on the "here" package (if not installed, run: install.packages("here")). This code assumes that here::here() returns "YOUR_UNIQUE_PATH/covid-pollution-master", which contains a "data" folder from/to which this script reads/writes data, and a "figures" folder where figures are saved.

# census -----------------------------------------------------

library(tidycensus) # see https://walker-data.com/tidycensus/articles/basic-usage.html

# census_api_key("YOUR API KEY GOES HERE")

# Download census block group data
ca <- get_acs(geography = "block group", 
               variables = c(medincome = "B19013_001",
                             race_total = "B02001_001", 
                             race_white = "B02008_001",
                             race_black = "B02009_001",
                             race_asian = "B02011_001",
                             hisp_total = "B03002_001",
                             hisp_yes = "B03002_012",
                             hisp_yes_black ="B03002_014",
                             hisp_yes_white ="B03002_013",
                             hisp_yes_asian = "B03002_016",
                             hisp_no_black = "B03002_004",
                             hisp_no_white = "B03002_003",
                             hisp_no_asian = "B03002_006"),
               state = "CA",
               year = 2018,
               geometry=TRUE) %>%
  st_transform(crs=st_crs(4326)) %>%
  select(GEOID,NAME,estimate,variable) %>% 
  rename(block_group = GEOID, name = NAME) %>%
  add_column(unit = "block_group")

# census block area
ca$area_block_group <- st_area(ca)

# isolate individual geometries
cau <- subset(ca,variable == "race_total")

# reformat non-spatial data
scp2 <- ca
st_geometry(scp2) <- NULL # remove spatial features
scp2 <- scp2 %>% pivot_wider(names_from=c(variable,unit),values_from=estimate)
#all(scp2$block_group == cau$block_group)

# re-apply geometries to wide format
st_geometry(scp2) <- st_geometry(cau)
#all(st_geometry(scp2) == st_geometry(cau), na.rm=T)

# remove entries (16) with no geometry + no metadata
scp <- scp2[-which(scp2$block_group %in% scp2[st_dimension(scp2) %>% is.na %>% which,]$block_group),]
#st_dimension(scp) %>% is.na() %>% any()

# clear
rm(scp2,cau)

# isolate county and state
scp <- scp %>% separate(name, c(NA,"tract","county","state"),", ")

# origin and race plots -----------------------------------------------------

## boxplot: percent of total hisp/non-hisp

tothisp <- ca %>%
  select(block_group,estimate,variable) %>%
  filter(!variable %in% c("medincome")) %>%
  as.data.frame() %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(
    "Hispanic and White" = hisp_yes_white/hisp_yes,
    "Hispanic and Black" = hisp_yes_black/hisp_yes,
    "Hispanic and Asian" = hisp_yes_asian/hisp_yes,
    "Non-Hispanic and White" = hisp_no_white/(hisp_total-hisp_yes),
    "Non-Hispanic and Black" = hisp_no_black/(hisp_total-hisp_yes),
    "Non-Hispanic and Asian" = hisp_no_asian/(hisp_total-hisp_yes)
  ) %>%
  select(-(geometry:hisp_no_asian)) %>%
  pivot_longer(-block_group,names_to="Origin and Race",values_to="Fraction") %>%
  mutate(
    "Percent" = Fraction * 100
  )

tothisp$`Origin and Race` <- factor(tothisp$`Origin and Race`, levels = c("Hispanic and White","Hispanic and Black","Hispanic and Asian","Non-Hispanic and White","Non-Hispanic and Black","Non-Hispanic and Asian"))

pdf(here::here("figures","FigS1_right.pdf"),width=5,height=4)
ggplot(tothisp, aes(x=`Origin and Race`, y=Percent)) +
  geom_boxplot(outlier.shape = NA)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.8)) +
  ylab("Percent (of Hispanic or Non-Hispanic)")+
  xlab(NULL)
dev.off()

## boxplot: percent of total pop

totpop <- ca %>%
  select(block_group,estimate,variable) %>%
  filter(variable %in% c("hisp_yes","hisp_total","race_white","race_black","race_asian","race_total")) %>%
  as.data.frame() %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(
    "White" = race_white/race_total,
    "Black" = race_black/race_total,
    "Asian" = race_asian/race_total,
    "Hispanic" = hisp_yes/hisp_total
  ) %>%
  select(-(geometry:hisp_yes)) %>%
  pivot_longer(-block_group,names_to="Origin and Race",values_to="Fraction") %>%
  mutate(
    "Percent" = Fraction * 100
  )

totpop$`Origin and Race` <- factor(totpop$`Origin and Race`, levels = c("White","Black","Asian","Hispanic"))

pdf(here::here("figures","FigS1_left.pdf"),width=3,height=4)
ggplot(totpop, aes(x=`Origin and Race`, y=Percent)) +
  geom_boxplot(outlier.shape = NA)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.8)) +
  ylab("Percent (of Total Population)")+
  xlab(" \n \n \n")
dev.off()

# ## summary statistics
# 
# tothisp %>% group_by(`Origin and Race`) %>% summarise(mean = mean(Percent,na.rm=T)) %>% as.data.frame()
# 
# totpop %>% group_by(`Origin and Race`) %>% 
#   summarise(mean = mean(Percent,na.rm=T),
#             min = mean(Percent,na.rm=T),
#             max = max(Percent,na.rm=T)) %>% 
#   as.data.frame()
# 
# totpop %>% group_by(`Origin and Race`) %>% 
#   summarise(p10 = quantile(Percent,probs = 0.1, na.rm=T),
#             p50 = quantile(Percent,probs = 0.5, na.rm=T),
#             p90 = quantile(Percent,probs = 0.9, na.rm=T))
# 
# totpop %>% group_by(`Origin and Race`) %>%
#   summarise(n90 = sum(Percent >= 50,na.rm=T))
# 
# totpop %>% group_by(`Origin and Race`) %>%
#   summarise(n100 = sum(Percent == 100,na.rm=T))

# road density from GRIP vector data -----------------------------------------------------

# Source: Meijer et al 2018 Environ. Res. Lett. https://doi.org/10.1088/1748-9326/aabd42. Download at https://www.globio.info/download-grip-dataset. 

# Instructions: Navigate to above link, scroll down to "Download GRIP4 vector datasets" table, and select "Download shape" for "Region 1: North America" (download link: https://dataportaal.pbl.nl/downloads/GRIP4/GRIP4_Region1_vector_shp.zip). Save unzipped "GRIP4/GRIP4_Region1_vector_shp" files in "data" folder (~ 2GB).

grip <- st_read(here::here("data","GRIP4_Region1_vector_shp"),"GRIP4_region1") %>% st_crop(.,scp) # crop to area of interest

# project
scpp <- st_transform(scp, crs = st_crs(3499))
gripp <- st_transform(grip, crs = st_crs(3499))

# function: clip roads to polygon (CBGs), calculate road lengths, sum lengths
blen <- function(l,p){
  l %>% 
    filter(st_contains(p, ., sparse = FALSE)) %>%
    st_length() %>%
    sum(.,na.rm=TRUE)
}

# total road length in CBGs
scppi <- NA
for (i in 1:dim(scpp)[1]){
  cat(i,"\n")
  scppi[i] <- blen(gripp,scpp[i,])
}

# density [m/km2]
scppi_dens <- scppi * 1e6 / scpp$area_block_group  %>% as.numeric()

# merge
scp <- cbind(scp,scppi_dens) %>% rename(grip4_block_group_dens_m_km2 = scppi_dens)

#rm(scpp,scppi)

# write/save -----------------------------------------------------

st_write(scp, here::here("data","CA_blockgroup_metadata_6.29.20.csv"))
save(scp,file=here::here("data","CA_blockgroup_metadata_6.29.20.Rdata"))

dir.create(here::here("data","CA_blockgroup_metadata_shp"))
st_write(scp %>% select(block_group,tract,county,state), here::here("data","CA_blockgroup_metadata_shp","CA_blockgroup_metadata_6.29.20.shp"))

