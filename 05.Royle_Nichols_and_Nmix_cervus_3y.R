#N-mixtures and Royle-Nichols models Doñana

# #packages
# library(tidyverse)
# library(lubridate)
# library(dplyr)
# library(tidyr)
# library(unmarked)
# library(broom)
# 
# observations <- read_csv("New data - 2021.09 on CopyGDM/observations.csv")
# deployments <- read_csv("New data - 2021.09 on CopyGDM/deployments_correctedSimone_2026.03.csv")
# 
# # Ensure Date column is in Date format
# deployments$deploymentStart <- as.Date(deployments$deploymentStart)
# class(deployments$deploymentStart)
# deployments$deploymentEnd <- as.Date(deployments$deploymentEnd)
# class(deployments$deploymentEnd)
# 
# #filtering for only 2022 to 204
# #deploymentsf <- deployments %>% filter(deploymentStart>"2021-12-31")
# observationsf <- observations %>% filter(eventStart>as.Date("2022-01-01")) %>% 
#   filter(eventStart<as.Date("2025-01-01")) %>% 
#   filter(observationType=="animal")
# 
# #exploring data
# 
# observationsf %>% group_by(scientificName) %>% summarise(n())
# observationsf$locationID <- as.factor(observationsf$locationID)
# observations.sp.y<- observationsf %>% mutate(Year=substr(timestamp,1,4)) %>% 
#   group_by(scientificName, Year) %>% summarise(n())
# 
# observations.sp.y.st<- observationsf %>% mutate(Year=substr(timestamp,1,4)) %>% 
#   group_by(scientificName, Year, locationID) %>% summarise(n()) %>% 
#   group_by(scientificName, Year) %>% summarise(n())
# 
#only selected species
data.cervus.occu <-  filter(observationsf, scientificName =="Cervus elaphus") %>% 
  left_join(dplyr::select(deployments,deploymentID,locationID))
# 
# 
# # Define the start and end date for the survey period
# start_date <- "2021-12-23" %>% as.Date("%Y-%m-%d")
# end_date <- "2024-12-01" %>% as.Date( "%Y-%m-%d") #
# # I am using the previous dates because they are more compatible with 
# # dividing the year into seasons which I`ll do next
# 
# # Create weekly intervals
# survey_occasions <- seq.Date(start_date, end_date, by = "6 days")

# Add a column for survey occasion
data.cervus.occu <- data.cervus.occu %>%
  mutate(Occasion = findInterval(as.Date(eventStart), survey_occasions))

# 
# # Get unique sites 
# all.sites <- as.factor(unique(observationsf$locationID))
# # all.sites <- as.factor(unique(data.cervus.occu$locationID))
# setdiff(all.sites2,all.sites)

# Create an empty matrix for detection/non-detection data
detection_data.cervus.occu <- matrix(0, nrow = length(all.sites), ncol = length(survey_occasions))
rownames(detection_data.cervus.occu) <- all.sites

#records per site per occasion
data.cervus.occu1 <- data.cervus.occu %>% mutate(day=substr(eventStart,1,7))%>%
  group_by(locationID,scientificName,Occasion,day) %>% summarize(max_animalsday=max(count))

data.cervus.occu2 <- data.cervus.occu1 %>% group_by(locationID, scientificName, Occasion)%>% 
  summarise(ind_records=sum(max_animalsday))

#max counts per occasion only ( the minimum count of individuals seen per occasion)
data.cervus.occu2.2 <- data.cervus.occu1 %>% group_by(locationID, scientificName, Occasion)%>% 
  summarise(min_n_idiv=max(max_animalsday))


# Fill the matrix with detections
for (i in 1:nrow(data.cervus.occu2)) {
  site <- data.cervus.occu2$locationID[i]
  occasion <- data.cervus.occu2$Occasion[i]
  ind_records <- filter(data.cervus.occu2, locationID==site & Occasion==occasion)$ind_records
  detection_data.cervus.occu[as.character(site), occasion] <- ind_records
  print(i)
}

# Convert matrix to data frame
detection_data.cervus.occu <- as.data.frame(detection_data.cervus.occu)

#make sure the order of occasion remain the same (alphabetIcal)
# important for when joining with site covariable
#detection_data.cervus.occu <-detection_data.cervus.occu[order(rownames(detection_data.cervus.occu)), ]

## adding start and end occasions for each tree/camera so the rest is filled with NAs
deployments <- deployments %>% mutate(Occasionstart = findInterval(deploymentStart, survey_occasions))
deployments <- deployments %>% mutate(Occasionend = findInterval(deploymentEnd, survey_occasions))

#max and minimal occasion per camera location
# location_timerang <- deployments %>% group_by(locationID) %>% 
#   summarise(minlocOccasion=min(Occasionstart),maxlocOccasion=max(Occasionend)) %>% 
#   filter(minlocOccasion!=0&maxlocOccasion!=0)
detection_data.cervus.occu2 <- detection_data.cervus.occu

#Start
for (i in 1:length(location_timerang$locationID)) {
  ifelse(location_timerang$minlocOccasion[i]==min(location_timerang$minlocOccasion),"no NAs - camera set on first occasion",
         detection_data.cervus.occu2[location_timerang$locationID[i],1:(location_timerang$minlocOccasion[i])]  <- NA)
}

#End
for (i in 1:length(location_timerang$locationID)) {
  ifelse(location_timerang$maxlocOccasion[i]==max(location_timerang$maxlocOccasion),"no NAs - camera set until last occasion",
         detection_data.cervus.occu2[location_timerang$locationID[i],(location_timerang$maxlocOccasion[i]):max(location_timerang$maxlocOccasion)]  <- NA)
}
#this is working but we don`t ahve NAs becuase we are starting and end after the first deloyment start 
#and ending before the last deploymetn ends

# #removing problematic days
# #loading operation tables
# oper_table <- read_csv("New data - 2021.09 on CopyGDM/locationInoperabilityDonana.csv")
# 
# camop_problem_lubridate <- camtrapR::cameraOperation(CTtable      = oper_table,
#                                                      stationCol   = "station",
#                                                      setupCol     = "Setup_date",
#                                                      retrievalCol = "Retrieval_date",
#                                                      writecsv     = FALSE,
#                                                      hasProblems  = TRUE,
#                                                      dateFormat   = "%d/%m/%Y"
# )
# 
# cam_op_table <- as_tibble(t(camop_problem_lubridate),rownames=NA)
# #adding occasion column
# cam_op_table <- cam_op_table %>% mutate(date=row.names(cam_op_table))
# #adding one more survey occasion after my end date so everything after fall into that bin
# cam_op_table <- cam_op_table %>%
#   mutate(Occasion = findInterval(as.Date(date), c(survey_occasions, end_date+1)))
# 
# #how many days was each camera active in each occasion?
# cam_op_tablecam <- cam_op_table %>%
#   dplyr::select(-date) %>%
#   group_by(Occasion) %>%
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
# #Occasion 0 and 181 were created because there are observations outside of our period of interest,
# # that is ok we will filter those out later
# 
# #was the camera active during that ocassion. I`ll consider if the camera was active in 5 ouf of 6 days
# #as a yes/active
# cam_op_tablecamfinal <- as_tibble(ifelse(cam_op_tablecam %>% dplyr::select(-Occasion)>=5,1,0)) %>%
#   mutate(Occasion=cam_op_tablecam$Occasion) %>%
#   dplyr::select(Occasion,everything())
# 
# #adding NAs back to the matrix

#first filtering only the stations that are in the cervus data.frame
cam_op_tablecamfinal2 <- cam_op_tablecamfinal %>% 
  dplyr::select(Occasion, row.names(detection_data.cervus.occu2)) %>% 
  filter(Occasion!=0,Occasion!=181) %>% #removing dates that fall out of the survey period
  dplyr::select(-Occasion)

#reansforming 0s to NAs, dayswere cameras were not active
cam_op_tablecamfinal2[cam_op_tablecamfinal2==0] <-NA

#checking sizes for matrix multiplication
#should be the same
length(as.matrix(cam_op_tablecamfinal2))==length(as.matrix(detection_data.cervus.occu2))

activeoccasions<- t(as.matrix(cam_op_tablecamfinal2))
observationsocassion_cervus <- as.matrix(detection_data.cervus.occu2)

#multiplying the matrices 
detection_data.cervus.occu3 <- as_tibble(activeoccasions * observationsocassion_cervus) 
row.names(detection_data.cervus.occu3) <- row.names(detection_data.cervus.occu2)

#exactly 180 occassion
#each year will have 60 occasion, with 4 season of 15 ocassions (90 days)
#detection_data.cervus.occu4 <- detection_data.cervus.occu3[,-XXX]
detection_data.cervus.occu4 <- detection_data.cervus.occu3

row.names(detection_data.cervus.occu4) <- row.names(detection_data.cervus.occu2)


# SPLITTING AND STACKING
# using 3 years together

#divinding into 5 periods of 6 weeks

# Split and stack
df_list_cervus <- split.default(detection_data.cervus.occu4, rep(1:36, each = 5))
for (i in 1:length(df_list_cervus)) {
  names(df_list_cervus[[i]]) <- c("V1", "V2", "V3", "V4", "V5")
  row.names(df_list_cervus[[i]]) <- paste0(rownames(detection_data.cervus.occu4),".", i)
}
df_stacked_cervus3y <-  do.call(rbind, df_list_cervus)


# df_stacked_cervus <- do.call(rbind, df_list_cervus)
# df_stacked_cervus22 <- df_stacked_cervus[1:(length(all.sites)*13),]
# row.names(df_stacked_cervus22)<- rownames(df_stacked_cervus)[1:(length(all.sites)*13)]
# 
# df_stacked_cervus23 <- df_stacked_cervus[((length(all.sites)*13)+1):(length(all.sites)*26),]
# row.names(df_stacked_cervus23)<- rownames(df_stacked_cervus)[((length(all.sites)*13)+1):(length(all.sites)*26)]
# 
# df_stacked_cervus24 <- df_stacked_cervus[((length(all.sites)*26)+1):length(rownames(df_stacked_cervus)),]
# row.names(df_stacked_cervus24)<- rownames(df_stacked_cervus)[((length(all.sites)*26)+1):length(rownames(df_stacked_cervus))]
# 

#Site covariates
# 
# site_covs <- read_csv("New data - 2021.09 on CopyGDM/Donana_env_var80.13.bilinearex.csv")
# 
# site_covs <- site_covs %>% left_join(deployments %>% dplyr::select(deploymentID,locationID)) %>% 
#   dplyr::select(-deploymentID) %>% distinct() %>% 
#   filter(locationID %in% row.names(detection_data.cervus.occu4))
# 
# site_covssel <- site_covs %>% 
#   dplyr::select(locationID,
#                 Local_veg_contrast_10m,
#                 Het_Dis,
#                 NDVI_longterm,
#                 NDWI_longterm,
#                 Canopy_height,
#                 Biomass_above,
#                 Veggie_complex,
#                 # LST_21_med,
#                 # LST_21_max,
#                 #`Annual mean temperature`,
#                 #Isothermality,
#                 #`Annual precipitation`,
#                 #ER_annualPET,
#                 #ER_PETWettestQuarter,
#                 trees,
#                 grassland,
#                 shrubs,
#                 cropland,
#                 built,
#                 water,
#                 wetland,
#                 elevation_regional,
#                 slope,
#                 Terrain_rougnhess1,
#                 human_footprint,
#                 pop_density_1kmradius,
#                 dist_fences_m,
#                 dist_lagoon_m,                 
#                 dist_marsh_m,
#                 dist_roads_m,
#                 BSF=BSF_longterm,
#                 water_ava22=`Water_R=1km_mean_2022_23`,
#                 water_ava23=`Water_R=1km_mean_2023_24`,
#                 water_ava24=`Water_R=1km_mean_2024_25`,
#                 albedo=albedo_2022
#   )
# 
# n_rep3y <- 36
# # number of occasion per year
# 
# site_covssel_rep3y <- do.call(
#   rbind,
#   lapply(seq_len(n_rep3y), function(i) {
#     df <- site_covssel
#     rownames(df) <- paste0(rownames(site_covssel), ".", i)
#     df
#   })
# )
# 
# ## adding season of the year as covariable so detection also varies by season
# # I divided the year into 12 priods of 5 occasions (30 days)
# seasons_year3y<- rep(c("Winter","Winter","Winter",
#                        "Spring","Spring", "Spring",
#                        "Summer","Summer","Summer",
#                        "Autumn","Autumn","Autumn"),3)
# 
# allstations_seasons3y <- vector()
# for (i in 1:length(seasons_year3y)) {
#   allstations_seasons3y<- c(allstations_seasons3y, rep(seasons_year3y[i], length(rownames(site_covssel))))
#   
# }
# site_covssel_rep3y$season <- allstations_seasons3y
# 
# 
# years3y<- c(rep("2022", (length(site_covssel_rep3y$season)/3)),
#             rep("2023", (length(site_covssel_rep3y$season)/3)),
#             rep("2024", (length(site_covssel_rep3y$season)/3)))
# 
# site_covssel_rep3y$year <- years3y
# 
# 


## 3year together------
#Royle Nichols model

umf.cervus.RN3y<- unmarkedFrameOccu(
  y = df_stacked_cervus3y,
  siteCovs = site_covssel_rep3y,
  
)

modelcervusRN3y_nm <- occuRN(~ 1
                          ~ 1, 
                          data = umf.cervus.RN3y,
                          K = max(rowSums(na.omit(df_stacked_cervus3y))))

summary(modelcervusRN3y_nm)

# Get empirical Bayes estimates of abundance for each site
re_nm_cervus_3y <- ranef(modelcervusRN3y_nm)

# checking the probabilities of the first station on the first season (4weeks)
re_nm_cervus_3y@post[1,,1] # this is the estimated abundances and their specific probabilities

#is the actual abundance
# Getting the mean
abundance_meanRN_nm_cervus_3y <- bup(re_nm_cervus_3y, stat = "mean")

#mean per station per year
abundance_meanRN_nm_cervus_3y.df<- tibble(abundance_meanRN_nm_cervus_3y, locationID=rep(all.sites,36), year=years3y) %>% 
  group_by(locationID, year) %>%  summarise(abundance_meanRN3_nm_cervus= mean(na.omit(abundance_meanRN_nm_cervus_3y)))

#separating each year and creating data frame
abundance_meanRN_nm_cervus_3y22.df<- abundance_meanRN_nm_cervus_3y.df %>% 
  filter(year==2022) %>% dplyr::select(-year)
abundance_meanRN_nm_cervus_3y23.df<- abundance_meanRN_nm_cervus_3y.df %>% 
  filter(year==2023) %>% dplyr::select(-year)
abundance_meanRN_nm_cervus_3y24.df<- abundance_meanRN_nm_cervus_3y.df %>% 
  filter(year==2024) %>% dplyr::select(-year)

#model selection according to AIC

###
#detection
detectio_f_cervus <- ~ 
  Local_veg_contrast_10m+
  dist_fences_m+
  dist_roads_m+
  season +
  year

# abundance side
abundance_f_cervus <- ~ year + season

# your K (as you used it)
K_use_cervus_min <- max(#rowSums(
  na.omit(df_stacked_cervus3y))+1
#)

K_use_cervus_max <- max(rowSums(
  na.omit(df_stacked_cervus3y)))

#function to select best model according to AIC
#may take some time


sel_rn_aic_cervus3y <- backward_occuRN_AIC_rnAIC(
  det_formula   = detectio_f_cervus,
  state_formula = abundance_f_cervus,
  data = umf.cervus.RN3y,
  K = K_use_cervus_max, 
  component = "det",
  keep_det = c("season", "year"),
  threads = 8
)

summary(sel_rn_aic_cervus3y$best_model)

# Get empirical Bayes estimates of abundance for each site
re_sv_cervus_3y <- ranef(sel_rn_aic_cervus3y$best_model)


# checking the probabilities of the first station on the first season (4weeks)
re_sv_cervus_3y@post[1, ,] # this is the estimated abundances and the probability that each abundance


# Getting the mean
abundance_meanRN_sv_cervus_3y <- bup(re_sv_cervus_3y, stat = "mean")


#mean per station per year
abundance_meanRN_sv_cervus_3y.df<- tibble(abundance_meanRN_sv_cervus_3y, locationID=rep(all.sites,36), year=years3y) %>% 
  group_by(locationID, year) %>%  summarise(abundance_meanRN3_sv_cervus= mean(na.omit(abundance_meanRN_sv_cervus_3y)))

#separating each year and creating data frame
abundance_meanRN_sv_cervus_3y22.df<- abundance_meanRN_sv_cervus_3y.df %>% 
  filter(year==2022) %>% dplyr::select(-year)
abundance_meanRN_sv_cervus_3y23.df<- abundance_meanRN_sv_cervus_3y.df %>% 
  filter(year==2023) %>% dplyr::select(-year)
abundance_meanRN_sv_cervus_3y24.df<- abundance_meanRN_sv_cervus_3y.df %>% 
  filter(year==2024) %>% dplyr::select(-year)


#comparing to null models
cor(na.omit(abundance_meanRN_sv_cervus_3y22.df$abundance_meanRN3_sv_cervus),
    na.omit(abundance_meanRN_nm_cervus_3y22.df$abundance_meanRN3_nm_cervus))
cor(na.omit(abundance_meanRN_sv_cervus_3y23.df$abundance_meanRN3_sv_cervus),
    na.omit(abundance_meanRN_nm_cervus_3y23.df$abundance_meanRN3_nm_cervus))
cor(na.omit(abundance_meanRN_sv_cervus_3y24.df$abundance_meanRN3_sv_cervus),
    na.omit(abundance_meanRN_nm_cervus_3y24.df$abundance_meanRN3_nm_cervus))
#very correlated the model with sensible variables and the null model
#which is probably good

#comparing to RAI counts
RAI22 <-  read_csv("New data - 2021.09 on CopyGDM/RAI_data/species_data_mat_Do22effcCERvusDama.csv")
RAI22cervus <- RAI22 %>% dplyr::select(locationName, `Cervus elaphus`) 
abundance_meanRN_sv_cervus_3y22.df2 <- left_join(
  abundance_meanRN_sv_cervus_3y22.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI22cervus)
cor(na.omit(abundance_meanRN_sv_cervus_3y22.df2)$abundance_meanRN3_sv_cervus,
    na.omit(abundance_meanRN_sv_cervus_3y22.df2)$`Cervus elaphus`)

RAI23 <-  read_csv("New data - 2021.09 on CopyGDM/RAI_data/species_data_mat_Do23effcCervusDama.csv")
RAI23cervus <- RAI23 %>% dplyr::select(locationName, `Cervus elaphus`) 
abundance_meanRN_sv_cervus_3y23.df2 <- left_join(
  abundance_meanRN_sv_cervus_3y23.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI23cervus)
cor(na.omit(abundance_meanRN_sv_cervus_3y23.df2)$abundance_meanRN3_sv_cervus,
    na.omit(abundance_meanRN_sv_cervus_3y23.df2)$`Cervus elaphus`)

RAI24 <-  read_csv("New data - 2021.09 on CopyGDM/RAI_data/species_data_mat_Do24effcCervusDama.csv")
RAI24cervus <- RAI24 %>% dplyr::select(locationName, `Cervus elaphus`) 
abundance_meanRN_sv_cervus_3y24.df2 <- left_join(
  abundance_meanRN_sv_cervus_3y24.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI24cervus)
cor(na.omit(abundance_meanRN_sv_cervus_3y24.df2)$abundance_meanRN3_sv_cervus,
    na.omit(abundance_meanRN_sv_cervus_3y24.df2)$`Cervus elaphus`)

#correlation with RAI count effort between 0.51 to 0.61, this is a very abundant species
#but with high variablity of count per record, and the counts in the RAI are underestimated because
# cervidae is not being counted

####
#### N Mixture model ##

umf.cervus.NM3y<- unmarkedFramePCount(
  y = df_stacked_cervus3y,
  siteCovs = site_covssel_rep3y 
)

modelcervusNmix3y_nmP <- pcount(
  ~1 ~1,
  data = umf.cervus.NM3y,
  K = max(rowSums(na.omit(df_stacked_cervus3y))),
  mixture = "P"
)
summary(modelcervusNmix3y_nmP)

modelcervusNmix3y_nmNB <- pcount(
  ~1 ~1,
  data = umf.cervus.NM3y,
  K = max(rowSums(na.omit(df_stacked_cervus3y))),
  mixture = "NB"
)
summary(modelcervusNmix3y_nmNB)

modelcervusNmix3y_nmZIP <- pcount(
  ~1 ~1,
  data = umf.cervus.NM3y,
  K = max(rowSums(na.omit(df_stacked_cervus3y))),
  mixture = "ZIP"
)
summary(modelcervusNmix3y_nmZIP)


##NB is the best distribution

# posterior distributions

# Get posterior distributions for each site
re_NMix_nm_cervus_3y <- ranef(modelcervusNmix3y_nmNB)

# checking the probabilities of the first station on the first season (4weeks)
re_NMix_nm_cervus_3y@post[1, ,] # this is the estimated abundances and the probability that each abundance

# Getting the mean
abundance_meanNmix_nm_cervus_3y <- bup(re_NMix_nm_cervus_3y, stat = "mean")

#mean per station per year
abundance_meanNmix_nm_cervus_3y.df<- tibble(abundance_meanNmix_nm_cervus_3y, locationID=rep(all.sites,36), year=years3y) %>% 
  group_by(locationID, year) %>%  summarise(abundance_meanNmix3_nm_cervus= mean(na.omit(abundance_meanNmix_nm_cervus_3y)))

#separating each year and creating data frame
abundance_meanNmix_nm_cervus_3y22.df<- abundance_meanNmix_nm_cervus_3y.df %>% 
  filter(year==2022) %>% dplyr::select(-year)
abundance_meanNmix_nm_cervus_3y23.df<- abundance_meanNmix_nm_cervus_3y.df %>% 
  filter(year==2023) %>% dplyr::select(-year)
abundance_meanNmix_nm_cervus_3y24.df<- abundance_meanNmix_nm_cervus_3y.df %>% 
  filter(year==2024) %>% dplyr::select(-year)



## with sensible variables

###
#variable selection according to AIC

###
#detection
detection_f_cervus <- ~ 
  Local_veg_contrast_10m+
  dist_fences_m+
  dist_roads_m+
  season +
  year

# abundance side
abundance_f_cervus <- ~ year + season

# your K (as you used it)
K_use_cervus <- max(#rowSums(
  na.omit(df_stacked_cervus3y))+1
#)

K_use_cervus_max <- max(rowSums(
  na.omit(df_stacked_cervus3y)))

#variable selection according to AIC

sel_Nmix_cervus_aic3y <- backward_pcount_AIC_pcAIC(
  det_formula   = detection_f_cervus,
  state_formula = abundance_f_cervus,
  data = umf.cervus.NM3y,
  K = K_use_cervus_max,
  mixture = "NB",
  threads = 8,
  component = "det",
  keep_det = c("season", "year")
)

summary(sel_Nmix_cervus_aic3y$best_model)

# posterior distributions

# Get posterior distributions for each site
reNMix_sv_cervus_3y <- ranef(sel_Nmix_cervus_aic3y$best_model)

# checking the probabilities of the first station on the first season (4weeks)
reNMix_sv_cervus_3y@post[1, ,] # this is the estimated abundances and the probability that each abundance

# Extract site-specific abundance estimates
abundance_meanNmix_sv_cervus_3y <- bup(reNMix_sv_cervus_3y, stat = "mean")

#mean per station per year
abundance_meanNmix_sv_cervus_3y.df<- tibble(abundance_meanNmix_sv_cervus_3y, locationID=rep(all.sites,36), year=years3y) %>% 
  group_by(locationID, year) %>%  summarise(abundance_meanNmix3_sv_cervus= mean(na.omit(abundance_meanNmix_sv_cervus_3y)))

#separating each year and creating data frame
abundance_meanNmix_sv_cervus_3y22.df<- abundance_meanNmix_sv_cervus_3y.df %>% 
  filter(year==2022) %>% dplyr::select(-year)
abundance_meanNmix_sv_cervus_3y23.df<- abundance_meanNmix_sv_cervus_3y.df %>% 
  filter(year==2023) %>% dplyr::select(-year)
abundance_meanNmix_sv_cervus_3y24.df<- abundance_meanNmix_sv_cervus_3y.df %>% 
  filter(year==2024) %>% dplyr::select(-year)


#comparing to null models
cor(na.omit(abundance_meanNmix_sv_cervus_3y22.df$abundance_meanNmix3_sv_cervus),
    na.omit(abundance_meanNmix_nm_cervus_3y22.df$abundance_meanNmix3_nm_cervus))
cor(na.omit(abundance_meanNmix_sv_cervus_3y23.df$abundance_meanNmix3_sv_cervus),
    na.omit(abundance_meanNmix_nm_cervus_3y23.df$abundance_meanNmix3_nm_cervus))
cor(na.omit(abundance_meanNmix_sv_cervus_3y24.df$abundance_meanNmix3_sv_cervus),
    na.omit(abundance_meanNmix_nm_cervus_3y24.df$abundance_meanNmix3_nm_cervus))
#very correlated the model with sensible variables and the null model

#comparing to RAI counts
abundance_meanNmix_sv_cervus_3y22.df2 <- left_join(
  abundance_meanNmix_sv_cervus_3y22.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI22cervus)
cor(na.omit(abundance_meanNmix_sv_cervus_3y22.df2)$abundance_meanNmix3_sv_cervus,
    na.omit(abundance_meanNmix_sv_cervus_3y22.df2)$`Cervus elaphus`)

abundance_meanNmix_sv_cervus_3y23.df2 <- left_join(
  abundance_meanNmix_sv_cervus_3y23.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI23cervus)
cor(na.omit(abundance_meanNmix_sv_cervus_3y23.df2)$abundance_meanNmix3_sv_cervus,
    na.omit(abundance_meanNmix_sv_cervus_3y23.df2)$`Cervus elaphus`)

abundance_meanNmix_sv_cervus_3y24.df2 <- left_join(
  abundance_meanNmix_sv_cervus_3y24.df %>% mutate(locationName=substr(locationID,1,9))
  , RAI24cervus)
cor(na.omit(abundance_meanNmix_sv_cervus_3y24.df2)$abundance_meanNmix3_sv_cervus,
    na.omit(abundance_meanNmix_sv_cervus_3y24.df2)$`Cervus elaphus`)

#correlation between -.89 and .98 

#correlation between  years
cor(na.omit(abundance_meanNmix_sv_cervus_3y22.df$abundance_meanNmix3_sv_cervus),
    na.omit(abundance_meanNmix_sv_cervus_3y23.df$abundance_meanNmix3_sv_cervus))
cor(na.omit(tibble (abundance_meanNmix_sv_cervus_3y22.df$abundance_meanNmix3_sv_cervus,
                    abundance_meanNmix_sv_cervus_3y24.df$abundance_meanNmix3_sv_cervus)))
cor(na.omit(tibble (abundance_meanNmix_sv_cervus_3y23.df$abundance_meanNmix3_sv_cervus,
                    abundance_meanNmix_sv_cervus_3y24.df$abundance_meanNmix3_sv_cervus)))

# between 0.83 and 0.94 correlation, some spatial pattern is maintained


## gneral table with all results
#22
abundance_meanallmodels22_cervus <- abundance_meanNmix_sv_cervus_3y22.df %>%
  ungroup() %>%
  mutate(cervus_RN3nm22=abundance_meanRN_nm_cervus_3y22.df$abundance_meanRN3_nm_cervus) %>% 
  mutate(cervus_RN3sv22=abundance_meanRN_sv_cervus_3y22.df$abundance_meanRN3_sv_cervus) %>% 
  mutate(cervus_Nmix3nm22=abundance_meanNmix_nm_cervus_3y22.df$abundance_meanNmix3_nm_cervus) %>%
  mutate(cervus_Nmix3sv22=abundance_meanNmix_sv_cervus_3y22.df$abundance_meanNmix3_sv_cervus) %>%
  dplyr::select(-abundance_meanNmix3_sv_cervus)



##adding everything to a dataframe that will be used as the matrix of the GDM
#22
species_data_mat_Do3y_RN_nm22 <- species_data_mat_Do3y_RN_nm22 %>% 
  left_join(abundance_meanallmodels22_cervus %>% 
              dplyr::select(locationID, cervus_RN3nm22))

species_data_mat_Do3y_RN_sv22 <- species_data_mat_Do3y_RN_sv22 %>% 
  left_join(abundance_meanallmodels22_cervus %>% 
              dplyr::select(locationID, cervus_RN3sv22))

species_data_mat_Do3y_Nmix_nm22 <- species_data_mat_Do3y_Nmix_nm22 %>% 
  left_join(abundance_meanallmodels22_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3nm22))

species_data_mat_Do3y_Nmix_sv22 <- species_data_mat_Do3y_Nmix_sv22 %>% 
  left_join(abundance_meanallmodels22_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3sv22))


#23

abundance_meanallmodels23_cervus <- abundance_meanNmix_sv_cervus_3y23.df %>%
  ungroup() %>%
  mutate(cervus_RN3nm23=abundance_meanRN_nm_cervus_3y23.df$abundance_meanRN3_nm_cervus) %>% 
  mutate(cervus_RN3sv23=abundance_meanRN_sv_cervus_3y23.df$abundance_meanRN3_sv_cervus) %>% 
  mutate(cervus_Nmix3nm23=abundance_meanNmix_nm_cervus_3y23.df$abundance_meanNmix3_nm_cervus) %>%
  mutate(cervus_Nmix3sv23=abundance_meanNmix_sv_cervus_3y23.df$abundance_meanNmix3_sv_cervus) %>%
  dplyr::select(-abundance_meanNmix3_sv_cervus)

##adding everything to a dataframe that will be used as the matrix of the GDM
species_data_mat_Do3y_RN_nm23 <- species_data_mat_Do3y_RN_nm23 %>% 
  left_join(abundance_meanallmodels23_cervus %>% 
              dplyr::select(locationID, cervus_RN3nm23))

species_data_mat_Do3y_RN_sv23 <- species_data_mat_Do3y_RN_sv23 %>% 
  left_join(abundance_meanallmodels23_cervus %>% 
              dplyr::select(locationID, cervus_RN3sv23))

species_data_mat_Do3y_Nmix_nm23 <- species_data_mat_Do3y_Nmix_nm23 %>% 
  left_join(abundance_meanallmodels23_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3nm23))

species_data_mat_Do3y_Nmix_sv23 <- species_data_mat_Do3y_Nmix_sv23 %>% 
  left_join(abundance_meanallmodels23_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3sv23))


#24
abundance_meanallmodels24_cervus <- abundance_meanNmix_sv_cervus_3y24.df %>%
  ungroup() %>%
  mutate(cervus_RN3nm24=abundance_meanRN_nm_cervus_3y24.df$abundance_meanRN3_nm_cervus) %>% 
  mutate(cervus_RN3sv24=abundance_meanRN_sv_cervus_3y24.df$abundance_meanRN3_sv_cervus) %>% 
  mutate(cervus_Nmix3nm24=abundance_meanNmix_nm_cervus_3y24.df$abundance_meanNmix3_nm_cervus) %>%
  mutate(cervus_Nmix3sv24=abundance_meanNmix_sv_cervus_3y24.df$abundance_meanNmix3_sv_cervus) %>%
  dplyr::select(-abundance_meanNmix3_sv_cervus) 


##adding everything to a dataframe that will be used as the matrix of the GDM
species_data_mat_Do3y_RN_nm24 <- species_data_mat_Do3y_RN_nm24 %>% 
  left_join(abundance_meanallmodels24_cervus %>% 
              dplyr::select(locationID, cervus_RN3nm24))

species_data_mat_Do3y_RN_sv24 <- species_data_mat_Do3y_RN_sv24 %>% 
  left_join(abundance_meanallmodels24_cervus %>% 
              dplyr::select(locationID, cervus_RN3sv24))

species_data_mat_Do3y_Nmix_nm24 <- species_data_mat_Do3y_Nmix_nm24 %>% 
  left_join(abundance_meanallmodels24_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3nm24))

species_data_mat_Do3y_Nmix_sv24 <- species_data_mat_Do3y_Nmix_sv24 %>% 
  left_join(abundance_meanallmodels24_cervus %>% 
              dplyr::select(locationID, cervus_Nmix3sv24))
