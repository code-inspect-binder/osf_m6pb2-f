
# load required packages
library(foreign) # read spss data
library(dplyr) # easy data manipulation

# load some useful functions
source("helpers.R")

# read data
mst2 <- read.table(paste0(datadir,"fullymerged.txt"), header=TRUE, sep = "\t", dec = "." )
mst2_study_grade <- read.spss(paste0(datadir,"cijfers_clean.sav"), to.data.frame =T, use.value.labels = F) 
mst2_abi_grade <- read.spss(paste0(datadir,"pt_y1.sav"), to.data.frame =T, use.value.labels = F) 
mst2_perceiver_y1 <- read.spss( paste0(datadir,"pt_iq_y1.sav"), to.data.frame =T, use.value.labels = F) 
mst2_perceiver_y5 <- read.spss( paste0(datadir,"pt_y5.sav"), to.data.frame =T, use.value.labels = F) 
mst2_perceiver_y6 <- read.spss( paste0(datadir,"pt_y6.sav"), to.data.frame =T, use.value.labels = F) 

# delete variables that would be duplicated when merging the data frames
mst2_perceiver_y1$wave <- NULL
mst2_perceiver_y5$wave <- NULL
mst2_perceiver_y6$wave <- NULL

# select abi grade from dataframe
mst2_abi_grade <- select(mst2_abi_grade, vp_id, gpa_highschool)

# merge data frames
mst2 <- merge(mst2, mst2_study_grade, by.x="vp_id", by.y="vp_id", all.x=T, all.y=T)
mst2 <- merge(mst2, mst2_abi_grade, by.x="vp_id", by.y="vp_id", all.x=T, all.y=T)
mst2 <- merge(mst2, mst2_perceiver_y1, by.x="vp_id", by.y="vp_id", all.x=T, all.y=T)
mst2 <- merge(mst2, mst2_perceiver_y5, by.x="vp_id", by.y="vp_id", all.x=T, all.y=T)
mst2 <- merge(mst2, mst2_perceiver_y6, by.x="vp_id", by.y="vp_id", all.x=T, all.y=T)

# sort data frame by random variable (to ensure anonymity in the final dataframe)
mst2 <- arrange(mst2, co.t_y6)


# aggregate outcome variables across time points
self_esteem_df <- as.matrix(select(mst2, se_y5, se_y6)) # self esteem
liking_self_df <- as.matrix(select(mst2, li_s_y5, li_s_y6)) # self-liking

paff_df <- as.matrix(select(mst2, pa_y5, pa_y6)) # positive affect
naff_df <- as.matrix(select(mst2, na_y5, na_y6)) # negative affect
depression_df <- as.matrix(select(mst2, de_y5, de_y6)) # depression

group_influence_self_df <- as.matrix(select(mst2, gi_s_y5, gi_s_y6)) # self-rated group_influence

intelligent_peer_df <- as.matrix(select(mst2, iq.t, iq.t_y6)) # peer-rated reasoning ability
group_influence_peer_df <- as.matrix(select(mst2, gi.t, gi.t_y6)) # peer-rated group influence

liking_peer_df <- as.matrix(select(mst2, li.t, li.t_y6)) # being liked by peers
friend_peer_df <- as.matrix(select(mst2, fr.t, fr.t_y6)) # peer-rated friendship potential
emosupp_peer_df <- as.matrix(select(mst2, se.t, se.t_y6)) # peer-rated emotional support

mst2 <- mst2 %>%
          mutate(
                    self_esteem_t56 = rowMeans(self_esteem_df, na.rm=T),
                    liking_self_t56 = rowMeans(liking_self_df, na.rm=T), 
                    #
                    paff_t56 = rowMeans(paff_df, na.rm=T),
                    naff_t56 = rowMeans(naff_df, na.rm=T),
                    depression_t56 = rowMeans(depression_df, na.rm=T),
                    #
                    group_influence_self_t56 = rowMeans(group_influence_self_df, na.rm=T),
                    #
                    intelligent_peer_t56 = rowMeans(intelligent_peer_df, na.rm=T),
                    group_influence_peer_t56 = rowMeans(group_influence_peer_df, na.rm=T),
                    #
                    liking_peer_t56 = rowMeans(liking_peer_df, na.rm=T),
                    friend_peer_t56 = rowMeans(friend_peer_df, na.rm=T),
                    emosupp_peer_t56 = rowMeans(emosupp_peer_df, na.rm=T)
          )

# rename and select variables
mst2 <- mst2 %>%
          select(
                    age = vp_age, # age
                    sex = vp_female,  # sex
                    #
                    Raven_self = iq_s_y1, # self-rated reasoning ability
                    Raven_obj = raven_r, # objectively measured reasoning ability
                    #
                    # outcome category "Global self-evaluation"
                    self_esteem = self_esteem_t56, # self-esteem
                    liking_self = liking_self_t56, # self-liking
                    #
                    # outcome category "Well-being"
                    paff = paff_t56, # positive affect
                    naff = naff_t56, # negative affect
                    depression = depression_t56, # depression
                    #
                    # outcome category "Self-rated agentic outcomes"
                    group_influence_self = group_influence_self_t56, # self-rated group influence
                    #
                    # outcome category "Peer-rated agentic outcomes"
                    intelligent_peer = intelligent_peer_t56, # peer-rated reasoning ability
                    group_influence_peer = group_influence_peer_t56, # peer-rated group influence
                    #
                    # outcome category "Peer-rated communal outcomes"
                    liking_peer = liking_peer_t56, # being liked by peers
                    friend_peer = friend_peer_t56, # peer-rated friendship quality
                    emosupp_peer = emosupp_peer_t56, # peer-rated emotional support
                    #
                    # outcome category "achievement"
                    study_grade = grade_mean,
                    abi_grade = gpa_highschool
          )

# Exclude subjects who have missing values on the ability test or on self-viewed ability
mst2 <- subset(mst2,   
                              (Raven_self != "NA") 
                              & (Raven_obj != "NA") 
                              )

# Save data frame for descriptive statistics analysis, before standardization
dir.create("Descriptives", showWarnings = FALSE)
mst2_descr <- mst2	
write.table(mst2_descr, file="Descriptives/Data_Sample_A_mst2_descr.txt",sep = "\t",col.names=TRUE)

# Add z-standardized variables
mst2$Z_Raven_self = scale(mst2$Raven_self)
mst2$Z_Raven_obj = scale(mst2$Raven_obj)
#
mst2$Z_self_esteem = scale(mst2$self_esteem)
mst2$Z_liking_self = scale(mst2$liking_self)
#
mst2$Z_paff = scale(mst2$paff)
mst2$Z_naff = scale(mst2$naff)
mst2$Z_depression = scale(mst2$depression)
#
mst2$Z_group_influence_self = scale(mst2$group_influence_self)
#
mst2$Z_intelligent_peer = scale(mst2$intelligent_peer)
mst2$Z_group_influence_peer = scale(mst2$group_influence_peer)
#
mst2$Z_liking_peer = scale(mst2$liking_peer)
mst2$Z_friend_peer = scale(mst2$friend_peer)
mst2$Z_emosupp_peer = scale(mst2$emosupp_peer)
#
mst2$Z_abi_grade = scale(mst2$abi_grade)
mst2$Z_study_grade = scale(mst2$study_grade)

# Aggregate outcome variables within the outcome categories
df_global_selfeval <- as.matrix(select(mst2, Z_self_esteem, Z_liking_self))
df_well_being <- select(mst2, Z_paff, Z_naff, Z_depression) # before inverting naff and depression
df_well_being$Z_naff <- -df_well_being$Z_naff # coding negative affect in the opposite direction
df_well_being$Z_depression <- -df_well_being$Z_depression # coding depression in the opposite direction
df_agency_self <- as.matrix(select(mst2, Z_group_influence_self))
df_agency_peer <- as.matrix(select(mst2, Z_intelligent_peer, Z_group_influence_peer))
df_comm_peer <- as.matrix(select(mst2, Z_liking_peer, Z_friend_peer, Z_emosupp_peer))
df_achievement <- as.matrix(select(mst2, Z_abi_grade, Z_study_grade))

mst2$Z_global_selfeval = scale(rowMeans(df_global_selfeval, na.rm=T))
mst2$Z_well_being = scale(rowMeans(df_well_being, na.rm=T))
mst2$Z_agency_self = scale(rowMeans(df_agency_self, na.rm=T))
mst2$Z_agency_peer = scale(rowMeans(df_agency_peer, na.rm=T))
mst2$Z_comm_peer = scale(rowMeans(df_comm_peer, na.rm=T))
mst2$Z_achievement = scale(rowMeans(df_achievement, na.rm=T))

# Select variables for the analyses and save data frame that will be uploaded in the OSF
mst2_osf <- select(mst2, Z_Raven_self:Z_Raven_obj, Z_global_selfeval:Z_achievement)
write.table(mst2_osf, file="Data_Sample_A_mst2.txt",sep = "\t",col.names=TRUE)


#########################
## DESCRIPTIVE STATISTICS
#########################

# compute and save sample statistics (age distribution, number of females)
age <- round(select(psych::describe(mst2_descr$age), n, min, max, mean, sd),2)
age$n <- nrow(mst2_descr)
sampstats <- mutate(age, 
                    female=plyr::count(mst2_descr$sex)[plyr::count(mst2_descr$sex)[,1]=="1",]["freq"]
)
write.table(sampstats, file="Descriptives/age_sex_Sample_A_mst2.dat", sep="\t", row.names=FALSE)

# compute and save descriptives statistics of variables before aggregation and standardization
descriptives <- round(select(psych::describe(mst2_descr), n, min, max, mean, sd),2)
write.table(descriptives, file="Descriptives/descriptives_Sample_A_mst2.dat", sep="\t")

# compute and save correlation table of variables before aggregation and standardization
cor_raw <- corcons(mst2_descr)
write.table(cor_raw, file="Descriptives/correlations_raw_Sample_A_mst2.dat", sep="\t")

# create folder for descriptive statistics
dir.create("Descriptives", showWarnings = FALSE)

# compute and save internal concistencies of the outcome aggregates
a_global_selfeval <- round(psych::alpha(df_global_selfeval)$total$raw_alpha,2)
a_well_being <- round(psych::alpha(df_well_being)$total$raw_alpha,2)
a_agency_self <-  "-"
a_comm_self <-  "-"
a_agency_peer <- round(psych::alpha(df_agency_peer)$total$raw_alpha,2)
a_comm_peer <- round(psych::alpha(df_comm_peer)$total$raw_alpha,2)
a_achievement <- round(psych::alpha(df_achievement)$total$raw_alpha,2)

alpha_mst2 <- data.frame(alpha_global_selfeval = a_global_selfeval,
                         alpha_well_being = a_well_being,
                         alpha_agency_self = a_agency_self,
                         alpha_comm_self = a_comm_self,
                         alpha_agency_peer = a_agency_peer,
                         alpha_comm_peer = a_comm_peer,
                         alpha_achievement = a_achievement
)
write.table(alpha_mst2, file="Descriptives/alpha_Sample_A_mst2.dat", sep="\t", row.names=FALSE)


# compute and save correlation table of self-rated and objective ability measures and outcome aggregates, 
varnames <- c("Raven","reasoning_self","global_selfevaluation","well_being","agency_self","agency_peer","communion_peer","achievement")
outcomes_mst2 <- dplyr::select(mst2, Z_Raven_obj, Z_Raven_self, Z_global_selfeval, Z_well_being, Z_agency_self, Z_agency_peer, Z_comm_peer, Z_achievement)
names(outcomes_mst2) <- varnames
cor_aggr <- corcons(outcomes_mst2)
write.table(cor_aggr, file="Descriptives/correlations_aggr_Sample_A_mst2.dat", sep="\t")




