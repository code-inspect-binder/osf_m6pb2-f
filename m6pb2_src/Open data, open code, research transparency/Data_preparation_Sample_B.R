
# load required packages
library(foreign) # read spss data
library(dplyr) # easy data manipulation

# load some useful functions
source("helpers.R")

######################################################
## For the codebook of this study explaining all 
## variable names, see osf.io/q5zwp.
######################################################

######################################################
### Read and prepare data from online survey
######################################################

# read data
survey <- read.spss( paste0(datadir,"01_pils_onlinesurvey_self.sav"), to.data.frame =T, use.value.labels = F) 

# select variables of interest
survey <- select(survey, id_a, age, sex, saq_13, saq_14)

######################################################
### Read and prepare data from round robin ratings
######################################################

# read data
rr_effects <- read.spss( paste0(datadir,"04_pils_sessiondata_effects.sav"), to.data.frame =T, use.value.labels = F) 

# aggregate outcome variables across time points (sessions 2 and 3)
rr_effects$t23active <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_active")])
rr_effects$t23satisfaction <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_satisfaction")])
rr_effects$t23optimistic <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_optimistic")])
rr_effects$t23trusting <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_trusting")])
rr_effects$t23determined <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_determined")])

rr_effects$t23leader.s <- rowMeans(rr_effects[,paste0("t",5:10,"_leadership_gm.s")])
rr_effects$t23leader.p <- rowMeans(rr_effects[,paste0("t",5:10,"_leadership_gm.p")])

rr_effects$t23liking.s <- rowMeans(rr_effects[,paste0("t",5:10,"_liking_gm.s")])
rr_effects$t23liking.p <- rowMeans(rr_effects[,paste0("t",5:10,"_liking_gm.p")])

rr_effects$t23friend.s <- rowMeans(rr_effects[,paste0("t",5:10,"_friendship_gm.s")])
rr_effects$t23friend.p <- rowMeans(rr_effects[,paste0("t",5:10,"_friendship_gm.p")])

rr_effects$t23intelligent.p <- rowMeans(rr_effects[,paste0("t",5:10,"_intelligence_gm.p")])

rr_effects$t23trust.s <- rowMeans(rr_effects[,paste0("t",5:10,"_trustworthiness_gm.s")])
rr_effects$t23trust.p <- rowMeans(rr_effects[,paste0("t",5:10,"_trustworthiness_gm.p")])

rr_effects$t23valence <- rowMeans(rr_effects[,paste0("t",5:10,"_stateaffect_pleasure")])


# select variables of interest
rr_effects <- select(rr_effects, id_a, t23active:t23valence)

######################################################
### Read and prepare data from cognitive ability tests
######################################################

# read data
abil <- read.spss( paste0(datadir,"06_pils_directobservations_cognitiveabilities.sav"), to.data.frame =T, use.value.labels = F) 

# select variables of interest
abil <- select(abil, id_a, raven_total, mwtb_total)

#########################
### Merge all data frames
#########################

pils1 <- merge(rr_effects, survey, by="id_a", all.x=T, all.y=T)
pils <- merge(pils1, abil, by="id_a", all.x=T, all.y=T)

# sort data frame by random variable (to ensure anonymity in the final dataframe)
pils <- arrange(pils, raven_total)

# rename and select variables
pils <- pils %>%
          select(
                    age = age, 
                    sex = sex, 
                    #
                    Raven_self = saq_14, # self-rated reasoning ability
                    Raven_obj = raven_total, # objectively measured reasoning ability
                    #
                    MWTB_self = saq_13, # self-rated vocabulary knowledge
                    MWTB_obj = mwtb_total, # objectively measured vocabulary knowledge
                    #
                    # outcome category "Global self-evaluation"
                    selfsat = t23satisfaction, # satisfied with oneself
                    liking_self = t23liking.s, # self-liking
                    # 
                    # outcome category "Well-being"
                    determined = t23determined, # determined
                    active = t23active, # active
                    optimistic = t23optimistic, # optimistic
                    valence = t23valence, # valence
                    #
                    # outcome category "Self-rated agentic outcomes"
                    leader_RR_self = t23leader.s, # self-rated leadership ability (RR)
                    trustabil_self = t23trusting, # trust in one's own abilities
                    #
                    # outcome category "Self-rated communal outcomes"
                    friend_self = t23friend.s, # self-rated friendship potential
                    trustworthy_RR_self = t23trust.s, # self-rated trustworthiness (RR)
                    #
                    # outcome category "Peer-rated agentic outcomes"
                    leader_peer = t23leader.p, # peer-rated leadership ability
                    intelligent_peer = t23intelligent.p, # peer-rated intelligence
                    #
                    # outcome category "Peer-rated communal outcomes"
                    friend_peer = t23friend.p, # peer-rated friendship potential
                    liking_peer = t23liking.p, # being liked by peers
                    trustworthy_peer = t23trust.p # peer-rated trustworthiness
          )


# Exclude subjects who have missing values on the ability test or on self-viewed ability
pils <- subset(pils,   
                      (Raven_self != "NA") 
                    & (Raven_obj != "NA") 
                    & (MWTB_self != "NA")
                    & (MWTB_obj != "NA")
                  )

# Save data frame for descriptive statistics analysis, before standardization
dir.create("Descriptives", showWarnings = FALSE)
pils_descr <- pils	
write.table(pils_descr, file="Descriptives/Data_Sample_B_pils_descr.txt",sep = "\t",col.names=TRUE)

# Add z-standardized variables
pils$Z_Raven_self = scale(pils$Raven_self)
pils$Z_Raven_obj = scale(pils$Raven_obj)
#
pils$Z_MWTB_self = scale(pils$MWTB_self)
pils$Z_MWTB_obj = scale(pils$MWTB_obj)
#
pils$Z_selfsat = scale(pils$selfsat)
pils$Z_liking_self = scale(pils$liking_self)
#
pils$Z_determined = scale(pils$determined)
pils$Z_active = scale(pils$active)
pils$Z_optimistic = scale(pils$optimistic)
pils$Z_valence = scale(pils$valence)
#
pils$Z_leader_RR_self = scale(pils$leader_RR_self)
pils$Z_trustabil_self = scale(pils$trustabil)
#
pils$Z_friend_self = scale(pils$friend_self)
pils$Z_trustworthy_RR_self = scale(pils$trustworthy_RR_self)
#
pils$Z_leader_peer = scale(pils$leader_peer)
pils$Z_intelligent_peer = scale(pils$intelligent_peer)
#
pils$Z_friend_peer = scale(pils$friend_peer)
pils$Z_liking_peer = scale(pils$liking_peer)
pils$Z_trustworthy_peer = scale(pils$trustworthy_peer)


# Aggregate outcome variables within the outcome categories
df_global_selfeval <- as.matrix(select(pils, Z_selfsat, Z_liking_self))
df_well_being <- as.matrix(select(pils, Z_determined, Z_active, Z_optimistic, Z_valence))
df_agency_self <- as.matrix(select(pils, Z_trustabil_self, Z_leader_RR_self))
df_comm_self <- as.matrix(select(pils, Z_friend_self, Z_trustworthy_RR_self))
df_agency_peer <- as.matrix(select(pils, Z_leader_peer, Z_intelligent_peer))
df_comm_peer <- as.matrix(select(pils, Z_friend_peer, Z_liking_peer, Z_trustworthy_peer))

pils$Z_global_selfeval = scale(rowMeans(df_global_selfeval, na.rm=T))
pils$Z_well_being = scale(rowMeans(df_well_being, na.rm=T))
pils$Z_agency_self = scale(rowMeans(df_agency_self, na.rm=T))
pils$Z_comm_self = scale(rowMeans(df_comm_self, na.rm=T))
pils$Z_agency_peer = scale(rowMeans(df_agency_peer, na.rm=T))
pils$Z_comm_peer = scale(rowMeans(df_comm_peer, na.rm=T))

# Select variables for the analyses and save data frame that will be uploaded in the OSF
pils_osf <- select(pils, Z_Raven_self:Z_MWTB_obj, Z_global_selfeval:Z_comm_peer)
write.table(pils_osf, file="Data_Sample_B_pils.txt",sep = "\t",col.names=TRUE)


#########################
## DESCRIPTIVE STATISTICS
#########################

# compute and save sample statistics (age distribution, number of females)
age <- round(select(psych::describe(pils_descr$age), n, min, max, mean, sd),2)
age$n <- nrow(pils_descr)
sampstats <- mutate(age, 
                    female=plyr::count(pils_descr$sex)[plyr::count(pils_descr$sex)[,1]=="1",]["freq"]
)
write.table(sampstats, file="Descriptives/age_sex_Sample_B_pils.dat", sep="\t", row.names=FALSE)

# compute and save descriptives statistics of variables before aggregation and standardization
descriptives <- round(select(psych::describe(pils_descr), n, min, max, mean, sd),2)
write.table(descriptives, file="Descriptives/descriptives_Sample_B_pils.dat", sep="\t")

# compute and save correlation table of variables before aggregation and standardization
cor_raw <- corcons(pils_descr)
write.table(cor_raw, file="Descriptives/correlations_raw_Sample_B_pils.dat", sep="\t")

# create folder for descriptive statistics
dir.create("Descriptives", showWarnings = FALSE)
          
# compute internal concistencies of the outcome aggregates
a_global_selfeval <- round(psych::alpha(df_global_selfeval)$total$raw_alpha,2)
a_well_being <- round(psych::alpha(df_well_being)$total$raw_alpha,2)
a_agency_self <- round(psych::alpha(df_agency_self)$total$raw_alpha,2)
a_comm_self <- round(psych::alpha(df_comm_self)$total$raw_alpha,2)
a_agency_peer <- round(psych::alpha(df_agency_peer)$total$raw_alpha,2)
a_comm_peer <- round(psych::alpha(df_comm_peer)$total$raw_alpha,2)
a_achievement <- "-"

alpha_pils <- data.frame(alpha_global_selfeval = a_global_selfeval,
                         alpha_well_being = a_well_being,
                         alpha_agency_self = a_agency_self,
                         alpha_comm_self = a_comm_self,
                         alpha_agency_peer = a_agency_peer,
                         alpha_comm_peer = a_comm_peer,
                         alpha_achievement = a_achievement
                         )
write.table(alpha_pils, file="Descriptives/alpha_Sample_B_pils.dat", sep="\t", row.names=FALSE)

# compute and save correlation table of self-rated and objective ability measures and outcome aggregates, 
varnames <- c("Raven","reasoning_self","MWTB","vocabulary_self","global_selfevaluation","well_being","agency_self","communion_self","agency_peer","communion_peer")
outcomes_pils <- dplyr::select(pils, Z_Raven_obj, Z_Raven_self, Z_MWTB_obj, Z_MWTB_self, Z_global_selfeval, Z_well_being, Z_agency_self, Z_comm_self, Z_agency_peer, Z_comm_peer)
names(outcomes_pils) <- varnames
cor_aggr <- corcons(outcomes_pils)
write.table(cor_aggr, file="Descriptives/correlations_aggr_Sample_B_pils.dat", sep="\t")


  
          