
# load required packages
library(foreign) # read spss data
library(dplyr) # easy data manipulation

# load some useful functions
source("helpers.R")

# read data
mst1 <- read.spss( paste0(datadir,"intellectual_SE.sav"), to.data.frame =T, use.value.labels = F) 

# some relevant peer variables are coded as 0, when the peer did not provide any ratings. Recode these.
zero_pages <- which(mst1$pagef == 0)
mst1[zero_pages, "pagef"] <- NA
mst1[zero_pages, "aaaf"] <- NA
mst1[zero_pages, "liking"] <- NA
mst1[zero_pages, "se_pc"] <- NA
mst1[zero_pages, "aacf"] <- NA

# exlude relevant variables from peers who did not follow instructions: 
# peers who did not complete the questionnaire (26 peers):
incomplete_survey <- which(mst1$pagef <= 17)
mst1[incomplete_survey, "aaaf"] <- NA
mst1[incomplete_survey, "liking"] <- NA
mst1[incomplete_survey, "se_pc"] <- NA
mst1[incomplete_survey, "aacf"] <- NA

# peers who completed the questionnaire but were unrealistically fast (6 peers)
unrealistically_fast <- which(mst1$pagef >= 18 & mst1$gtimef <= 600)
mst1[unrealistically_fast, "aaaf"] <- NA
mst1[unrealistically_fast, "liking"] <- NA
mst1[unrealistically_fast, "se_pc"] <- NA
mst1[unrealistically_fast, "aacf"] <- NA

# one participant has a typing error in the age variable (stated he was 2). Set to NA.
mst1[which(mst1$age == 2), "age"] <- NA

# remove the 12-year old participant
mst1 <- mst1[-which(mst1$age == 12),]

# sort data frame by random variable (to ensure anonymity in the final dataframe)
mst1 <- arrange(mst1, NPI_ss)

# rename and select variables
mst1 <- mst1 %>%
  select(
	age = age,  # age
	sex = sex,  # sex
	#
	MWTB_self = inte, # self-rated vocabulary knowledge
	MWTB_obj = mwtscore, # objectively measured vocabulary knowledge
	#
	# outcome category "Global self-evaluation"
	self_esteem = se, # self-esteem
	#
          # outcome category "Well-being"
	optimistic = optim, # Optimismus 
	pessimistic = pess, # pessimismus
	positive_affect = paff, # positive affect
	negative_affect = naff, # -negative affect
	depression = depr,  # depression
	life_satisfaction = ls, # life satisfaction
	#
	# outcome category "Self-rated agentic outcomes"
	leader_authority_self = NPI_la, # NPI-Facette Leadership
	agentic_above_average_self = aaa, # agentic above average selbst
	#
	# outcome category "Self-rated communal outcomes"
	comm_above_average_self = aac, # communal above average
	#
	# outcome category "Peer-rated agentic outcomes"
	agentic_above_average_peer = aaaf, # agentic above average fremd 
	#
	# outcome category "Peer-rated communal outcomes"
	liking_peer = liking, # (like 1,2,6,7)
	emosupp_peer = se_pc, # perceived emotional support: se_pc = (use1 + use2 + use3 + use4) / 4
	comm_above_average_peer = aacf # communal above average fremd
        )



# Exclude subjects who have missing values on the ability test or on self-viewed ability
mst1 <- subset(mst1,   
                              (MWTB_self != "NA") 
                              & (MWTB_obj != "NA") 
                              )

# Save data frame for descriptive statistics analysis, before standardization
dir.create("Descriptives", showWarnings = FALSE)
mst1_descr <- mst1	
write.table(mst1_descr, file="Descriptives/Data_Sample_D_mst1_descr.txt",sep = "\t",col.names=TRUE)

# Add z-standardized variables
mst1$Z_MWTB_self = scale(mst1$MWTB_self)
mst1$Z_MWTB_obj = scale(mst1$MWTB_obj)
#
mst1$Z_self_esteem = scale(mst1$self_esteem)
#
mst1$Z_optimistic = scale(mst1$optimistic)
mst1$Z_pessimistic = scale(mst1$pessimistic)
mst1$Z_positive_affect = scale(mst1$positive_affect)
mst1$Z_negative_affect = scale(mst1$negative_affect)
mst1$Z_depression = scale(mst1$depression)
mst1$Z_life_satisfaction = scale(mst1$life_satisfaction)
#
mst1$Z_leader_authority_self = scale(mst1$leader_authority_self)
mst1$Z_agentic_above_average_self = scale(mst1$agentic_above_average_self)
#
mst1$Z_comm_above_average_self = scale(mst1$comm_above_average_self)
#
mst1$Z_agentic_above_average_peer = scale(mst1$agentic_above_average_peer)
#
mst1$Z_liking_peer = scale(mst1$liking_peer)
mst1$Z_emosupp_peer = scale(mst1$emosupp_peer)
mst1$Z_comm_above_average_peer = scale(mst1$comm_above_average_peer)


# Aggregate outcome variables within the outcome categories
df_global_selfeval <- as.matrix(select(mst1, Z_self_esteem))
#
df_well_being <- select(mst1, Z_optimistic, Z_pessimistic, Z_positive_affect, Z_negative_affect, Z_depression, Z_life_satisfaction) # before inverting pessimism, negative affect and depression
df_well_being$Z_pessimistic <- -df_well_being$Z_pessimistic # coding pessimism in the opposite direction
df_well_being$Z_negative_affect <- -df_well_being$Z_negative_affect # coding negative affect in the opposite direction
df_well_being$Z_depression <- -df_well_being$Z_depression # coding depression in the opposite direction
#
df_agency_self <- as.matrix(select(mst1, Z_leader_authority_self, Z_agentic_above_average_self))
df_comm_self <- as.matrix(select(mst1, Z_comm_above_average_self))
df_agency_peer <- as.matrix(select(mst1, Z_agentic_above_average_peer))
df_comm_peer <- as.matrix(select(mst1, Z_comm_above_average_peer, Z_liking_peer, Z_emosupp_peer))

mst1$Z_global_selfeval = scale(rowMeans(df_global_selfeval, na.rm=T))
mst1$Z_well_being = scale(rowMeans(df_well_being, na.rm=T))
mst1$Z_agency_self = scale(rowMeans(df_agency_self, na.rm=T))
mst1$Z_comm_self = scale(rowMeans(df_comm_self, na.rm=T))
mst1$Z_agency_peer = scale(rowMeans(df_agency_peer, na.rm=T))
mst1$Z_comm_peer = scale(rowMeans(df_comm_peer, na.rm=T))

# Select variables for the analyses and save data frame that will be uploaded in the OSF
mst1_osf <- select(mst1, Z_MWTB_self:Z_MWTB_obj, Z_global_selfeval:Z_comm_peer)
write.table(mst1_osf, file="Data_Sample_D_mst1.txt",sep = "\t",col.names=TRUE)


#########################
## DESCRIPTIVE STATISTICS
#########################

# compute and save sample statistics (age distribution, number of females)
age <- round(select(psych::describe(mst1_descr$age), n, min, max, mean, sd),2)
age$n <- nrow(mst1_descr)
sampstats <- mutate(age, 
                    female=plyr::count(mst1_descr$sex)[plyr::count(mst1_descr$sex)[,1]=="2",]["freq"]
)
write.table(sampstats, file="Descriptives/age_sex_Sample_D_mst1.dat", sep="\t", row.names=FALSE)

# compute and save descriptives statistics of variables before aggregation and standardization
descriptives <- round(select(psych::describe(mst1_descr), n, min, max, mean, sd),2)
write.table(descriptives, file="Descriptives/descriptives_Sample_D_mst1.dat", sep="\t")

# compute and save correlation table of variables before aggregation and standardization
cor_raw <- corcons(mst1_descr)
write.table(cor_raw, file="Descriptives/correlations_raw_Sample_D_mst1.dat", sep="\t")

# create folder for descriptive statistics
dir.create("Descriptives", showWarnings = FALSE)

# compute and save internal concistencies of the outcome aggregates
a_global_selfeval <- "-"
a_well_being <- round(psych::alpha(df_well_being)$total$raw_alpha,2)
a_agency_self <-  round(psych::alpha(df_agency_self)$total$raw_alpha,2)
a_comm_self <-  "-"
a_agency_peer <- "-"
a_comm_peer <- round(psych::alpha(df_comm_peer)$total$raw_alpha,2)
a_achievement <- "-"

alpha_mst1 <- data.frame(alpha_global_selfeval = a_global_selfeval,
                         alpha_well_being = a_well_being,
                         alpha_agency_self = a_agency_self,
                         alpha_comm_self = a_comm_self,
                         alpha_agency_peer = a_agency_peer,
                         alpha_comm_peer = a_comm_peer,
                         alpha_achievement = a_achievement
)
write.table(alpha_mst1, file="Descriptives/alpha_Sample_D_mst1.dat", sep="\t", row.names=FALSE)

# compute and save correlation table of self-rated and objective ability measures and outcome aggregates, 
varnames <- c("MWTB","vocabulary_self","global_selfevaluation","well_being","agency_self","communion_self","agency_peer","communion_peer")
outcomes_mst1 <- dplyr::select(mst1, Z_MWTB_obj, Z_MWTB_self, Z_global_selfeval, Z_well_being, Z_agency_self, Z_comm_self, Z_agency_peer, Z_comm_peer)
names(outcomes_mst1) <- varnames
cor_aggr <- corcons(outcomes_mst1)
write.table(cor_aggr, file="Descriptives/correlations_aggr_Sample_D_mst1.dat", sep="\t")


