
# load required packages
library(foreign) # read spss data
library(dplyr) # easy data manipulation

# load some useful functions
source("helpers.R")

# read data
mSI <- read.spss( paste0(datadir,"Dataset_paper_prepared_1.sav"), to.data.frame =T, use.value.labels = F) 

# set those study grades to NA which are outside the range of the grading system
mSI$crt.gru_s_w2[mSI$crt.gru_s_w2 == 0] <- NA
mSI$crt.gru_s_w2[mSI$crt.gru_s_w2 > 6] <- NA


# sort data frame by random variable (to ensure anonymity in the final dataframe)
mSI <- arrange(mSI, naf_d_w2)

# rename and select variables
mSI <- mSI %>%
  select(
	age = age_s, # age
	sex = sex_s, # sex
	MWTB_self = per.iqvSCL_s, # self-rated vocabulary knowledge
	MWTB_obj = per.iqv_t, # objectively measured vocabulary knowledge
	#
	# outcome category "Global self-evaluation"
	self_esteem_survey = ses_s_w2, # Rosenberg in online survey
	self_esteem_diary = ses_d_w2, # Rosenberg diary
	#
	# outcome category "Well-being"
	life_satisfaction = swl_s_w2, # Life satisfaction
	optimistic = opt_s_w2, # Optimism 
	positive_affect = paf_d_w2, # Positive affect diary
	negative_affect = naf_d_w2, #   Negative affect diary
	depression = dep_s_w2, #   Depression
	#
	# outcome category "Self-rated agentic outcomes"
	leader_authority_self = nar.alaNPI_s_w2, #  NPI leadership/authority sub (facet by Ackerman et al.)
	agentic_abstract_other_self = bta.agcSAO_s_w2, # agentic above average rating, abstract other
	agentic_concrete_other_self = bta.agcSCO_s_w2, # agentic above average rating, concrete other
	#
	# outcome category "Self-rated communal outcomes"
	comm_abstract_other_self = bta.comSAO_s_w2, # communal above average rating, abstract other
	comm_concrete_other_self = bta.comSCO_s_w2, # communal above average rating, concrete other
	#
	# outcome category "Peer-rated agentic outcomes"
	social_influence_peer = sin_p_w2, # social influence peer-rated
	#
	# outcome category "Peer-rated communal outcomes"
	liking_peer = qfr.lfr_p_w2, # (peer-rated) Likabilty
	interaction_quality_peer = qfr.qoi_p_w2, # quality of interactions
	conflict_peer = cfl_p_w2, # Relationship conflict
	#
	# outcome category "Achievement"
	abi_grade = crt.grs_s_w2, # grade in the Abitur, coded in German grading format (lower value = better grade)
	study_grade = crt.gru_s_w2 # final grade for studies, coded in German grading format (lower value = better grade)
        ) 


# Exclude subjects who have missing values on the ability test or on self-viewed ability
mSI <- subset(mSI,   
	  (MWTB_self != "NA") 
	  & (MWTB_obj != "NA") 
	  )

# Save data frame for descriptive statistics analysis, before standardization
dir.create("Descriptives", showWarnings = FALSE)
mSI_descr <- mSI	
write.table(mSI_descr, file="Descriptives/Data_Sample_E_mSI_descr.txt",sep = "\t",col.names=TRUE)

# Add z-standardized variables
mSI$Z_MWTB_self = scale(mSI$MWTB_self)
mSI$Z_MWTB_obj = scale(mSI$MWTB_obj)
#
mSI$Z_self_esteem_survey = scale(mSI$self_esteem_survey)
mSI$Z_self_esteem_diary = scale(mSI$self_esteem_diary)
# 
mSI$Z_life_satisfaction = scale(mSI$life_satisfaction)
mSI$Z_optimistic = scale(mSI$optimistic)
mSI$Z_positive_affect = scale(mSI$positive_affect)
mSI$Z_negative_affect = scale(mSI$negative_affect)
mSI$Z_depression = scale(mSI$depression)
# 
mSI$Z_leader_authority_self = scale(mSI$leader_authority_self)
mSI$Z_agentic_abstract_other_self = scale(mSI$agentic_abstract_other_self)
mSI$Z_agentic_concrete_other_self = scale(mSI$agentic_concrete_other_self)
# 
mSI$Z_comm_abstract_other_self = scale(mSI$comm_abstract_other_self)
mSI$Z_comm_concrete_other_self = scale(mSI$comm_concrete_other_self)
# 
mSI$Z_social_influence_peer = scale(mSI$social_influence_peer)
# 
mSI$Z_liking_peer = scale(mSI$liking_peer)
mSI$Z_interaction_quality_peer = scale(mSI$interaction_quality_peer)
mSI$Z_conflict_peer = scale(mSI$conflict_peer)
#
mSI$Z_abi_grade = scale(mSI$abi_grade)
mSI$Z_study_grade = scale(mSI$study_grade)



# Aggregate outcome variables within the outcome categories
df_global_selfeval <- as.matrix(select(mSI, Z_self_esteem_survey, Z_self_esteem_diary))
#
df_well_being <- select(mSI, Z_positive_affect, Z_negative_affect, Z_depression, Z_life_satisfaction, Z_optimistic) # before inverting negative affect and depression
df_well_being$Z_negative_affect <- -df_well_being$Z_negative_affect # coding negative affect in the opposite direction
df_well_being$Z_depression <- -df_well_being$Z_depression # coding depression in the opposite direction
#
df_agency_self <- as.matrix(select(mSI, Z_leader_authority_self, Z_agentic_abstract_other_self, Z_agentic_concrete_other_self))
df_comm_self <- as.matrix(select(mSI, Z_comm_abstract_other_self, Z_comm_concrete_other_self))
df_agency_peer <- as.matrix(select(mSI, Z_social_influence_peer))
df_comm_peer <- select(mSI, Z_liking_peer, Z_interaction_quality_peer, Z_conflict_peer) # before inverting conflict
df_comm_peer$Z_conflict_peer <- -df_comm_peer$Z_conflict_peer # coding conflict in the opposite direction
df_achievement <- select(mSI, Z_abi_grade, Z_study_grade) # before inverting abi grades and study grades
df_achievement$Z_abi_grade <- -df_achievement$Z_abi_grade
df_achievement$Z_study_grade <- -df_achievement$Z_study_grade

mSI$Z_global_selfeval = scale(rowMeans(df_global_selfeval, na.rm=T))
mSI$Z_well_being = scale(rowMeans(df_well_being, na.rm=T))
mSI$Z_agency_self = scale(rowMeans(df_agency_self, na.rm=T))
mSI$Z_comm_self = scale(rowMeans(df_comm_self, na.rm=T))
mSI$Z_agency_peer = scale(rowMeans(df_agency_peer, na.rm=T))
mSI$Z_comm_peer = scale(rowMeans(df_comm_peer, na.rm=T))
mSI$Z_achievement = scale(rowMeans(df_achievement, na.rm=T))

# Select variables for the analyses and save data frame that will be uploaded in the OSF
mSI_osf <- select(mSI, Z_MWTB_self:Z_MWTB_obj, Z_global_selfeval:Z_achievement)
write.table(mSI_osf, file="Data_Sample_E_mSI.txt",sep = "\t",col.names=TRUE)


#########################
## DESCRIPTIVE STATISTICS
#########################

# compute and save sample statistics (age distribution, number of females)
age <- round(select(psych::describe(mSI_descr$age), n, min, max, mean, sd),2)
age$n <- nrow(mSI_descr)
sampstats <- mutate(age, 
                    female=plyr::count(mSI_descr$sex)[plyr::count(mSI_descr$sex)[,1]=="0",]["freq"]
)
write.table(sampstats, file="Descriptives/age_sex_Sample_E_mSI.dat", sep="\t", row.names=FALSE)

# compute and save descriptives statistics of variables before aggregation and standardization
descriptives <- round(select(psych::describe(mSI_descr), n, min, max, mean, sd),2)
write.table(descriptives, file="Descriptives/descriptives_Sample_E_mSI.dat", sep="\t")

# compute and save correlation table of variables before aggregation and standardization
cor_raw <- corcons(mSI_descr)
write.table(cor_raw, file="Descriptives/correlations_raw_Sample_E_mSI.dat", sep="\t")	

# create folder for descriptive statistics
dir.create("Descriptives", showWarnings = FALSE)

# compute and save internal concistencies of the outcome aggregates
a_global_selfeval <- round(psych::alpha(df_global_selfeval)$total$raw_alpha,2)
a_well_being <- round(psych::alpha(df_well_being)$total$raw_alpha,2)
a_agency_self <-  round(psych::alpha(df_agency_self)$total$raw_alpha,2)
a_comm_self <-  round(psych::alpha(df_comm_self)$total$raw_alpha,2)
a_agency_peer <- "-"
a_comm_peer <- round(psych::alpha(df_comm_peer)$total$raw_alpha,2)
a_achievement <- round(psych::alpha(df_achievement)$total$raw_alpha,2)

alpha_mSI <- data.frame(alpha_global_selfeval = a_global_selfeval,
                         alpha_well_being = a_well_being,
                         alpha_agency_self = a_agency_self,
                         alpha_comm_self = a_comm_self,
                         alpha_agency_peer = a_agency_peer,
                         alpha_comm_peer = a_comm_peer,
                         alpha_achievement = a_achievement
)
write.table(alpha_mSI, file="Descriptives/alpha_Sample_E_mSI.dat", sep="\t", row.names=FALSE)

# compute and save correlation table of self-rated and objective ability measures and outcome aggregates, 
varnames <- c("MWTB","vocabulary_self","global_selfevaluation","well_being","agency_self","communion_self","agency_peer","communion_peer","achievement")
outcomes_mSI <- dplyr::select(mSI, Z_MWTB_obj, Z_MWTB_self, Z_global_selfeval, Z_well_being, Z_agency_self, Z_comm_self, Z_agency_peer, Z_comm_peer, Z_achievement)
names(outcomes_mSI) <- varnames
cor_aggr <- corcons(outcomes_mSI)
write.table(cor_aggr, file="Descriptives/correlations_aggr_Sample_E_mSI.dat", sep="\t")

	
		
		