
# load required packages
library(foreign) # read spss data
library(haven) # read spss data (function read_sav)
library(dplyr) # easy data manipulation

# load some useful functions
source("helpers.R")

######################################################
## For the codebook of this study explaining all 
## variable names, see osf.io/2pmcr.
######################################################

######################################################
### Read and prepare data from cognitive ability tests
######################################################

# read data
abil <- read.spss(paste0(datadir,"14_connect_directobservations_cognitiveabilities.sav"), to.data.frame =T, use.value.labels = F) 

# select variables of interest
abil <- select(abil, id_a, raven_total, mwtb_total)

###########################################################
### Read and prepare data from event-based assessment (app)
###########################################################

### Self-ratings

# read app data in "uniqueinteractions" format = one interaction per row
app_unique <- read.spss(paste0(datadir,"13_connect_eventbasedassessment_uniqueinteractions.sav"), to.data.frame =T, use.value.labels = F) 

# select data from second and third week of event-based assessment
app_unique <- filter(app_unique, day >= 59 & day <= 65 | day >= 107 & day <= 113)

# select self-rating variables of interest
app_SR <- select(app_unique, id_a, SR_dis_satisfiedwmyself, SR_good_bad_mood, SR_friendly_unfriendly)

# for each subject, aggregate his/her self-ratings across all interactions (in second and third week of event-based assessment)
app_SR_aggr <- aggregate(app_SR, by=list(app_SR$id_a), mean)

# delete grouping variable (= duplicate of id_p)
app_SR_aggr$Group.1 <- NULL


### Peer-ratings

# read app data in long format = one peer judgment per row
app_long <- read.spss(paste0(datadir,"12_connect_eventbasedassessment_long.sav"), to.data.frame =T, use.value.labels = F) 

# select data from second and third week of event-based assessment
app_long <- filter(app_long, day >= 59 & day <= 65 | day >= 107 & day <= 113)

# select peer-rating variables of interest
app_OR <- select(app_long, id_p, IR_positive_negative, OR_friendly_unfriendly)

# for each subject, aggregate his/her peer-ratings across all interactions where he/she was named as interaction partner
app_OR_aggr <- aggregate(app_OR, by=list(app_OR$id_p), mean)

# delete grouping variable (= duplicate of id_p)
app_OR_aggr$Group.1 <- NULL


### merge self-ratings and peer-ratings to one data frame
app <- merge(app_SR_aggr, app_OR_aggr, by.x="id_a", by.y="id_p", all.x=T, all.y=T)


###############################################################
### Read and prepare data from time-based assessments (diaries)
###############################################################

# read diary data including target effects and self-ratings
diary <- read.spss(paste0(datadir,"10_connect_timebasedassessment_effects.sav"), to.data.frame =T, use.value.labels = F) 

### aggregate diary data starting at the fourth week of aquaintance (starting at 27/10/2012)

# variables assessed in Diary A: Timepoints 10 to 23
knowing_df <- as.matrix(diary[ , c(paste0("t",10:23,"_knowing.p"))])
liking_df <- as.matrix(diary[ , c(paste0("t",10:23,"_liking.p"))])
liking_self_df <- as.matrix(diary[ , c(paste0("t",10:23,"_liking.s"))])

# variables assessed in Diary B: Timepoints 10, 12, 14, 16, 18, 20, 22, 23
warmhearted_df <- as.matrix(diary[ , c(paste0("t",c(10, 12, 14, 16, 18, 20, 22, 23),"_critical_warmhearted.p"))])
coldhearted_df <- as.matrix(diary[ , c(paste0("t",c(10, 12, 14, 16, 18, 20, 22, 23),"_affectionate_coldhearted.p"))])
unintelligent_df <- as.matrix(diary[ , c(paste0("t",c(10, 12, 14, 16, 18, 20, 22, 23),"_intelligent_unintelligent.p"))])
warmhearted_self_df <- as.matrix(diary[ , c(paste0("t",c(10, 12, 14, 16, 18, 20, 22, 23),"_critical_warmhearted.s"))])
coldhearted_self_df <- as.matrix(diary[ , c(paste0("t",c(10, 12, 14, 16, 18, 20, 22, 23),"_affectionate_coldhearted.s"))])

# variables assessed in Diary C: Timepoints 11, 13, 15, 17, 19, 21, 22, 23
leader_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_leader.p"))])
friend_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_friend.p"))])
relsatisfied_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_relsatisfied.p"))])
relimportant_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_relimportant.p"))])
conflict_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_conflict.p"))])
emosupport_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_emosupport.p"))])
acceptance_df <- as.matrix(diary[ , c(paste0("t",c(11, 13, 15, 17, 19, 21, 22, 23),"_acceptance.p"))])

diary <- diary %>%
          mutate(
                    knowing_t34 = rowMeans(knowing_df, na.rm=T),
                    liking_t34 = rowMeans(liking_df, na.rm=T),
                    liking_self_t34 = rowMeans(liking_self_df, na.rm=T),
                    warmhearted_t34 = rowMeans(warmhearted_df, na.rm=T),
                    coldhearted_t34 = rowMeans(coldhearted_df, na.rm=T),
                    unintelligent_t34 = rowMeans(unintelligent_df, na.rm=T),
                    warmhearted_self_t34 = rowMeans(warmhearted_self_df, na.rm=T),
                    coldhearted_self_t34 = rowMeans(coldhearted_self_df, na.rm=T),
                    leader_peer = rowMeans(leader_df, na.rm=T),
                    friend_peer = rowMeans(friend_df, na.rm=T),
                    relsatisfied_t34 = rowMeans(relsatisfied_df, na.rm=T),
                    relimportant_t34 = rowMeans(relimportant_df, na.rm=T),
                    conflict_t34 = rowMeans(conflict_df, na.rm=T),
                    emosupport_t34 = rowMeans(emosupport_df, na.rm=T),
                    acceptance_t34 = rowMeans(acceptance_df, na.rm=T)
          ) %>%
          select(id_a, knowing_t34:acceptance_t34)


#############################################
### Read and prepare data from online surveys
#############################################

surv1 <- read_sav(paste0(datadir,"03_connect_onlinesurvey_self_t1.sav"))
surv2 <- read_sav(paste0(datadir,"04_connect_onlinesurvey_self_t2.sav"))
surv3 <- read_sav(paste0(datadir,"05_connect_onlinesurvey_self_t3.sav"))
surv4 <- read_sav(paste0(datadir,"06_connect_onlinesurvey_self_t4.sav"))
surv5 <- read_sav(paste0(datadir,"07_connect_onlinesurvey_self_t5.sav"))

## merge survey data assessed at T2 to T5
surv23 <- merge(surv2,surv3, by="id_a", all.x=T, all.y=T)
surv234 <- merge(surv23,surv4, by="id_a", all.x=T, all.y=T)
surv2345 <- merge(surv234,surv5, by="id_a", all.x=T, all.y=T)

# recode abitur grades which were not provided but stored as 0, 
# and grades that were provided in units of the wrong grading system
surv2345$abitur_grade[surv2345$abitur_grade == 0] <- NA
surv2345$abitur_grade[surv2345$abitur_grade == 12] <- 2.0
surv2345$abitur_grade[surv2345$abitur_grade == 13] <- 1.7


## prepare exam grade variables

# The exam grade variables are coded as 0 when the subject did not provide any ratings. 
# They are coded as 12 when the respective exam had not been tried yet. 
# They are coded as 11 when the student failed the exam (for whatever reason, 
# some students even fail voluntarily to be able to repeat the exam). 
# Recode all these cases to NA.
gradevariables <- names(surv2345[, grep("retro_grades_", names(surv2345))])
surv2345[,gradevariables][surv2345[,gradevariables] == 0] <- NA
surv2345[,gradevariables][surv2345[,gradevariables] == 12] <- NA
surv2345[,gradevariables][surv2345[,gradevariables] == 11] <- NA

# Some persons provided more than one rating for one and the same exam (although they could only take the exam once),
# presumably because they did not remember that they already provided their grade. 
# For each exam and each person, we select the first grade they provided, because it is closest to the exam.
# (if all are NA, set the respective grade variable missing)

# define new variable for each of the 18 exams
surv2345[,paste0("retro_grades_",1:18)] <- NA

# define a function which, for each person, selects the first non-NA measurement he provided out of three time points (= out of three variables)
firstnonNA_3timepoints <- function(df) {if(all(is.na(df))){ 
                                        NA 
                                        } else if (!is.na(df[1])){
                                                  df[1]
                                        } else if (!is.na(df[2])){
                                                  df[2]
                                        } else if (!is.na(df[3])){
                                                  df[3]
                                        } 
}

# define a respective function for only two time points (= two variables)
firstnonNA_2timepoints <- function(df) {if(all(is.na(df))){ 
                                        NA 
                                        } else if (!is.na(df[1])){
                                                  df[1]
                                        } else if (!is.na(df[2])){
                                                  df[2]
                                        } 
}

# for the exam grades 1 to 6 which have been assessed at three occasions (Surveys 3, 4, 5), select first non-NA grade the student provided
surv2345$retro_grades_1 <- apply(surv2345[,paste0("retro_grades_1_t",3:5)], 1, firstnonNA_3timepoints)
surv2345$retro_grades_2 <- apply(surv2345[,paste0("retro_grades_2_t",3:5)], 1, firstnonNA_3timepoints)
surv2345$retro_grades_3 <- apply(surv2345[,paste0("retro_grades_3_t",3:5)], 1, firstnonNA_3timepoints)
surv2345$retro_grades_4 <- apply(surv2345[,paste0("retro_grades_4_t",3:5)], 1, firstnonNA_3timepoints)
surv2345$retro_grades_5 <- apply(surv2345[,paste0("retro_grades_5_t",3:5)], 1, firstnonNA_3timepoints)
surv2345$retro_grades_6 <- apply(surv2345[,paste0("retro_grades_6_t",3:5)], 1, firstnonNA_3timepoints)

# for the exam grades 7 to 14 which have been assessed at three occasions (Surveys 4, 5), select first non-NA grade the student provided
surv2345$retro_grades_7  <- apply(surv2345[,paste0("retro_grades_7_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_8  <- apply(surv2345[,paste0("retro_grades_8_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_9  <- apply(surv2345[,paste0("retro_grades_9_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_10 <- apply(surv2345[,paste0("retro_grades_10_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_11 <- apply(surv2345[,paste0("retro_grades_11_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_12 <- apply(surv2345[,paste0("retro_grades_12_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_13 <- apply(surv2345[,paste0("retro_grades_13_t",4:5)], 1, firstnonNA_2timepoints)
surv2345$retro_grades_14 <- apply(surv2345[,paste0("retro_grades_14_t",4:5)], 1, firstnonNA_2timepoints)

# for the exam grades 15 to 18 which have been assessed only at the last occasions (Survey 5), save the grade in new variable for consistency in notation
surv2345$retro_grades_15 <- surv2345$retro_grades_15_t5
surv2345$retro_grades_16 <- surv2345$retro_grades_16_t5
surv2345$retro_grades_17 <- surv2345$retro_grades_17_t5
surv2345$retro_grades_18 <- surv2345$retro_grades_18_t5


## Aggregate variables

# select outcomes to be aggregated across surveys at T2 to T5
se_df <- as.matrix(surv2345[ , c(paste0("rses_t",2:5))])
selfsat_df <- as.matrix(surv2345[ , c(paste0("traitaff_2_t",2:5))])
determined_df <- as.matrix(surv2345[ , c(paste0("traitaff_6_t",2:5))])
active_df <- as.matrix(surv2345[ , c(paste0("traitaff_1_t",2:5))])
goodmood_df <- as.matrix(surv2345[ , c(paste0("traitaff_8_t",2:5))])
opt_df <- as.matrix(surv2345[ , c(paste0("traitaff_3_t",2:5))])
leader_self_df <- as.matrix(surv2345[ , c(paste0("saq_5_t",2:5))])
trustabil_df <- as.matrix(surv2345[ , c(paste0("traitaff_5_t",2:5))])
assertive_df <- as.matrix(surv2345[ , c(paste0("saq_17_t",2:5))])
independent_df <- as.matrix(surv2345[ , c(paste0("saq_19_t",2:5))])
ambitious_df <- as.matrix(surv2345[ , c(paste0("saq_21_t",2:5))])
trust_self_df <- as.matrix(surv2345[ , c(paste0("saq_22_t",2:5))])

# select items for leadership/authority subscale of the NPI
leader_self_npi_df <- as.matrix(surv2345[ , c(paste0("npi_",c(1,11,12,30,33),"_t",2:5),paste0("npi_",c(7,10,32),"_t",2:5,"_r"))])

# select variables (all 18 exam grades) to be aggregated for mean exam grade
retro_grades_df <- as.matrix(surv2345[,paste0("retro_grades_",1:18)])

# compute aggregates
surv2345 <- surv2345 %>%
          mutate(
                    self_esteem = rowMeans(se_df, na.rm=T),
                    selfsat_p2 = rowMeans(selfsat_df, na.rm=T),
                    determined_p2 = rowMeans(determined_df, na.rm=T),
                    active_p2 = rowMeans(active_df, na.rm=T),
                    goodmood_p2 = rowMeans(goodmood_df, na.rm=T),
                    opt_p2 = rowMeans(opt_df, na.rm=T),
                    leader_self = rowMeans(leader_self_df, na.rm=T),
                    leader_self_npi_p2 = rowMeans(leader_self_npi_df, na.rm=T),
                    trustabil_p2 = rowMeans(trustabil_df, na.rm=T),
                    assertive_p2 = rowMeans(assertive_df, na.rm=T),
                    independent_p2 = rowMeans(independent_df, na.rm=T),
                    ambitious_p2 = rowMeans(ambitious_df, na.rm=T),
                    trust_self = rowMeans(trust_self_df, na.rm=T),
                    retro_grades = rowMeans(retro_grades_df, na.rm=T)
          ) %>%
          select(id_a, self_esteem:retro_grades, abitur_grade)


# select variables from T1
surv1 <- surv1 %>%
          mutate(
                    Raven_self = saq_14_t1,
                    MWTB_self = saq_13_t1
          ) %>%
          select(id_a, age_t1, sex_t1, Raven_self, MWTB_self)

# merge T1 variables to other survey variables
surv <- merge(surv2345, surv1, by="id_a", all.x=T, all.y=T)


#########################
### Merge all data frames
#########################

# merge data frames
merge1 <- merge(surv, abil, by="id_a", all.x=T, all.y=T)
merge2 <- merge(merge1, diary, by="id_a", all.x=T, all.y=T)
connect <- merge(merge2, app, by="id_a", all.x=T, all.y=T)

# sort data frame by random variable (to ensure anonymity in the final dataframe)
connect <- arrange(connect, coldhearted_self_t34)

# rename and select variables
connect <- connect %>%
          select(
                  age = age_t1, # age
                  sex = sex_t1, # sex
                  #
                  Raven_self = Raven_self, # self-rated reasoning ability
                  Raven_obj = raven_total, # objectively measured reasoning ability
                  #
                  MWTB_self = MWTB_self, # self-rated vocabulary knowledge
                  MWTB_obj = mwtb_total, # objectively measured vocabulary knowledge
                  #
                  # outcome category "Global self-evaluation"
                  self_esteem = self_esteem, # self-esteem
                  selfsat_survey = selfsat_p2, # satisfied with oneself (online survey)
                  selfsat_app = SR_dis_satisfiedwmyself, # satisfied with oneself (App)
                  liking_self = liking_self_t34, # self-rated likability (Diary)
                  #
                  # outcome category "Well-being"
                  determined = determined_p2, # determined
                  active = active_p2, # active
                  optimistic = opt_p2, # optimistic
                  good_mood = goodmood_p2, # in a good mood
                  bad_mood = SR_good_bad_mood, # in a bad mood
                  #
                  # outcome category "Self-rated agentic outcomes"
                  leader_SAQ_self = leader_self, # self-rated leadership ability (aggregate of surveys 2,3, and 4)
                  leader_authority_self = leader_self_npi_p2, # leadership/authority subscale of the NPI
                  trustabil_self = trustabil_p2, # trust in own abilities
                  assertive_self = assertive_p2, # assertive
                  independent_self = independent_p2, # independent
                  ambitious_self = ambitious_p2, # ambitious
                  #
                  # outcome category "Self-rated communal outcomes"
                  unfriendly_self = SR_friendly_unfriendly, # unfriendly behavior
                  trust_self = trust_self, # self-rated trustworthiness
                  warmhearted_self = warmhearted_self_t34, # warm-hearted
                  coldhearted_self = coldhearted_self_t34, # cold-hearted
                  #
                  # outcome category "Peer-rated agentic outcomes"
                  knowing_peer = knowing_t34, # I knew this person before today
                  leader_peer = leader_peer, # peer-rated leadership ability (target effects; aggregate of diaries after forth week)
                  unintelligent_peer = unintelligent_t34, # peer-rated "unintelligence"
                  #
                  # outcome category "Peer-rated communal outcomes"
                  rel_imp_peer = relimportant_t34, # relationship importance
                  rel_sat_peer = relsatisfied_t34, # relationship satisfaction
                  acceptance_peer = acceptance_t34, # acceptance
                  emosupp_peer = emosupport_t34, # emotional support
                  conflicts_peer = conflict_t34, # conflict potential
                  warmhearted_peer = warmhearted_t34, # warm-hearted
                  coldhearted_peer = coldhearted_t34, # cold-hearted
                  friend_peer = friend_peer, # peer-rated friendship potential (target effects; aggregate of diaries after forth week)
                  liking_peer = liking_t34, # likability
                  neg_interactions_peer = IR_positive_negative, # App target effects: negativity of interactions
                  unfriendly_peer = OR_friendly_unfriendly, # App target effects: unfriendly
                  #
                  # outcome category "Achievement"
                  abi_grade = abitur_grade, # grade in the Abitur, coded in German grading format (lower value = better grade)
                  study_grade = retro_grades # average grade in all exams during the studies until Survey 5, coded in German grading format (lower value = better grade)
          ) 


# Exclude subjects who have missing values on the ability test or on self-viewed ability
connect <- subset(connect,   
                      (Raven_self != "NA") 
                    & (Raven_obj != "NA") 
                    & (MWTB_self != "NA")
                    & (MWTB_obj != "NA")
                       )

# Save data frame for descriptive statistics analysis, before standardization
dir.create("Descriptives", showWarnings = FALSE)
connect_descr <- connect	
write.table(connect_descr, file="Descriptives/Data_Sample_C_connect_descr.txt",sep = "\t",col.names=TRUE)
	
# Add z-standardized variables
connect$Z_Raven_self = scale(connect$Raven_self)
connect$Z_Raven_obj = scale(connect$Raven_obj)
#
connect$Z_MWTB_self = scale(connect$MWTB_self)
connect$Z_MWTB_obj = scale(connect$MWTB_obj)
#
connect$Z_self_esteem = scale(connect$self_esteem)
connect$Z_selfsat_survey = scale(connect$selfsat_survey) 
connect$Z_selfsat_app = scale(connect$selfsat_app) 
connect$Z_liking_self = scale(connect$liking_self) 
#
# outcome category "Well-being"
connect$Z_determined = scale(connect$determined) 
connect$Z_active = scale(connect$active)
connect$Z_optimistic = scale(connect$optimistic) 
connect$Z_good_mood = scale(connect$good_mood) 
connect$Z_bad_mood = scale(connect$bad_mood)
#
# outcome category "Self-rated agentic outcomes"
connect$Z_leader_SAQ_self = scale(connect$leader_SAQ_self)
connect$Z_leader_authority_self = scale(connect$leader_authority_self)
connect$Z_trustabil_self = scale(connect$trustabil_self) 
connect$Z_assertive_self = scale(connect$assertive_self) 
connect$Z_independent_self = scale(connect$independent_self) 
connect$Z_ambitious_self = scale(connect$ambitious_self) 
#
# outcome category "Self-rated communal outcomes"
connect$Z_unfriendly_self = scale(connect$unfriendly_self)
connect$Z_trust_self = scale(connect$trust_self) 
connect$Z_warmhearted_self = scale(connect$warmhearted_self)
connect$Z_coldhearted_self = scale(connect$coldhearted_self)
#
# outcome category "Peer-rated agentic outcomes"
connect$Z_knowing_peer = scale(connect$knowing_peer) 
connect$Z_leader_peer = scale(connect$leader_peer) 
connect$Z_unintelligent_peer = scale(connect$unintelligent_peer) 
#
# outcome category "Peer-rated communal outcomes"
connect$Z_rel_imp_peer = scale(connect$rel_imp_peer)
connect$Z_rel_sat_peer = scale(connect$rel_sat_peer)
connect$Z_acceptance_peer = scale(connect$acceptance_peer) 
connect$Z_emosupp_peer = scale(connect$emosupp_peer) 
connect$Z_conflicts_peer = scale(connect$conflicts_peer) 
connect$Z_warmhearted_peer = scale(connect$warmhearted_peer)
connect$Z_coldhearted_peer = scale(connect$coldhearted_peer)
connect$Z_friend_peer = scale(connect$friend_peer)
connect$Z_liking_peer = scale(connect$liking_peer)
connect$Z_neg_interactions_peer = scale(connect$neg_interactions_peer) 
connect$Z_unfriendly_peer = scale(connect$unfriendly_peer)
#
# outcome category "Achievement"
connect$Z_abi_grade = scale(connect$abi_grade)
connect$Z_study_grade = scale(connect$study_grade)


# Aggregate outcome variables within the outcome categories
df_global_selfeval <- as.matrix(select(connect, Z_self_esteem, Z_selfsat_survey, Z_selfsat_app, Z_liking_self ))
#
df_well_being <- select(connect, Z_determined, Z_active, Z_optimistic, Z_good_mood, Z_bad_mood)
df_well_being$Z_bad_mood <- -df_well_being$Z_bad_mood
#
df_agency_self <- as.matrix(select(connect, 
                                   Z_leader_SAQ_self, Z_assertive_self, Z_independent_self, Z_ambitious_self,
                                   Z_leader_authority_self, Z_trustabil_self))
#
df_comm_self <- select(connect, Z_trust_self, Z_unfriendly_self, Z_warmhearted_self, Z_coldhearted_self )
df_comm_self$Z_unfriendly_self <- -df_comm_self$Z_unfriendly_self
df_comm_self$Z_coldhearted_self <- -df_comm_self$Z_coldhearted_self
#
df_agency_peer <- select(connect, Z_knowing_peer, Z_leader_peer, Z_unintelligent_peer) # Z_ninteractions
df_agency_peer$Z_unintelligent_peer <- -df_agency_peer$Z_unintelligent_peer
#
df_comm_peer <- select(connect, Z_liking_peer, Z_friend_peer,Z_rel_sat_peer, Z_rel_imp_peer, 
                       Z_conflicts_peer, Z_emosupp_peer,Z_acceptance_peer, Z_warmhearted_peer, Z_coldhearted_peer, 
                       Z_neg_interactions_peer, Z_unfriendly_peer)
df_comm_peer$Z_conflicts_peer <- -df_comm_peer$Z_conflicts_peer
df_comm_peer$Z_coldhearted_peer <- -df_comm_peer$Z_coldhearted_peer
df_comm_peer$Z_neg_interactions_peer <- -df_comm_peer$Z_neg_interactions_peer
df_comm_peer$Z_unfriendly_peer <- -df_comm_peer$Z_unfriendly_peer
#
df_achievement <- select(connect, Z_abi_grade, Z_study_grade)
df_achievement$Z_abi_grade <- -df_achievement$Z_abi_grade
df_achievement$Z_study_grade <- -df_achievement$Z_study_grade


connect$Z_global_selfeval = scale(rowMeans(df_global_selfeval, na.rm=T))
connect$Z_well_being = scale(rowMeans(df_well_being, na.rm=T))
connect$Z_agency_self = scale(rowMeans(df_agency_self, na.rm=T))
connect$Z_comm_self = scale(rowMeans(df_comm_self, na.rm=T))
connect$Z_agency_peer = scale(rowMeans(df_agency_peer, na.rm=T))
connect$Z_comm_peer = scale(rowMeans(df_comm_peer, na.rm=T))
connect$Z_achievement = scale(rowMeans(df_achievement, na.rm=T))

# Select variables for the analyses and save data frame that will be uploaded in the OSF
connect_osf <- select(connect, Z_Raven_self:Z_MWTB_obj, Z_global_selfeval:Z_achievement)
write.table(connect_osf, file="Data_Sample_C_connect.txt",sep = "\t",col.names=TRUE)


#########################
## DESCRIPTIVE STATISTICS
#########################

# compute and save sample statistics (age distribution, number of females)
age <- round(select(psych::describe(connect_descr$age), n, min, max, mean, sd),2)
age$n <- nrow(connect_descr)
sampstats <- mutate(age, 
                    female=plyr::count(connect_descr$sex)[plyr::count(connect_descr$sex)[,1]=="1",]["freq"]
)
write.table(sampstats, file="Descriptives/age_sex_Sample_C_connect.dat", sep="\t", row.names=FALSE)

# compute and save descriptives statistics of variables before aggregation and standardization
descriptives <- round(select(psych::describe(connect_descr), n, min, max, mean, sd),2)
write.table(descriptives, file="Descriptives/descriptives_Sample_C_connect.dat", sep="\t")

# compute and save correlation table of variables before aggregation and standardization
cor_raw <- corcons(connect_descr)
write.table(cor_raw, file="Descriptives/correlations_raw_Sample_C_connect.dat", sep="\t")

# create folder for descriptive statistics
dir.create("Descriptives", showWarnings = FALSE)

# compute and save internal concistencies of the outcome aggregates
a_global_selfeval <- round(psych::alpha(df_global_selfeval)$total$raw_alpha,2)
a_well_being <- round(psych::alpha(df_well_being)$total$raw_alpha,2)
a_agency_self <- round(psych::alpha(df_agency_self)$total$raw_alpha,2)
a_comm_self <- round(psych::alpha(df_comm_self)$total$raw_alpha,2)
a_agency_peer <- round(psych::alpha(df_agency_peer)$total$raw_alpha,2)
a_comm_peer <- round(psych::alpha(df_comm_peer)$total$raw_alpha,2)
a_achievement <- round(psych::alpha(df_achievement)$total$raw_alpha,2)

alpha_connect <- data.frame(alpha_global_selfeval = a_global_selfeval,
                         alpha_well_being = a_well_being,
                         alpha_agency_self = a_agency_self,
                         alpha_comm_self = a_comm_self,
                         alpha_agency_peer = a_agency_peer,
                         alpha_comm_peer = a_comm_peer,
                         alpha_achievement = a_achievement
)
write.table(alpha_connect, file="Descriptives/alpha_Sample_C_connect.dat", sep="\t", row.names=FALSE)

# compute and save correlation table of self-rated and objective ability measures and outcome aggregates, 
varnames <- c("Raven","reasoning_self","MWTB","vocabulary_self","global_selfevaluation","well_being","agency_self","communion_self","agency_peer","communion_peer","achievement")
outcomes_connect <- dplyr::select(connect, Z_Raven_obj, Z_Raven_self, Z_MWTB_obj, Z_MWTB_self, Z_global_selfeval, Z_well_being, Z_agency_self, Z_comm_self, Z_agency_peer, Z_comm_peer, Z_achievement)
names(outcomes_connect) <- varnames
cor_aggr <- corcons(outcomes_connect)
write.table(cor_aggr, file="Descriptives/correlations_aggr_Sample_C_connect.dat", sep="\t")




 