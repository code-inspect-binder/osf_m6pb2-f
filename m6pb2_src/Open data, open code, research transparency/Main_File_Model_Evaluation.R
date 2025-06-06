## This code is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## 
## For a copy of the GNU General Public License, 
## see <http://www.gnu.org/licenses/>.

## (c) 2018 Sarah Humberg. 

## This source code accompanies the following paper:
## Humberg, S., Dufner, M., Schönbrodt, F. D., Geukes, K., Hutteman, R., Küfner, A. C. P., van Zalk, M. H. W., Denissen, J. J. A., Nestler, S., & Back, M. D. (2018). Is accurate, positive, or inflated self-perception most advantageous for psychological adjustment? A competitive test of key hypotheses. Retrieved from osf.io/9w3bh


########################################
### LOAD REQUIRED PACKAGES AND SCRIPTS
########################################

library(lavaan) # estimation of regression models; version 0.5-20 was used for our analyses
library(AICcmodavg) # computation of AICc values and Akaike weights; version 2.0-4
library(dplyr) # easy data manipulation; version 0.5.0
library(RSA) # plotting of response surfaces; version 0.9.10


##############################
### PREPARE DATA
##############################

# data preparation scripts for each sample
# source("Data_preparation_Sample_A.R")
# source("Data_preparation_Sample_B.R")
# source("Data_preparation_Sample_C.R")
# source("Data_preparation_Sample_D.R")
# source("Data_preparation_Sample_E.R")

# read data
mst2 <- read.table("Data_Sample_A_mst2.txt", header=TRUE, sep = "\t")
pils <- read.table("Data_Sample_B_pils.txt", header=TRUE, sep = "\t")
connect <- read.table("Data_Sample_C_connect.txt", header=TRUE, sep = "\t")
mst1 <- read.table("Data_Sample_D_mst1.txt", header=TRUE, sep = "\t")
mSI <- read.table("Data_Sample_E_mSI.txt", header=TRUE, sep = "\t")

# add dummy variables that encode samples
mst2[,c("Dummy_A","Dummy_B","Dummy_C","Dummy_D","Dummy_E")] <- c(rep(1,nrow(mst2)), rep(0,nrow(mst2)), rep(0,nrow(mst2)), rep(0,nrow(mst2)), rep(0,nrow(mst2)))
pils[,c("Dummy_A","Dummy_B","Dummy_C","Dummy_D","Dummy_E")] <- c(rep(0,nrow(pils)), rep(1,nrow(pils)), rep(0,nrow(pils)), rep(0,nrow(pils)), rep(0,nrow(pils)))
connect[,c("Dummy_A","Dummy_B","Dummy_C","Dummy_D","Dummy_E")] <- c(rep(0,nrow(connect)), rep(0,nrow(connect)), rep(1,nrow(connect)), rep(0,nrow(connect)), rep(0,nrow(connect)))
mst1[,c("Dummy_A","Dummy_B","Dummy_C","Dummy_D","Dummy_E")] <- c(rep(0,nrow(mst1)), rep(0,nrow(mst1)), rep(0,nrow(mst1)), rep(1,nrow(mst1)), rep(0,nrow(mst1)))
mSI[,c("Dummy_A","Dummy_B","Dummy_C","Dummy_D","Dummy_E")] <- c(rep(0,nrow(mSI)), rep(0,nrow(mSI)), rep(0,nrow(mSI)), rep(0,nrow(mSI)), rep(1,nrow(mSI)))

# add variable Z_comm_self as missings to Sample A, as this outcome category was not assessed in this sample (Sample A will be excluded in the analyses for this outcome category)
mst2$Z_comm_self <- NA

# similar for Z_achievement, where it was not assessed
pils$Z_achievement <- NA
mst1$Z_achievement <- NA

# similar for vocabulary knowledge and reasoning ability, where it was not assessed
mst2$Z_MWTB_self <- NA
mst2$Z_MWTB_obj <- NA

mst1$Z_Raven_self <- NA
mst1$Z_Raven_obj <- NA

mSI$Z_Raven_self <- NA
mSI$Z_Raven_obj <- NA

# bind data of all samples containing self-rated and objectively assessed reasoning ability to one large data frame
variables <- c("Z_Raven_self", "Z_Raven_obj", "Z_MWTB_self", "Z_MWTB_obj", "Z_global_selfeval", "Z_well_being", "Z_agency_self", "Z_comm_self", "Z_agency_peer", "Z_comm_peer", "Z_achievement", "Dummy_A", "Dummy_B", "Dummy_C", "Dummy_D", "Dummy_E")
df <- rbind(mst2[,variables], pils[,variables], connect[,variables], mst1[,variables], mSI[,variables])

# add squared and interaction terms
df$Z_Raven_self2 <- df$Z_Raven_self*df$Z_Raven_self
df$Z_Raven_self_Z_Raven_obj <- df$Z_Raven_self*df$Z_Raven_obj
df$Z_Raven_obj2 <- df$Z_Raven_obj*df$Z_Raven_obj

df$Z_MWTB_self2 <- df$Z_MWTB_self*df$Z_MWTB_self
df$Z_MWTB_self_Z_MWTB_obj <- df$Z_MWTB_self*df$Z_MWTB_obj
df$Z_MWTB_obj2 <- df$Z_MWTB_obj*df$Z_MWTB_obj


##############################
### PRE-ANALYSES
##############################

# check for discrepant predictors according to Shanock et al. (2010)
diff_Raven <- df$Z_Raven_obj - df$Z_Raven_self
cong_Raven <- cut(diff_Raven, breaks=c(-Inf, -.5, .5, Inf), labels=c(paste0("Z_Raven_obj", " < ", "Z_Raven_self"), "Congruence", paste0("Z_Raven_obj", " > ", "Z_Raven_self")))
print(round(prop.table(table(cong_Raven)), 2)*100)

diff_MWTB <- df$Z_MWTB_obj - df$Z_MWTB_self
cong_MWTB <- cut(diff_MWTB, breaks=c(-Inf, -.5, .5, Inf), labels=c(paste0("Z_MWTB_obj", " < ", "Z_MWTB_self"), "Congruence", paste0("Z_MWTB_obj", " > ", "Z_MWTB_self")))
print(round(prop.table(table(cong_MWTB)), 2)*100)

# check for multicollinearity
cor(df[,c("Z_Raven_obj","Z_Raven_self")], use="complete.obs")
cor(df[,c("Z_MWTB_obj","Z_MWTB_self")], use="complete.obs")


##############################
### PREPARE MAIN ANALYSES
##############################

# load function eam which estimates all models in our initial model set and prepares AICc table
source("helpers.R")

# define initial model set
initial_modelset <- c(
          "onlyxpos", # only positive PSV effect model
          "xandypos", # positive main (PSV) effect model
          "onlyxneg", # only negative PSV effect model
          "xandyneg", # negative main (PSV) effect model
          "discrpos", # positive discrepancy (SE) effect model
          "discrneg", # negative discrepancy (SE) effect model
          "SQDpos", # agreement (self-insight) effect model
          "SSQDposCneg", # optimal margin effect model
          "onlyypos", # positive R effect model
          "onlyx2pos", # curvilinear effect of S model
          "onlyy2pos", # curvilinear effect of R model
          "IApos", # positive interaction effect model
          "null", # null model
          "full" # full polynomial model of second degree
)

# create folder for result objects
dir.create("Result_objects_eam", showWarnings = FALSE)


######################################
### RUN ANALYSES 
######################################

######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: GLOBAL SELF-EVALUATION
######################################

# define variables for this analysis
var <- c("Z_global_selfeval","Z_Raven_self","Z_Raven_obj")

# estimate models
Raven_global_selfeval <- eam(
                              variables=var, 
                              controlvariables=" + Dummy_B + Dummy_C", 
                              modelset=initial_modelset, 
                              data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
                              )

# save result object for initial modelset
save(Raven_global_selfeval, file = "Result_objects_eam/object_initialset_Raven_global_selfeval.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "discrpos", 
          "onlyxneg", 
          "xandyneg", 
          "discrneg", 
          "onlyy2pos", 
          "full", 
          "xandypos", 
          "IApos", 
          "onlyypos" 
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]

# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- Raven_global_selfeval$aiccoeftab[,-which(names(Raven_global_selfeval$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"

# Repeat analyses with reduced modelset
Raven_global_selfeval <- eam(
                              variables=var, 
                              controlvariables=" + Dummy_B + Dummy_C", 
                              modelset=reduced_modelset, 
                              data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
                              )

# save result object for reduced modelset
save(Raven_global_selfeval, file = "Result_objects_eam/object_reducedset_Raven_global_selfeval.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- Raven_global_selfeval$aiccoeftab[Raven_global_selfeval$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", Raven_global_selfeval$filename,".dat"), sep="\t", row.names=FALSE)


# build table with confidence set models to display in the manuscript
Raven_confset_table_variables <- c("nicenames","w",paste0("b",1:5))
Raven_confset_table <- Raven_global_selfeval$aiccoeftab[1,Raven_confset_table_variables]
Raven_confset_table[1,] <- c("Global self-evaluation", rep(NA,ncol(Raven_confset_table)-1))
Raven_confset_table <- rbind(Raven_confset_table, Raven_global_selfeval$aiccoeftab[Raven_global_selfeval$aiccoeftab$confset==1,Raven_confset_table_variables])


# show number of outliers that were removed in the analysis
length(Raven_global_selfeval$outliers)

# show R^2 of full model and its p-value
Raven_global_selfeval$r2_full
Raven_global_selfeval$p_full

# plot models of interest
plotmodel(Raven_global_selfeval, model="onlyx2pos", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Global self-evaluation")

# How many percent of the values of S lie "left" of the vertex of onlyx2pos?
Raven_global_selfeval$coeftab[Raven_global_selfeval$coeftab$Modnames == "onlyx2pos", "left.of.V"]






######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: WELL-BEING
######################################

var <- c("Z_well_being","Z_Raven_self","Z_Raven_obj")

Raven_well_being <- eam(
                    variables=var, 
                    controlvariables=" + Dummy_B + Dummy_C", 
                    modelset=initial_modelset, 
                    data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
                    )

# save result object for initial modelset
save(Raven_well_being, file = "Result_objects_eam/object_initialset_Raven_well_being.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "onlyxneg",    
          "xandyneg",    
          "onlyy2pos",   
          "discrneg",     
          "xandypos",	
          "discrpos",	
          "IApos",	
          "onlyypos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]

# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- Raven_well_being$aiccoeftab[,-which(names(Raven_well_being$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"

# Repeat analyses with reduced modelset
Raven_well_being <- eam(
          variables=var,
          controlvariables=" + Dummy_B + Dummy_C",
          modelset=reduced_modelset,
          data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
)

# save result object for reduced modelset
save(Raven_well_being, file = "Result_objects_eam/object_reducedset_Raven_well_being.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- Raven_well_being$aiccoeftab[Raven_well_being$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", Raven_well_being$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
Raven_confset_table <- rbind(Raven_confset_table, c("Well-being", rep(NA,ncol(Raven_confset_table)-1)))
Raven_confset_table <- rbind(Raven_confset_table, Raven_well_being$aiccoeftab[Raven_well_being$aiccoeftab$confset==1,Raven_confset_table_variables])

# show number of outliers that were removed in the analysis
length(Raven_well_being$outliers)

# show R^2 of full model and its p-value
Raven_well_being$r2_full
Raven_well_being$p_full

# plot models of interest
plotmodel(Raven_well_being, model="onlyx2pos", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Well-being")
plotmodel(Raven_well_being, model="full", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Well-being")

# How many percent of the values of S lie "left" of the vertex of onlyx2pos?
Raven_well_being$coeftab[Raven_well_being$coeftab$Modnames == "onlyx2pos", "left.of.V"]




######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: SELF-RATED AGENTIC OUTCOMES
######################################

var <- c("Z_agency_self","Z_Raven_self","Z_Raven_obj")

Raven_agency_self <- eam(
                              variables=var, 
                              controlvariables=" + Dummy_B + Dummy_C", 
                              modelset=initial_modelset, 
                              data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
                              )

# save result object for initial modelset
save(Raven_agency_self, file = "Result_objects_eam/object_initialset_Raven_agency_self.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "discrpos", 
          "onlyxneg", 
          "xandyneg", 
          "discrneg", 
          "onlyx2pos",	
          "xandypos",	
          "IApos",	
          "full",	
          "onlyypos",	
          "onlyy2pos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]

# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- Raven_agency_self$aiccoeftab[,-which(names(Raven_agency_self$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
Raven_agency_self <- eam(
          variables=var, 
          controlvariables=" + Dummy_B + Dummy_C", 
          modelset=reduced_modelset, 
          data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
)

# save result object for reduced modelset
save(Raven_agency_self, file = "Result_objects_eam/object_reducedset_Raven_agency_self.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- Raven_agency_self$aiccoeftab[Raven_agency_self$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", Raven_agency_self$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
Raven_confset_table <- rbind(Raven_confset_table, c("Self-rated agentic outcomes", rep(NA,ncol(Raven_confset_table)-1)))
Raven_confset_table <- rbind(Raven_confset_table, Raven_agency_self$aiccoeftab[Raven_agency_self$aiccoeftab$confset==1,Raven_confset_table_variables])


# show number of outliers that were removed in the analysis
length(Raven_agency_self$outliers)

# show R^2 of full model and its p-value
Raven_agency_self$r2_full
Raven_agency_self$p_full

# plot models of interest
plotmodel(Raven_agency_self, model="onlyxpos", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Agency (self)")



######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: SELF-RATED COMMUNAL OUTCOMES
### (outcome category not available in Sample A)
######################################

var <- c("Z_comm_self","Z_Raven_self","Z_Raven_obj") 

Raven_comm_self <- eam(variables=var, 
                             controlvariables=" + Dummy_B + Dummy_C", 
                             modelset=initial_modelset, 
                             data=filter(df, Dummy_B==1|Dummy_C==1)
                       )


# extend table with confidence set models to display in the manuscript
Raven_confset_table <- rbind(Raven_confset_table, c("Self-rated communal outcomes - full model not significant", rep(NA,ncol(Raven_confset_table)-1)))

# full model is not significant



######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: PEER-RATED AGENTIC OUTCOMES
######################################

var <- c("Z_agency_peer","Z_Raven_self","Z_Raven_obj")

Raven_agency_peer <- eam(
                              variables=var, 
                              controlvariables=" + Dummy_B + Dummy_C", 
                              modelset=initial_modelset, 
                              data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
                              )

# save result object for initial modelset
save(Raven_agency_peer, file = "Result_objects_eam/object_initialset_Raven_agency_peer.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "IApos",      
          "discrneg",   
          "discrpos",   
          "SQDpos",
          "SSQDposCneg", 
          "onlyxneg",   
          "xandyneg",   
          "onlyy2pos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]

# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- Raven_agency_peer$aiccoeftab[,-which(names(Raven_agency_peer$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
Raven_agency_peer <- eam(
          variables=var, 
          controlvariables=" + Dummy_B + Dummy_C", 
          modelset=reduced_modelset, 
          data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1)
          )

# save result object for reduced modelset
save(Raven_agency_peer, file = "Result_objects_eam/object_reducedset_Raven_agency_peer.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- Raven_agency_peer$aiccoeftab[Raven_agency_peer$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", Raven_agency_peer$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
Raven_confset_table <- rbind(Raven_confset_table, c("Peer-rated agentic outcomes", rep(NA,ncol(Raven_confset_table)-1)))
Raven_confset_table <- rbind(Raven_confset_table, Raven_agency_peer$aiccoeftab[Raven_agency_peer$aiccoeftab$confset==1,Raven_confset_table_variables])

# show number of outliers that were removed in the analysis
length(Raven_agency_peer$outliers)

# show R^2 of full model and its p-value
Raven_agency_peer$r2_full
Raven_agency_peer$p_full

# plot models of interest
plotmodel(Raven_agency_peer, model="xandypos", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Agency (peer)")
plotmodel(Raven_agency_peer, model="full", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Agency (peer)")




######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: PEER-RATED COMMUNAL OUTCOMES
######################################

var <- c("Z_comm_peer","Z_Raven_self","Z_Raven_obj")

Raven_comm_peer <- eam(variables=var,
                       controlvariables=" + Dummy_B + Dummy_C",
                       modelset=initial_modelset,
                       data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1))

# save result object for initial modelset
save(Raven_comm_peer, file = "Result_objects_eam/object_initialset_Raven_comm_peer.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "IApos",     
          "xandypos",  
          "onlyxpos",  
          "discrpos",  
          "xandyneg",  
          "SQDpos",
          "SSQDposCneg",
          "onlyy2pos",	
          "full",	
          "onlyxneg"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]

# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- Raven_comm_peer$aiccoeftab[,-which(names(Raven_comm_peer$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
Raven_comm_peer <- eam(variables=var,
                       controlvariables=" + Dummy_B + Dummy_C",
                       modelset=reduced_modelset,
                       data=filter(df, Dummy_A==1|Dummy_B==1|Dummy_C==1))

# save result object for reduced modelset
save(Raven_comm_peer, file = "Result_objects_eam/object_reducedset_Raven_comm_peer.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- Raven_comm_peer$aiccoeftab[Raven_comm_peer$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", Raven_comm_peer$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
Raven_confset_table <- rbind(Raven_confset_table, c("Peer-rated communal outcomes", rep(NA,ncol(Raven_confset_table)-1)))
Raven_confset_table <- rbind(Raven_confset_table, Raven_comm_peer$aiccoeftab[Raven_comm_peer$aiccoeftab$confset==1,Raven_confset_table_variables])

# show number of outliers that were removed in the analysis
length(Raven_comm_peer$outliers)

# show R^2 of full model and its p-value
Raven_comm_peer$r2_full
Raven_comm_peer$p_full

# plot models of interest
plotmodel(Raven_comm_peer, model="discrneg", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Communion (peer)")
plotmodel(Raven_comm_peer, model="onlyypos", xlab="Reasoning (self)", ylab="Reasoning (obj.)", zlab="Communion (peer)")



######################################
### DOMAIN: REASONING ABILITY
### OUTCOME: ACHIEVEMENT
### (outcome category not available in Sample B)
######################################

var <- c("Z_achievement","Z_Raven_self","Z_Raven_obj")

Raven_achievement <- eam(variables=var, 
                       controlvariables=" + Dummy_C", 
                       modelset=initial_modelset, 
                       data=filter(df, Dummy_A==1|Dummy_C==1)) 

# full model is not significant


######################################
######################################

# save table with confidence set models to display in the manuscript
write.table(Raven_confset_table, file="Result_tables/Raven_confset_table.dat", sep="\t", row.names=FALSE)

######################################
######################################



######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: GLOBAL SELF-EVALUATION
######################################

var <- c("Z_global_selfeval","Z_MWTB_self","Z_MWTB_obj")

MWTB_global_selfeval <- eam(
                              variables=var,
                              controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                              modelset=initial_modelset,
                              data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                              )

# save result object for initial modelset
save(MWTB_global_selfeval, file = "Result_objects_eam/object_initialset_MWTB_global_selfeval.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "xandypos",  
          "discrneg",  
          "onlyy2pos", 
          "onlyxneg",  
          "xandyneg",  
          "IApos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]


# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- MWTB_global_selfeval$aiccoeftab[,-which(names(MWTB_global_selfeval$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"

# Repeat analyses with reduced modelset
MWTB_global_selfeval <- eam(
                              variables=var,
                              controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                              modelset=reduced_modelset,
                              data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                              )

# save result object for reduced modelset
save(MWTB_global_selfeval, file = "Result_objects_eam/object_reducedset_MWTB_global_selfeval.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- MWTB_global_selfeval$aiccoeftab[MWTB_global_selfeval$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", MWTB_global_selfeval$filename,".dat"), sep="\t", row.names=FALSE)


# build table with confidence set models to display in the manuscript
MWTB_confset_table_variables <- c("nicenames","w",paste0("b",1:5))
MWTB_confset_table <- MWTB_global_selfeval$aiccoeftab[1,MWTB_confset_table_variables]
MWTB_confset_table[1,] <- c("Global self-evaluation", rep(NA,ncol(MWTB_confset_table)-1))
MWTB_confset_table <- rbind(MWTB_confset_table, MWTB_global_selfeval$aiccoeftab[MWTB_global_selfeval$aiccoeftab$confset==1,MWTB_confset_table_variables])


# show number of outliers that were removed in the analysis
length(MWTB_global_selfeval$outliers)

# show R^2 of full model and its p-value
MWTB_global_selfeval$r2_full
MWTB_global_selfeval$p_full

# plot models of interest
plotmodel(MWTB_global_selfeval, model="full", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Global self-evaluation")
plotmodel(MWTB_global_selfeval, model="onlyx2pos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Global self-evaluation")
plotmodel(MWTB_global_selfeval, model="discrpos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Global self-evaluation")

# How many percent of the values of S lie "left" of the vertex of onlyx2pos?
MWTB_global_selfeval$coeftab[MWTB_global_selfeval$coeftab$Modnames == "onlyx2pos", "left.of.V"]


######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: WELL-BEING
######################################

var <- c("Z_well_being","Z_MWTB_self","Z_MWTB_obj")

MWTB_well_being <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=initial_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for initial modelset
save(MWTB_well_being, file = "Result_objects_eam/object_initialset_MWTB_well_being.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "IApos",    
          "xandypos", 
          "discrneg", 
          "xandyneg", 
          "onlyypos",	
          "onlyxneg",	
          "onlyy2pos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]



# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- MWTB_well_being$aiccoeftab[,-which(names(MWTB_well_being$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
MWTB_well_being <- eam(
          variables=var,
          controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
          modelset=reduced_modelset,
          data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
)

# save result object for reduced modelset
save(MWTB_well_being, file = "Result_objects_eam/object_reducedset_MWTB_well_being.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- MWTB_well_being$aiccoeftab[MWTB_well_being$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", MWTB_well_being$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
MWTB_confset_table <- rbind(MWTB_confset_table, c("Well-being", rep(NA,ncol(MWTB_confset_table)-1)))
MWTB_confset_table <- rbind(MWTB_confset_table, MWTB_well_being$aiccoeftab[MWTB_well_being$aiccoeftab$confset==1,MWTB_confset_table_variables])

# show number of outliers that were removed in the analysis
length(MWTB_well_being$outliers)

# show R^2 of full model and its p-value
MWTB_well_being$r2_full
MWTB_well_being$p_full

# plot models of interest
plotmodel(MWTB_well_being, model="onlyx2pos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Well-being")
plotmodel(MWTB_well_being, model="full", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Well-being")


# How many percent of the values of S lie "left" of the vertex of onlyx2pos?
MWTB_well_being$coeftab[MWTB_well_being$coeftab$Modnames == "onlyx2pos", "left.of.V"]


######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: SELF-RATED AGENTIC OUTCOMES
######################################

var <- c("Z_agency_self","Z_MWTB_self","Z_MWTB_obj")

MWTB_agency_self <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=initial_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for initial modelset
save(MWTB_agency_self, file = "Result_objects_eam/object_initialset_MWTB_agency_self.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "onlyx2pos", 
          "xandypos",  
          "discrneg",  
          "IApos",	
          "full",	
          "onlyypos",	
          "onlyxneg"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]


# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- MWTB_agency_self$aiccoeftab[,-which(names(MWTB_agency_self$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
MWTB_agency_self <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=reduced_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for reduced modelset
save(MWTB_agency_self, file = "Result_objects_eam/object_reducedset_MWTB_agency_self.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- MWTB_agency_self$aiccoeftab[MWTB_agency_self$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", MWTB_agency_self$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
MWTB_confset_table <- rbind(MWTB_confset_table, c("Self-rated agentic outcomes", rep(NA,ncol(MWTB_confset_table)-1)))
MWTB_confset_table <- rbind(MWTB_confset_table, MWTB_agency_self$aiccoeftab[MWTB_agency_self$aiccoeftab$confset==1,MWTB_confset_table_variables])

# show number of outliers that were removed in the analysis
length(MWTB_agency_self$outliers)

# show R^2 of full model and its p-value
MWTB_agency_self$r2_full
MWTB_agency_self$p_full

# plot models of interest
plotmodel(MWTB_agency_self, model="discrpos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Agency (self)")





######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: SELF-RATED COMMUNAL OUTCOMES
######################################

var <- c("Z_comm_self","Z_MWTB_self","Z_MWTB_obj")

MWTB_comm_self <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=initial_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for initial modelset
save(MWTB_comm_self, file = "Result_objects_eam/object_initialset_MWTB_comm_self.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "SSQDposCneg", # remove because b3 = b4 = b5 = 0 in this model
          "SQDpos",
          "onlyx2pos",  
          "onlyxneg",   
          "onlyypos",   
          "discrneg",   
          "IApos",	
          "xandypos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]


# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- MWTB_comm_self$aiccoeftab[,-which(names(MWTB_comm_self$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"

# Repeat analyses with reduced modelset
MWTB_comm_self <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=reduced_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for reduced modelset
save(MWTB_comm_self, file = "Result_objects_eam/object_reducedset_MWTB_comm_self.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- MWTB_comm_self$aiccoeftab[MWTB_comm_self$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", MWTB_comm_self$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
MWTB_confset_table <- rbind(MWTB_confset_table, c("Self-rated communal outcomes", rep(NA,ncol(MWTB_confset_table)-1)))
MWTB_confset_table <- rbind(MWTB_confset_table, MWTB_comm_self$aiccoeftab[MWTB_comm_self$aiccoeftab$confset==1,MWTB_confset_table_variables])


# show number of outliers that were removed in the analysis
length(MWTB_comm_self$outliers)

# show R^2 of full model and its p-value
MWTB_comm_self$r2_full
MWTB_comm_self$p_full

# plot models of interest
plotmodel(MWTB_comm_self, model="discrpos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Communion (self)")
plotmodel(MWTB_comm_self, model="full", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Communion (self)")



######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: PEER-RATED AGENTIC OUTCOMES
######################################

var <- c("Z_agency_peer","Z_MWTB_self","Z_MWTB_obj")

MWTB_agency_peer <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=initial_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for initial modelset
save(MWTB_agency_peer, file = "Result_objects_eam/object_initialset_MWTB_agency_peer.Rdata")

# reduce modelset by models which have essentially the same log-likelihood as simpler models nested within them
remove_models <- c(
          "xandypos", 
          "onlyxneg", 
          "xandyneg", 
          "discrneg", 
          "onlyy2pos",
          "discrpos",	
          "IApos",	
          "SQDpos",	
          "full",	
          "onlyypos"	
)

reduced_modelset <- initial_modelset[which(!initial_modelset %in% remove_models)]



# Start to create results table including removed models, to be shown in the additional materials 
aiccoeftab <- MWTB_agency_peer$aiccoeftab[,-which(names(MWTB_agency_peer$aiccoeftab) %in% c("position", "Cum.Wt", "confset", "R2", "abs", "Shift.C", "Vertex", "left.of.V", "left.of.ridge"))]
aiccoeftab$w <- "(-)"


# Repeat analyses with reduced modelset
MWTB_agency_peer <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=reduced_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# save result object for reduced modelset
save(MWTB_agency_peer, file = "Result_objects_eam/object_reducedset_MWTB_agency_peer.Rdata")

# Finish and save results table including removed models, to be shown in the additional materials
for (model in reduced_modelset){
          aiccoeftab[aiccoeftab$Modnames==model,"w"] <- MWTB_agency_peer$aiccoeftab[MWTB_agency_peer$aiccoeftab$Modnames==model, "w"]
}
aiccoeftab <- aiccoeftab[,-which(names(aiccoeftab)=="Modnames")]
write.table(aiccoeftab, file=paste0("Result_tables/aiccoeftab_initialset_", MWTB_agency_peer$filename,".dat"), sep="\t", row.names=FALSE)


# extend table with confidence set models to display in the manuscript
MWTB_confset_table <- rbind(MWTB_confset_table, c("Peer-rated agentic outcomes", rep(NA,ncol(MWTB_confset_table)-1)))
MWTB_confset_table <- rbind(MWTB_confset_table, MWTB_agency_peer$aiccoeftab[MWTB_agency_peer$aiccoeftab$confset==1,MWTB_confset_table_variables])

# show number of outliers that were removed in the analysis
length(MWTB_agency_peer$outliers)

# show R^2 of full model and its p-value
MWTB_agency_peer$r2_full
MWTB_agency_peer$p_full

# plot models of interest
plotmodel(MWTB_agency_peer, model="onlyx2pos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Agency (peer)")
plotmodel(MWTB_agency_peer, model="onlyxpos", xlab="Vocabulary (self)", ylab="Vocabulary (obj.)", zlab="Agency (peer)")

# How many percent of the values of S lie "left" of the vertex of onlyx2pos?
MWTB_agency_peer$coeftab[MWTB_agency_peer$coeftab$Modnames == "onlyx2pos", "left.of.V"]



######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: PEER-RATED COMMUNAL OUTCOMES
######################################

var <- c("Z_comm_peer","Z_MWTB_self","Z_MWTB_obj")

MWTB_comm_peer <- eam(
                    variables=var,
                    controlvariables=" + Dummy_C + Dummy_D + Dummy_E",
                    modelset=initial_modelset,
                    data=filter(df, Dummy_B==1|Dummy_C==1|Dummy_D==1|Dummy_E==1)
                    )

# extend table with confidence set models to display in the manuscript
MWTB_confset_table <- rbind(MWTB_confset_table, c("Peer-rated communal outcomes - full model not significant", rep(NA,ncol(MWTB_confset_table)-1)))

# full model not significant



######################################
### DOMAIN: VOCABULARY KNOWLEDGE
### OUTCOME: ACHIEVEMENT
### (outcome category not available in Samples B and D)
######################################

var <- c("Z_achievement","Z_MWTB_self","Z_MWTB_obj")

MWTB_achievement <- eam(
                    variables=var, 
                    controlvariables=" + Dummy_C + Dummy_E",
                    modelset=initial_modelset, 
                    data=filter(df, Dummy_C==1|Dummy_E==1)
                    )

# full model not significant


######################################
######################################

# save table with confidence set models to display in the manuscript
write.table(MWTB_confset_table, file="Result_tables/MWTB_confset_table.dat", sep="\t", row.names=FALSE)
