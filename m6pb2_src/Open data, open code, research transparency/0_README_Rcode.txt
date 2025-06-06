This file contains descriptions of the data and R code files uploaded in the OSF project "Is Accurate, Positive, or Inflated Self-Perception Most Advantageous for Psychological Adjustment? A Competitive Test of Key Hypotheses".

Data analysis for the manuscript was seperated into two steps:

######################
## 1. Data Preparation
######################

The procedure of data preparation can be retraced in the data preparation scripts "Data_preparation_Sample_A.R" to "Data_preparation_Sample_E.R". These scripts are based on the primary data files of Samples A to E, which can be requested in the following ways if reproducing the data preparation is of interest: 

(A) The raw data from Sample A (Study 2 by Dufner et al., 2012) can be requested from the authors of the manuscript (please use the contact information provided in the author note). 
(B) The raw data from Sample B (PILS study by Geukes et al., 2017) can be requested at osf.io/q5zwp by use of the Reproduction Request formula. 
(C) The raw data from Sample C (Connect study by Geukes et al., 2017) can be requested at osf.io/2pmcr by use of the Reproduction Request formula. 
(D) The raw data from Sample D (Study 1 by Dufner et al., 2012) can be requested from the authors of the manuscript (please use the contact information provided in the author note). 
(E) The raw data from Sample E (Self-Insight study by Dufner et al., 2015) can be requested from the authors of the manuscript (please use the contact information provided in the author note). 

With the data preparation scripts, the raw data files were prepared for our main analyses in the following way: 

(a) merging data frames from various sources within one study
(b) aggregation of outcome variables across time points of data assessment
(c) exclusion of subjects with missing values on the self-rated or objectively assessed ability measures
(d) z-standardization of all considered variables
(e) aggregation of outcome variables within the six outcome categories
(f) computation of descriptive statistics and internal consistencies that are reported in the manuscript or in Additional Material 3.

The resulting data frames after preparation were saved as "Data_Sample_A_mst2.txt" to "Data_Sample_E_mSI.txt", ready to be used in the main analyses. These data files are provided in the OSF. 



###################
## 2. Main Analyses
###################

The main analysis reported in the manuscript is the evaluation of the competing models in integrated data from Samples A to E. The R code for this analysis can be found in the file "Main_File_Model_Evaluation.R", which additionally sources some functions defined in the file "helpers.R". The main analyses contain the following steps: 

(a) integration of Sample A to Sample E into one single data frame, including dummy variables that indicate the respective origin sample
(b) definition of a function (see "helpers.R") that identifies and removes outliers according to Bollen and Jackman (1980), tests the full polynomial model for significance, estimates all models in a pre-defined initial model set, and prepares respective AICc tables and further output objects that can be used for interpretation
(c) definition of initial model set for our analyses
(d) application of the model evaluation function, for reasoning ability and each outcome category separately, then for vocabulary knowledge and each outcome category, creation of result tables to be shown in the manuscript and in Additional Material 1, and plotting of models to be shown in the manuscript.




################################
## Literature cited in this file
################################

Dufner, M., Arslan, R. C., Hagemeyer, B., Schönbrodt, F. D., & Denissen, J. J. A. (2015). Affective contingencies in the affiliative domain: Physiological assessment, associations with the affiliation motive, and prediction of behavior. Journal of Personality and Social Psychology, 109(4), 662–676. doi:http://dx.doi.org/10.1037/pspp0000025

Dufner, M., Denissen, J. J. A., van Zalk, M., Matthes, B., Meeus, W. H. J., van Aken, M. A. G., & Sedikides, C. (2012). Positive intelligence illusions: On the relation between intellectual self-enhancement and psychological adjustment. Journal of Personality, 80(3), 537–572. doi:10.1111/j.1467-6494.2011.00742.x

Geukes, K., Breil, S. M., Hutteman, R., Küfner, A. C. P., Nestler, S., & Back, M. D. (2017). Explaining the longitudinal interplay of personality and social relationships in the laboratory and in the field: The PILS and CONNECT study. Manuscript submitted for publication.
