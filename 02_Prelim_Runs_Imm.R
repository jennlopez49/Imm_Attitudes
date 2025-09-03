### Set up Models--- Vars 

dvs <- c("BorderSecurity", "Pathway_Deport", "Mex_Deport")  # List of DVs (Y)
ivs <- list("conc_lat_index_16","latino_conc_16") # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", 
              # "Education",
              # "Income", "Pol_Interest", 
              # "Mexican", "Cuban",
              # "Linked_Fate", 
              "Party",
              "More_Than_SecondGen" 
              # "Discrimination_Scale", 
              # "Latino_Disc", "Spanish_Media", "Worry_Deport", 
              # "Inclusion_External", "Inclusion_Internal"
              )

## simple models - 1-10 BorderSec, 11-20 Pathway, 21-30 Mex Deport

mediation_function_standard(dvs, ivs, mediators, controls, 
                            cmps_lat_16, cmps_lat_16, out = "imm.mods")

bordersec <- imm.mods$outcome_models[1:10]
stargazer((imm.mods$outcome_models[1:10]), covariate.labels = c("Imm. Stigma",
                                                              "Imm. Stigma Index",
                                                              "Age", "Gender", 
                                                            "Party",
                                                            "More_Than_SecondGen"),
          dep.var.labels = c("Tighten Border Security"), type = "text"
          
) 

stargazer(imm.mods$outcome_models[1:10], 
          covariate.labels = c("Imm. Stigma Index","Imm. Stigma", "Fear",
          "Angry", "Pride", "Hope", "Sad", "Age", "Gender","Party",
          "Second Gen +", "Constant"),
          dep.var.labels = c("Tighten Border Security"),
          type = "latex", out = "bordersec_simple.tex"
) 

stargazer(imm.mods$outcome_models[11:20], 
          covariate.labels = c("Imm. Stigma Index","Imm. Stigma", "Fear",
                               "Angry", "Pride", "Hope", "Sad", "Age", "Gender","Party",
                               "Second Gen +", "Constant"),
          dep.var.labels = c("Pathway to Citizenship vs Deportation"),
          type = "latex", out = "pathway_simple.tex"
) 

stargazer(imm.mods$outcome_models[21:30], 
          covariate.labels = c("Imm. Stigma Index","Imm. Stigma", "Fear",
                               "Angry", "Pride", "Hope", "Sad", "Age", "Gender","Party",
                               "Second Gen +", "Constant"),
          dep.var.labels = c("Pathway to Citizenship vs Deportation, vers. 2"),
          type = "latex", out = "pathway_vers2_simple.tex"
) 

### Models with other vars --- 
# BORDER SECURITY

mediation_function_standard("BorderSecurity", ivs, mediators, controls, 
                            cmps_lat_16, cmps_lat_16, out = "bordersec")

### DEPORTATION
controls_pathway <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Latino_Disc", "Spanish_Media", "Worry_Deport", 
              "Inclusion_External", "Inclusion_Internal", "SplitPathway")

mediation_function_standard("Pathway_Deport", ivs, mediators, controls_pathway, 
                            cmps_lat_16, cmps_lat_16, out = "deport_path")

## DEPORT MEXICANS

controls_mex <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican", "Cuban",
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale", 
                      "Latino_Disc", "Spanish_Media", "Worry_Deport", 
                      "Inclusion_External", "Inclusion_Internal", "MexSplit")

mediation_function_standard("Mex_Deport", ivs, mediators, controls_mex, 
                            cmps_lat_16, cmps_lat_16, out = "deport_mex")

### PATHWAY TO CITIZENSHIP


mediation_function_standard("Pathway_Citizenship", ivs, mediators, controls, 
                            cmps_lat_16, cmps_lat_16, out = "pathway")

