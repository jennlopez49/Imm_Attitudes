### Set up Models--- Vars 

cmps_lat_16$variables$ICI_Reverse_Fac <- as.factor(cmps_lat_16$variables$ICI_Reverse)
dvs <- c("")  # List of DVs (Y)
ivs <- list("ICI_Reverse", "ICI_Reverse_Fac")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Latino_Disc", "Spanish_Media", "Worry_Deport", 
              "Inclusion_External", "Inclusion_Internal")

### Models --- 
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

