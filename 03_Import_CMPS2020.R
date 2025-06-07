### Importing CMPS 2020 
cmps2020 <- read_dta("/Users/jenniferlopez/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/CMPS 2020 full adult sample weighted STATA.dta",
                     encoding = "UTF-8")

### Subsetting to only relevant variables ######
cmps.sub.2020  <- cmps2020 %>% dplyr::select(S1, S2_Racer2,S2_Race_Prime, 
                                             S2_Hispanicr2,  
                                S2_Hispanicr3,  S2_Hispanicr4, S2_Hispanicr5, 
                                S3b, S4, S5, S5_Age, S7, S10, S10_Mex, S13,
                                Q12, Q21, Q22, Q23, Q43, Q44, Q45, Q46, Q77r2,
                                Q77r3, Q77r4, Q77r5, Q77r6, Q77r7, Q77r8, Q77r9,
                                Q99r3, Q117r11, Q131r3, Q131r4, Q131r5,
                                Q159r1, Q159r2, Q159r3, Q182, Q183, Q184r1, 
                                Q184r2, Q187r1, Q208r1, Q208r2, Q208r3, Q208r4,
                                Q208r5, Q213r1, Q217r1, Q217r2, Q217r3, Q217r4,
                                Q261, Q411_Q416r1, Q411_Q416r2, Q411_Q416r3,
                                Q411_Q416r4, Q411_Q416r5, Q411_Q416r6, 
                                Q467_Q469r1, Q478_Q483r4, Q490r1, Q490r2,
                                Q490r3, Q490r4, Q490r5, Q490r6, Q490r7, Q490r8,
                                Q491r1, Q491r2, Q491r3, Q491r4, Q491r5, Q491r6,
                                Q491r7, Q493, Q492, Q504r8, Q550r1, Q550r2, 
                                Q550r3, Q550r4, Q550r5, Q550r6, Q551_Q559r2, 
                                Q619_Q626r5, Q619_Q626r6, Q630_Q632r1, Q630_Q632r3,
                                Q629r1, Q627, Q628, Q629r5, Q629r7, Q709_712r1,
                                Q709_712r2, Q709_712r3, Q713, Q715_718r1, 
                                Q715_718r2, Q715_718r3, Q715_718r4, Q809, Q812,
                                Q816, weight, splitQ716
)


### Cleaning --------------------------------------------------------------------
cmps2020.clean <- cmps.sub.2020 %>% mutate(
 Survey.Lang = as.factor(S1),                                                   ## Language of Survey
 Hispanic = as.numeric(S2_Racer2),                                              ## Self identify as Hisp for sample 
 Race = as.factor(S2_Race_Prime),                                               ## Primary race/ethnicity 
 Self_Hisp = case_when(S2_Hispanicr2 == 2 ~ 1,                                  ## Consider themselves Hisp 
                       S2_Hispanicr2 == 0 ~ 0,
                       TRUE ~ NA),
 Parents_Hisp = case_when(S2_Hispanicr2 == 3 ~ 1,                               ## Consider parents Hisp 
                          S2_Hispanicr2 == 0 ~ 0,
                          TRUE ~ NA),
 GrParents_Hisp = case_when(S2_Hispanicr2 == 4 ~ 1,                             ## Consider grandparents Hisp 
           S2_Hispanicr2 == 0 ~ 0,
           TRUE ~ NA),
 DisRel_Hisp = case_when(S2_Hispanicr2 == 5 ~ 1,                                ## Consider distant relatives Hisp 
                         S2_Hispanicr2 == 0 ~ 0,
                         TRUE ~ NA),
 Gender = S3b,                                                                  ## Man - 1, Woman - 2, Nonbin - 3, Other - 4 
 State = S4,
 Birthyear = S5,
 Age_Category = as.factor(S5_Age),
 Age = 2025 - S5,
 Native = case_when(S7 == 1 ~ 1, 
                    S7 ==  2 ~ 0,
                    S7 == 3 ~ 0.5), 
 Country_Origin = S10,
 Cuban = ifelse(cmps.sub.2020$S10 == 6, 1, 0),                                  # Cuban
 Mexican = ifelse(cmps.sub.2020$S10 == 12, 1, 0),                               # Mexican --- other S10_Mex doesn't 
 Education = S13,                                                               # Numeric - 1-7, 1 - Grade school (up to 8th) and 7 - Post-grad
 Voted = case_when(Q12 == 1 ~ 1,                                                # Voted - recoded so ---> higher numbers, higher confidence of having voted
                   Q12 == 2 ~ .5, 
                   Q12 == 3 ~ -.5,
                   Q12 == 4 ~ 0), 
 PartyID_3 = case_when(Q21 == 1 ~ 0,                                            ## 0 - Rep, 1 - Independent/Other, 2 - Dem
                       Q21 == 3 ~ 1,
                       Q21 == 4 ~ 1, 
                       Q21 == 2 ~ 2), 
 PartyStrength = Q22,                                                           ## 1 - Strong, 2 - Not so strong
 Ind_Closer = Q23,                                                              ## 1 - Rep, 2 - Dem, 3 - Ind, 4 - Other party 
 Ideology = ifelse(cmps.sub.2020$Q43 == 6, NA_real_,                                         ## 1 - Very Lib, 5 - Very cons, 6 - none of these - recoded to NA
                   cmps.sub.2020$Q43),                                                               
 Trust_Government = case_when(Q44 == 4 ~ 0,                                      ## Trust in Govt in Washington to do right thing - Recoded so 0 - never, 3 - always
                             Q44 == 3 ~ 1,
                             Q44 == 2 ~ 2,
                             Q44 == 1 ~ 3), 
 Pol_System_Needs = case_when(Q45 == 4 ~ 0,                                      ## Pol System Helps Ppl with their needs - Recoded so 0 - strongly disagree, 3 - strongly agree
                              Q45 == 3 ~ 1,
                              Q45 == 2 ~ 2,
                              Q45 == 1 ~ 3), 
 Ext_Pol_Eff_1 = Q46,                                                           ## "The Pol System does not listen to ppl like me." -- left as-is, 1 - strongly agree, 4 - strongly disagree
 Contributed_CampMoney = case_when(Q77r2 == 1 ~ 5,                              ## Recoded - 5 - I did that, 1 - No, and don't want to. 
                                   Q77r2 == 2 ~ 4,                              ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                   Q77r2 == 3 ~ 1,
                                   Q77r2 == 4 ~ 2,
                                   Q77r2 == 5 ~ 3),
 Wore_CampaignButton = case_when(Q77r3 == 1 ~ 5,                                ## Recoded - 5 - I did that, 1 - No, and don't want to.
                                 Q77r3 == 2 ~ 4,                                ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                 Q77r3 == 3 ~ 1,
                                 Q77r3 == 4 ~ 2,
                                 Q77r3 == 5 ~ 3),
 Contacted_Rep =  case_when(Q77r4 == 1 ~ 5,                                     ## Recoded - 5 - I did that, 1 - No, and don't want to.
                            Q77r4 == 2 ~ 4,                                      ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                            Q77r4 == 3 ~ 1,
                            Q77r4 == 4 ~ 2,
                            Q77r4 == 5 ~ 3),
 Contacted_Means =  case_when(Q77r5 == 1 ~ 5,                                   ## Recoded - 5 - I did that, 1 - No, and don't want to.
                              Q77r5 == 2 ~ 4,                                   ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                              Q77r5 == 3 ~ 1,                                   # unclear what this is asking ---> just says "Contacted in any way, such as by letter, telephone, internet, or in perso" on all docs
                              Q77r5 == 4 ~ 2,
                              Q77r5 == 5 ~ 3),
 Cooperated_Others = case_when(Q77r6 == 1 ~ 5,                                  ## Recoded - 5 - I did that, 1 - No, and don't want to.
                               Q77r6 == 2 ~ 4,                                  ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                               Q77r6 == 3 ~ 1,                                   
                               Q77r6 == 4 ~ 2,
                               Q77r6 == 5 ~ 3),
 Discussed_Pol_Online = case_when(Q77r7 == 1 ~ 5,                               ## Recoded - 5 - I did that, 1 - No, and don't want to.
                                  Q77r7 == 2 ~ 4,                               ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                  Q77r7 == 3 ~ 1,                                 
                                  Q77r7 == 4 ~ 2,
                                  Q77r7 == 5 ~ 3),, 
 Signed_Petition = case_when(Q77r8 == 1 ~ 5,                                    ## Recoded - 5 - I did that, 1 - No, and don't want to.
                             Q77r8 == 2 ~ 4,                                    ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                             Q77r8 == 3 ~ 1,                                   
                             Q77r8 == 4 ~ 2,
                             Q77r8 == 5 ~ 3), 
 Boycott_Pol = case_when(Q77r9 == 1 ~ 5,                                        ## Recoded - 5 - I did that, 1 - No, and don't want to.
                         Q77r9 == 2 ~ 4,                                        ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                         Q77r9 == 3 ~ 1,                                      
                         Q77r9 == 4 ~ 2,
                         Q77r9 == 5 ~ 3),
 Effective_Protests_Imm = case_when(Q77r5 == 1 ~ 5,                             ## Recoded - 5 - Very effective, 1 - very ineffective.
                                    Q77r5 == 2 ~ 4,                             
                                    Q77r5 == 3 ~ 3,                                   
                                    Q77r5 == 4 ~ 2,
                                    Q77r5 == 5 ~ 1),
 Priority_ImmRights = Q117r11, 
 Family_Separation = case_when(Q131r3 == 1 ~ 5,                                 ## Recoded - 5 - str support, 1 - str oppose
                               Q131r3 == 2 ~ 4,                                  
                               Q131r3 == 3 ~ 3,                                   
                               Q131r3 == 4 ~ 2,
                               Q131r3 == 5 ~ 1), 
 Pathway_Citizenship = case_when(Q131r4 == 1 ~ 5,                               ## Recoded - 5 - str support, 1 - str oppose
                                 Q131r4 == 2 ~ 4,                                  
                                 Q131r4 == 3 ~ 3,                                   
                                 Q131r4 == 4 ~ 2,
                                 Q131r4 == 5 ~ 1), 
 EssentialWorkers_Citizenship = case_when(Q131r5 == 1 ~ 5,                      ## Recoded - 5 - str support, 1 - str oppose
                                          Q131r5 == 2 ~ 4,                                  
                                          Q131r5 == 3 ~ 3,                                   
                                          Q131r5 == 4 ~ 2,
                                          Q131r5 == 5 ~ 1),
 Government_Helps_Latinos = Q159r1,                                             # Never - All the time (1-5)
 Latinos_Say = Q159r2, 
 ICE_Accountable = case_when(Q182 == 1 ~ 1,                                     ## Who should be held accountable for hiring of undoc workers?
                             Q182 == 3 ~ 2,                                     ## Switched order so it goes from employer --> both --> only undocumented immigrants
                             Q182 == 2 ~ 3), 
 ICE_Opinion = Q183, 
 BorderSecurity = case_when(Q184r1 == 5 ~ 1,                                    ## BorderSec as a Nat'l Priority - recoded, 1 - strongly disagree - 5 - strongly agree
                            Q184r1 == 4 ~ 2,
                            Q184r1 == 3 ~ 3,
                            Q184r1 == 2 ~ 4,
                            Q184r1 == 1 ~ 5), 
 Immigration_Easier = case_when(Q184r1 == 5 ~ 1,                                ## Easier to get visa + citizenship - recoded, 1 - strongly disagree - 5 - strongly agree
                                Q184r1 == 4 ~ 2,
                                Q184r1 == 3 ~ 3,
                                Q184r1 == 2 ~ 4,
                                Q184r1 == 1 ~ 5),  
 Birthright_Undocumented = Q187r1,                                              # Kept as is - 1 - Strongly disagree about removing automatic birthright, 5 - strongly agree
 Individualism1 = Q208r1, 
 Individualism2 = Q208r2, 
 Individualism3 = Q208r3,
 Individualism4 = Q208r4,
 Individualism5 = Q208r5, 
 Resentment1 = case_when(Q213r1 == 5 ~ 1,                                       ## Irish & Italian, others should do the same --> 1) agree, 5) disagree
                         Q213r1 == 4 ~ 2, 
                         Q213r1 == 3 ~ 3,
                         Q213r1 == 2 ~ 4,
                         Q213r1 == 1 ~ 5),
 RaceDenial1 = case_when(Q217r1 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - White ppl don't have advantages
                       Q217r1 == 4 ~ 2, 
                       Q217r1 == 3 ~ 3,
                       Q217r1 == 2 ~ 4,
                       Q217r1 == 1 ~ 5),  
 RaceDenial2 = case_when(Q217r2 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - Race problems are rare isolated events
                        Q217r2 == 4 ~ 2, 
                        Q217r2 == 3 ~ 3,
                        Q217r2 == 2 ~ 4,
                        Q217r2 == 1 ~ 5), 
 RaceDenial3 = Q217r3,                                                        ## kept asis, 1-strongly agree, 5-strongly disagree - I am angry that racism exists
 RaceDenial4 = case_when(Q217r4 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - I am sometimes fearful of people of other races
                         Q217r4 == 4 ~ 2, 
                         Q217r4 == 3 ~ 3,
                         Q217r4 == 2 ~ 4,
                         Q217r4 == 1 ~ 5), 
 Skintone = Q261, 
 Little_Interest = Q411_Q416r1,                                                 ## Mental Health 1 - little interest / pleasure in doing things over past 2 weeks
 Down_Depressed = Q411_Q416r2,                                                  ## Mental Health 2 - Feeling down, depressed, or hopeless over past 2 weeks
 Nervous_OnEdge = Q411_Q416r3,                                                  ## Mental Health 3 - Feeling nervous, anxious, or on edge over past 2 weeks
 Afraid = Q411_Q416r4,                                                          ## Mental Health 4 - Feeling afraid as if something awful might happen over past 2 weeks
 Cannot_Relax = Q411_Q416r5,                                                    ## Mental Health 5 - Can't relax unless they know what will happen the next day
 Uncertainty_Stressed = Q411_Q416r6,                                            ## Mental Health 6 - Uncertainty makes them stressed / anxious / uneasy 
 Helped_Someone_SeekImm = case_when(Q467_Q469r1 == 1 ~ 1,                       ## Helped someone seek immigration-related information or services (1 - Yes, 0 - No [recoded])
                                    Q467_Q469r1 == 2 ~ 0),  
 Feel_FullCitizen =  case_when(Q478_Q483r4 == 7 ~ 1,                            ## recoded, 1-strongly disagree, 7-strongly agree - I feel like a full and equal citizen in this country with all the rights & protections others have
                               Q478_Q483r4 == 6 ~ 2, 
                               Q478_Q483r4 == 5 ~ 3,
                               Q478_Q483r4 == 4 ~ 4,
                               Q478_Q483r4 == 3 ~ 5,
                               Q478_Q483r4 == 2 ~ 6,
                               Q478_Q483r4 == 1 ~ 7), 
 BeenStopped_Questioned = Q490r1,
 BeenStopped_Questioned_Family = Q490r2,
 BeenDetained_Deported_Family = Q490r3,
 BeenStopped_Questioned_OtherRel = Q490r4,
 BeenDetained_Deported_OtherRel = Q490r5, 
 BeenStopped_Questioned_FriendCoworker = Q490r6, 
 BeenDetained_Deported_FriendCoworker = Q490r7, 
 No_DoNotKnowAnyone = Q490r8,
 DoNotKnowUndoc = Q491r1,                                                       # Questions over whether they know someone undocumented & their relation (0 - no, 1 - yes)
 Parent_Undoc = Q491r2, 
 Sibling_Undoc = Q491r3, 
 Children_Undoc = Q491r4,
 OtherFam_Undoc = Q491r5, 
 Friend_Undoc = Q491r6,
 Coworker_Undoc = Q491r7, 
 Worried_DetainedDeported = Q493,                                               ### about themselves -- worry over being detained /deported 
 Worried_DetainedDeported_SomeoneElse = Q492,                                   ### about ppl they know -- worry over being detained /deported 
 Anxiety_Stress_ImmPolicy = Q504r8,                                             ## HOw much anxiety / stress has Imm Policy caused you - 1 - least 10 - most
 KnowLatinoImm_Partner = Q550r1, 
 KnowLatinoImm_GPParents = Q550r2, 
 KnowLatinoImm_Children = Q550r3, 
 KnowLatinoImm_CloseFriend = Q550r4, 
 KnowLatinoImm_DoNot = Q550r5, 
 KnowLatinoImm_Unsure = Q550r6, 
 Linked_Fate = Q551_Q559r2,                                                     # 1 - nothing, 5 - a huge amt 
 Immigrant_Disc = case_when(Q619_Q626r5 == 1 ~ 4,                               # How much disc against immigrants (1 - none at all, 4 - a lot [recoded 5 "don't know" as NA])
                            Q619_Q626r5 == 2 ~ 3,
                            Q619_Q626r5 == 3 ~ 2,
                            Q619_Q626r5 == 4 ~ 1,
                            Q619_Q626r5 == 5 ~ NA_real_),
 Latinos_Disc = case_when(Q619_Q626r6 == 1 ~ 4,                                 # How much disc against Latinos (1 - none at all, 4 - a lot [recoded 5 "don't know" as NA])
                          Q619_Q626r6 == 2 ~ 3,
                          Q619_Q626r6 == 3 ~ 2,
                          Q619_Q626r6 == 4 ~ 1,
                          Q619_Q626r6 == 5 ~ NA_real_),
 Assume_DontSpeakEnglish = case_when(Q630_Q632r1 == 1 ~ 1,                      # Ppl act as if you don't speak English (recoded, 1 - yes, 0 - no)
                                     Q630_Q632r1 == 2 ~ 0), 
 Assume_NotAmerican = case_when(Q630_Q632r3 == 1 ~ 1,                           # Ppl assume as if you are not American (recoded, 1 - yes, 0 - no)
                                Q630_Q632r3 == 2 ~ 0), 
 Disc_Racial_Ethnic = Q629r1, 
 Discriminated = case_when(Q627 == 1 ~ 1,                                       # Recoded 1 - Yes, 0 - No
                           Q627 == 2 ~ 0), 
 Disc_Where = Q628,                                                             # Where 1 - US, 2 -Country of Origin, 3 - In both
 Disc_Imm_Status = Q629r5, 
 Disc_Imm_Accent = Q629r7, 
 Race_Denial5 = Q709_712r1,                                                     # White privilege is a major problem in the US today (1 - strongly agree, 4 - Strongly disagree)
 Race_Denial6 = case_when(Q709_712r2 == 1 ~ 4,                                  # Discrimination against Whites has been a big a problem as against others (recoded, 1- strongly disagree, 4 - strongly agree)
                          Q709_712r2 == 2 ~ 3,
                          Q709_712r2 == 3 ~ 2,
                          Q709_712r2 == 4 ~ 1),                                                     
 Race_Denial7 = case_when(Q709_712r3 == 1 ~ 4,                                  # Am. Way of Life needs to be protected from foreign influence (recoded, 1- strongly disagree, 4 - strongly agree)
                         Q709_712r3 == 2 ~ 3,
                         Q709_712r3 == 3 ~ 2,
                         Q709_712r3 == 4 ~ 1),    
 Part_ImmComm = case_when(Q713 == 1 ~ 4,                                        # Part of Imm Community (recoded, 1- never, 4 - all the time)
                          Q713 == 2 ~ 3,
                          Q713 == 3 ~ 2,
                          Q713 == 4 ~ 1),    , 
 Belong_USSociety = case_when(Q715_718r1 == 1 ~ 3,                              # Believe they belong in US Society (recoded, 1 - not at all, 3 - a lot)
                              Q715_718r1 == 2 ~ 2,
                              Q715_718r1 == 3 ~ 1), 
 InsOutsider_USSociety = Q715_718r2,
 Split_Terms_Ins = splitQ716,
 Outsider_USSociety = ifelse(cmps.sub.2020$splitQ716 == 1,                # Outsider in US Society (recoded, 1 (not at all) - 3 (a lot)), flipped the scale for those shown "outsider" (---> higher numbers, higher feelings of being excluded) 
                             cmps.sub.2020$Q715_718r2,
                             5 - cmps.sub.2020$Q715_718r2),
 Value_RespectYou = Q715_718r3,                                                 # how much do those in the US value and respect you? 1 - a lot, 3 - not at all.    
 Excluded_US = Q715_718r4,                                                      # how much do those in the US accept and include you? 1 - a lot, 3 - not at all.    
 Parents_Born = case_when(Q809 == 2 ~ 1,                                        # Recoded - 1 - both outside the US, 2 - 1/1, 3 - Both in PR, 4 - both in US, 88 - IDK
                          Q809 == 4 ~ 2,
                          Q809 == 3 ~ 3,
                          Q809 == 1 ~ 4, 
                          Q809 == 88 ~ NA_real_),
 Grandparents_Born = case_when(Q812 == 5 ~ 1,                                        # Recoded - 1 - 4 outside the US, 2 - 3/1, 3 - 2/2, 4 - 1/3, 5 - all 4 in the US, 88 - NA
                               Q812 == 4 ~ 2,
                               Q812 == 3 ~ 3,
                               Q812 == 2 ~ 4, 
                               Q812 == 1 ~ 5,
                               Q812 == 88 ~ NA_real_),
 Spanish_Use = case_when(Q816 == 5 ~ 1,                                        # Recoded - 1 - never, 5 - very often 
                         Q816 == 4 ~ 2,
                         Q816 == 3 ~ 3,
                         Q816 == 2 ~ 4, 
                         Q816 == 1 ~ 5)
 )

