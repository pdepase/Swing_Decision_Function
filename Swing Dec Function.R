
Swing_Decision_function <- function(data_set){
  library(tidyverse)
  library(mgcv)
  library(broom)
  
  #Import all necessary models for the function
  MLB_Called_Strike_Bam<-readRDS("MLB_Called_Strike_Bam.rds")
  MLB_Swing_Bam<-readRDS("MLB_Swing_Bam.rds")
  MLB_Singles_Bam<-readRDS("MLB_Singles_Bam.rds")
  MLB_Doubles_Bam<-readRDS("MLB_Doubles_Bam.rds")
  MLB_Triples_Bam<-readRDS("MLB_Triples_Bam.rds")
  MLB_HR_Bam<-readRDS("MLB_HR_Bam.rds")
  MLB_Out_Bam<-readRDS("MLB_Out_Bam.rds")
  
  
  ## Remove all incomplete data##
  data_set<- data_set[!is.na(data_set$plate_x), ]
  data_set<- data_set[!is.na(data_set$plate_z), ]
  data_set<- data_set[!is.na(data_set$pfx_x), ]
  data_set<- data_set[!is.na(data_set$pfx_z), ]
  data_set<- data_set[!is.na(data_set$balls), ]
  data_set<- data_set[!is.na(data_set$strikes), ]
  
  #Find Called Strike Probability#
  data_set <- MLB_Called_Strike_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Called_Strike_Prob"
  
  
  
  #Find Swing Probability
  data_set <- MLB_Swing_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Swing_Prob"
  
  
  #Find chance of a single#
  data_set <- MLB_Singles_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Single_Prob"
  
  
  #Find Chance of a double#
  data_set <- MLB_Doubles_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Double_Prob"
  
  
  #Find chance of a triple#
  data_set <- MLB_Triple_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Triple_Prob"
  
  
  #Find chance of a home run#
  data_set <- MLB_HR_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "HR_Prob"
  
  
  #Find chance of an out#
  data_set <- MLB_Out_Bam %>% augment(type.predict="response", newdata=data_set )
  
  data_set <- data_set %>% select(!.se.fit)
  
  names(data_set)[names(data_set) == ".fitted"] <- "Out_Prob"
  
  
  
  #Create a column to represent where the runners are#
  data_set$Runners <- ifelse(is.na(data_set$on_1b)==TRUE & is.na(data_set$on_2b)==TRUE & is.na(data_set$on_3b)==TRUE,"000",
                                             ifelse(is.na(data_set$on_1b)==FALSE & is.na(data_set$on_2b)==TRUE & is.na(data_set$on_3b)==TRUE,"100",
                                                    ifelse(is.na(data_set$on_1b)==TRUE & is.na(data_set$on_2b)==FALSE & is.na(data_set$on_3b)==TRUE,"010",
                                                           ifelse(is.na(data_set$on_1b)==TRUE & is.na(data_set$on_2b)==TRUE & is.na(data_set$on_3b)==FALSE,"001",
                                                                  ifelse(is.na(data_set$on_1b)==FALSE & is.na(data_set$on_2b)==FALSE & is.na(data_set$on_3b)==TRUE,"110",
                                                                         ifelse(is.na(data_set$on_1b)==FALSE & is.na(data_set$on_2b)==TRUE & is.na(data_set$on_3b)==FALSE,"101",
                                                                                ifelse(is.na(data_set$on_1b)==TRUE & is.na(data_set$on_2b)==FALSE & is.na(data_set$on_3b)==FALSE,"011",
                                                                                       ifelse(is.na(data_set$on_1b)==FALSE & is.na(data_set$on_2b)==FALSE & is.na(data_set$on_3b)==FALSE,"111","ERROR"))))))))
  
  
  #Determine the Run Value of an out#
  data_set$RV_Out <- ifelse(data_set$Runners=="000" & data_set$outs_when_up=="0",-0.25,
                                            ifelse(data_set$Runners=="000" & data_set$outs_when_up=="1",-0.18,
                                                   ifelse(data_set$Runners=="000" & data_set$outs_when_up=="2",-0.11,
                                                          ifelse(data_set$Runners=="100" & data_set$outs_when_up=="0",-0.38,
                                                                 ifelse(data_set$Runners=="100" & data_set$outs_when_up=="1",-0.33,
                                                                        ifelse(data_set$Runners=="100" & data_set$outs_when_up=="2",-0.24,
                                                                               ifelse(data_set$Runners=="010" & data_set$outs_when_up=="0",-0.44,
                                                                                      ifelse(data_set$Runners=="010" & data_set$outs_when_up=="1",-0.39,
                                                                                             ifelse(data_set$Runners=="010" & data_set$outs_when_up=="2",-0.34,
                                                                                                    ifelse(data_set$Runners=="001" & data_set$outs_when_up=="0",-0.42,
                                                                                                           ifelse(data_set$Runners=="001" & data_set$outs_when_up=="1",-0.62,
                                                                                                                  ifelse(data_set$Runners=="001" & data_set$outs_when_up=="2",-0.38,
                                                                                                                         ifelse(data_set$Runners=="110" & data_set$outs_when_up=="0",-0.55,
                                                                                                                                ifelse(data_set$Runners=="110" & data_set$outs_when_up=="1",-0.54,
                                                                                                                                       ifelse(data_set$Runners=="110" & data_set$outs_when_up=="2",-0.46,
                                                                                                                                              ifelse(data_set$Runners=="101" & data_set$outs_when_up=="0",-0.55,
                                                                                                                                                     ifelse(data_set$Runners=="101" & data_set$outs_when_up=="1",-0.69,
                                                                                                                                                            ifelse(data_set$Runners=="101" & data_set$outs_when_up=="2",-0.55,
                                                                                                                                                                   ifelse(data_set$Runners=="011" & data_set$outs_when_up=="0",-0.61,
                                                                                                                                                                          ifelse(data_set$Runners=="011" & data_set$outs_when_up=="1",-0.82,
                                                                                                                                                                                 ifelse(data_set$Runners=="011" & data_set$outs_when_up=="2",-0.61,
                                                                                                                                                                                        ifelse(data_set$Runners=="111" & data_set$outs_when_up=="0",-0.67,
                                                                                                                                                                                               ifelse(data_set$Runners=="111" & data_set$outs_when_up=="1",-0.86,
                                                                                                                                                                                                      ifelse(data_set$Runners=="111" & data_set$outs_when_up=="2",-0.77,"ERROR"))))))))))))))))))))))))
  
  
  
  #Determine the Run Value of a walk#
  data_set$RV_Walk <- ifelse(data_set$Runners=="000" & data_set$outs_when_up=="0",0.41,
                                             ifelse(data_set$Runners=="000" & data_set$outs_when_up=="1",0.28,
                                                    ifelse(data_set$Runners=="000" & data_set$outs_when_up=="2",0.13,
                                                           ifelse(data_set$Runners=="100" & data_set$outs_when_up=="0",0.6,
                                                                  ifelse(data_set$Runners=="100" & data_set$outs_when_up=="1",0.43,
                                                                         ifelse(data_set$Runners=="100" & data_set$outs_when_up=="2",0.22,
                                                                                ifelse(data_set$Runners=="010" & data_set$outs_when_up=="0",0.38,
                                                                                       ifelse(data_set$Runners=="010" & data_set$outs_when_up=="1",0.27,
                                                                                              ifelse(data_set$Runners=="010" & data_set$outs_when_up=="2",0.12,
                                                                                                     ifelse(data_set$Runners=="001" & data_set$outs_when_up=="0",0.37,
                                                                                                            ifelse(data_set$Runners=="001" & data_set$outs_when_up=="1",0.24,
                                                                                                                   ifelse(data_set$Runners=="001" & data_set$outs_when_up=="2",0.17,
                                                                                                                          ifelse(data_set$Runners=="110" & data_set$outs_when_up=="0",0.75,
                                                                                                                                 ifelse(data_set$Runners=="110" & data_set$outs_when_up=="1",0.63,
                                                                                                                                        ifelse(data_set$Runners=="110" & data_set$outs_when_up=="2",0.31,
                                                                                                                                               ifelse(data_set$Runners=="101" & data_set$outs_when_up=="0",0.51,
                                                                                                                                                      ifelse(data_set$Runners=="101" & data_set$outs_when_up=="1",0.39,
                                                                                                                                                             ifelse(data_set$Runners=="101" & data_set$outs_when_up=="2",0.22,
                                                                                                                                                                    ifelse(data_set$Runners=="011" & data_set$outs_when_up=="0",0.26,
                                                                                                                                                                           ifelse(data_set$Runners=="011" & data_set$outs_when_up=="1",0.2,
                                                                                                                                                                                  ifelse(data_set$Runners=="011" & data_set$outs_when_up=="2",0.16,
                                                                                                                                                                                         ifelse(data_set$Runners=="111" & data_set$outs_when_up=="0",1,
                                                                                                                                                                                                ifelse(data_set$Runners=="111" & data_set$outs_when_up=="1",1,
                                                                                                                                                                                                       ifelse(data_set$Runners=="111" & data_set$outs_when_up=="2",1,"ERROR"))))))))))))))))))))))))
  
  
  
  data_set$RV_Out <- as.numeric(data_set$RV_Out)
  data_set$RV_Walk <- as.numeric(data_set$RV_Walk)
  
  #Determine the Run Value of a Ball#
  data_set$Count_Ball_RV <- ifelse(data_set$balls=="0" & data_set$strikes=="0",0.036108717,
                                                   ifelse(data_set$balls=="1" & data_set$strikes=="0",0.069174515,
                                                          ifelse(data_set$balls=="2" & data_set$strikes=="0",0.146047634655311,
                                                                 ifelse(data_set$balls=="0" & data_set$strikes=="1",0.0226912526927981,
                                                                        ifelse(data_set$balls=="1" & data_set$strikes=="1",0.0486392876019061,
                                                                               ifelse(data_set$balls=="2" & data_set$strikes=="1",0.118238221335809,
                                                                                      ifelse(data_set$balls=="0" & data_set$strikes=="2",0.0373043974898094,
                                                                                             ifelse(data_set$balls=="1" & data_set$strikes=="2",0.0500591067994056,
                                                                                                    ifelse(data_set$balls=="2" & data_set$strikes=="2",0.0874434324885823,
                                                                                                           ifelse((data_set$balls=="3" & data_set$strikes=="0")|(data_set$balls=="3" & data_set$strikes=="1")|(data_set$balls=="3" & data_set$strikes=="2"),data_set$RV_Walk,"ERROR"))))))))))
  
  
  
  
  
  #Determine the Run Value of a strike#
  data_set$Count_Strike_RV <- ifelse(data_set$balls=="0" & data_set$strikes=="0",-0.0500955001440635,
                                                     ifelse(data_set$balls=="1" & data_set$strikes=="0",-0.0635129642662081,
                                                            ifelse(data_set$balls=="2" & data_set$strikes=="0",-0.0840481921616983,
                                                                   ifelse(data_set$balls=="3" & data_set$strikes=="0",-0.1118576054812,
                                                                          ifelse(data_set$balls=="0" & data_set$strikes=="1",-0.0602540389132455,
                                                                                 ifelse(data_set$balls=="1" & data_set$strikes=="1",-0.0456408941162342,
                                                                                        ifelse(data_set$balls=="2" & data_set$strikes=="1",-0.0442210749187347,
                                                                                               ifelse(data_set$balls=="3" & data_set$strikes=="1",-0.0750158637659617,
                                                                                                      ifelse((data_set$balls=="0" & data_set$strikes=="2")|(data_set$balls=="1" & data_set$strikes=="2")|(data_set$balls=="2" & data_set$strikes=="2")|(data_set$balls=="3" & data_set$strikes=="2") ,data_set$RV_Out,"ERROR")))))))))
  
  
  
  
  
  
  
  #Determine the Run Value of a Swing#
  data_set <- data_set %>% mutate(xRV_Swing=0.883*Single_Prob + 1.238*Double_Prob + 1.558*Triple_Prob + 1.979*HR_Prob + RV_Out*Out_Prob)
  
  
  data_set$Count_Strike_RV <- as.numeric(data_set$Count_Strike_RV)
  data_set$Count_Ball_RV <- as.numeric(data_set$Count_Ball_RV)
  
  #Determine the Run Value of a Take#
  data_set<- data_set %>% mutate(xRV_Take= Called_Strike_Prob*Count_Strike_RV + Count_Ball_RV * (1 - Called_Strike_Prob))
  
  
  #Determine whether the batter should swing or take the pitch#
  data_set$Correct_Decision<- ifelse(data_set$xRV_Swing < data_set$xRV_Take,"Take","Swing")
  
  #Say whether the batter made the Correct Decision#
  data_set$IsCorrect_Decision <- ifelse((data_set$Correct_Decision=="Swing" & data_set$IsSwing=="1")|(data_set$Correct_Decision=="Take" & data_set$IsSwing=="0"),1,0)
  
  
  #Determine the Run Value of the batters decision and weight it based on how easy the decision was#
  data_set$RV_of_Result <- ifelse(data_set$IsSwing=="0",data_set$xRV_Take * data_set$Swing_Prob,
                                                    ifelse(data_set$IsSwing=="1",(data_set$xRV_Swing )*(1- data_set$Swing_Prob),"ERROR"))
  
  data_set$RV_of_Result <- as.numeric(data_set$RV_of_Result)
  
  data_set<- data_set[!is.na(data_set$RV_of_Result), ]
  
  
  #Grade the decision based on the RV_of_Result score#
  data_set$Swing_Decision_Grade <-  ifelse(data_set$RV_of_Result>= 0.01648372,"A",
                                                         ifelse(data_set$RV_of_Result>= 0.007272974  & data_set$RV_of_Result< 0.01648372,"B",
                                                                ifelse(data_set$RV_of_Result>= 0.001374903  & data_set$RV_of_Result<0.007272974 ,"C",
                                                                       ifelse(data_set$RV_of_Result>=(-0.01711077 ) & data_set$RV_of_Result< 0.001374903 ,"D",
                                                                              ifelse(data_set$RV_of_Result< (-0.01711077 ),"F","ERROR")))))
  
  
  
  
  
  
  return(data_set)
  
}
  


 