pacman::p_load(ggplot2, magrittr, SAScii, GGally,beepr, stringr,tidyr, dplyr)

#---IMPORT DATA--------------------------------------------------------------------------------------#
f1 <- read.csv("NSCISC_Public_Data/f1_public.csv")

form1 <- c("UniID", "AInjAge", "AHDaSyRb", 
           "AFScorRb", "AASATotR", "AHghtRhb", "AWghtRhb","ANurLvlR", 
           "AVertInj", "AAsscInj", "ASpinSrg", "AUMVAdm", "AASATotR",
           "AI2RhADa",  "AHDaSyAc", "AHDaSyRb")
f1 %<>% dplyr::select(all_of(form1))
rm(form1)

#Clean Variables---------------------------------------------------------------#

#Days 
f1$AHDaSyRb[f1$AHDaSyRb >= 8888] = NA #days in rehab
f1$AHDaSyRb_log <- log(f1$AHDaSyRb)

f1$AI2RhADa[f1$AI2RhADa >= 8888] = NA #days in rehab
f1$AI2RhADa_log <- log(f1$AI2RhADa)


#Age 
f1$AInjAge <- factor(f1$AInjAge,
                     levels = c("0-14y", "15-29y", "30-44y", "60-74y", "45-59y" ,"75+y" ))#,ordered = TRUE)

#height
f1$AHghtRhb[f1$AHghtRhb > 100] = NA

#weight
f1$AWghtRhb[f1$AWghtRhb >= 888] = NA

#calcuate BMI
#https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
f1$ABMI <- f1$AWghtRhb / f1$AHghtRhb / f1$AHghtRhb * 703
#[weight (lb) / height (in) / height (in)] x 703


#vertebral injury
f1$AVertInj[f1$AVertInj > 1] = NA
f1$AVertInj <- as.factor(f1$AVertInj)

#associated injury
f1$AAsscInj[f1$AAsscInj > 1] = NA
f1$AAsscInj <- as.factor(f1$AAsscInj )

#spinal surgery
f1$ASpinSrg[f1$ASpinSrg > 1] = NA
f1$ASpinSrg <- as.factor(f1$ASpinSrg )

#use of mechanical ventilation
#f1$AUMVAdm_rc[f1$AUMVAdm_rc == 9] = NA
f1$AUMVAdm_rc <-  recode(f1$AUMVAdm,
                         "0" = "0",
                         "1" = "1",
                         "2" = "2",
                         "3" = "3",
                         "4" = "4")
f1$AUMVAdm_rc <- factor(f1$AUMVAdm_rc,
                         levels = c("0", "1", "2", "3", "4"))#,ordered = TRUE)
f1$AUMVAdm_rcc <-  recode(f1$AUMVAdm,
                         "0" = "0",
                         "1" = "1",
                         "2" = "1",
                         "3" = "1",
                         "4" = "1")


#total asia impairment score
f1$AASATotR[f1$AASATotR >= 888] = NA #rehab


#Functional Independence Score
f1$AFScorRb[f1$AFScorRb > 91] = NA

#Neurologic Level of Injury at Rehab Admit
f1$ANurLvlR_rcc <-  recode(na_if(f1$ANurLvlR,""),
                           "X99" = "U",
                           "C05" = "C05",
                           "T11" = "T11",
                           "C02" = "C02",
                           "L01"= "L01",
                           "T10"= "T10",
                           "T07"= "T07",
                           "C06" = "C06",
                           "C04" = "C04",
                           "T05"= "T05",
                           "L03" = "L03",
                           "T09"= "T09",
                           "T12"= "T12",
                           "C08" = "C08",
                           "T01"= "T01",
                           "C07" = "C07",
                           "T08"= "T08",
                           "T03"= "T03",
                           "C03" = "C03",
                           "T04"= "T04",
                           "T02"= "T02",
                           "C01" = "C01",
                           "L05"= "L05",
                           "L02"= "L02",
                           "S04"= "S",
                           "T06"= "T06",
                           "T99"= "U",
                           "X00" = "U",
                           "C99" = "U",
                           "L04" = "L04",
                           "L99"= "U", 
                           "S02" = "S",
                           "1"   = "U",
                           "S03"= "S",
                           .missing="U"
)


#Neurologic Level of Injury at Rehab Admit
f1$ANurLvlR_rc <-  recode(na_if(f1$ANurLvlR,""),
                          "X99" = "U",
                          "C05" = "C",
                          "T11" = "T",
                          "C02" = "C",
                          "L01"= "L",
                          "T10"= "T",
                          "T07"= "T",
                          "C06" = "C",
                          "C04" = "C",
                          "T05"= "T",
                          "L03" = "L",
                          "T09"= "T",
                          "T12"= "T",
                          "C08" = "C",
                          "T01"= "T",
                          "C07" = "C",
                          "T08"= "T",
                          "T03"= "T",
                          "C03" = "C",
                          "T04"= "T",
                          "T02"= "T",
                          "C01" = "C",
                          "L05"= "L",
                          "L02"= "L",
                          "S04"= "S",
                          "T06"= "T",
                          "T99"= "T",
                          "X00" = "U",
                          "C99" = "C",
                          "L04" = "L",
                          "L99"= "L", 
                          "S02" = "S",
                          "1"   = "U",
                          "S03"= "S",
                          .missing="U"
)

f1$ANurLvlR_rc <- factor(f1$ANurLvlR_rc,
                         levels = c("C", "T", "L", "S", "U"),ordered = TRUE)

#STANDARDIZE VARIABLES---------------------------------------------------------#

stand <- function(data){
  std_data <- (data - mean(data, na.rm = TRUE))/sd(data, na.rm= TRUE)
  return(std_data)}

#total asia impairment score
f1$AASATotR_st <-stand(f1$AASATotR)

#BMI
f1$ABMI_st<- stand(f1$ABMI)

beep()



     
