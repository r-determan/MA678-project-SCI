pacman::p_load(tidyr, dplyr, ggplot2)


# IMPORT DATA
f1 <- read.csv("NSCISC_Public_Data/f1_public.csv")
form1 <- c("UniID", "AInjDt", "AAdmDt", "ARbAdmDt", "ADisDt", "AInjAge", 
           "ASex", "APResInj", "APResDis", "ATrmEtio", "AExtCsIj", "AVertInj",
           "AAsscInj", "ASpinSrg", "AHghtRhb", "AWghtRhb", "AUMVAdm","AHDaSyRb", 
           "AUMVDis","AFScorRb","ANurLvlR","AASAImRb","APPTotDs","ANurLvlD",
           "AASAImDs","ANCatDis","AHispnic","ARace","ASpkEngl","AMarStIj",
           "AEducLvl","APrLvlSt","AJobCnCd","AWrkRltd","AFmIncLv","AVeteran",
           "APrimPay","ADepress","AAnxiety","ADiabete","AAlcRate","AAlcNbDr",
           "AAlc6Mor","ATBILOC","ATBIMem","ATBISevR")
f1 %<>% select(all_of(form1))

f2 <- read.csv("NSCISC_Public_Data/f2_public.csv")
form2 <- c("UniID", "BYear","BFolUpCt","BIntvDt","BAnExDt","BWeight","BPlcRes",
           "BMarStat","BMarStCh","BEducLvl","BPrLvlSt","BJobCnCd","BFmIncLv",
           "BCompUse","BCellPhn","BLifSat1","BLifSat2","BLifSat3","BLifSat4",
           "BLifSat5","BLifSatT","BBPHQ1","BBPHQ2","BHeight","BPrSrIn5",
           "BSPHthSt","BSPHthRC","BDepress","BAnxiety","BAlcRate",
           "BAlcNbDr","BAlc6Mor","BWCSUse","BWCSType","BWlk150","BWlkBlck",
           "BWlkStps","BMobAid1","BMobAid2","BMobAid3","BMobAid4","BMobAid5",
           "BModVhcl","BDrvModV","BASATot","BLTTotal","BPPTotal","BASAImp",
           "BMotLvRt","BMotLvLf","BPrNuFRt","BPrNuFLf","BNurLvl")
f2 %<>% select(all_of(form2))

rec_public <- read.csv("NSCISC_Public_Data/rec_public.csv")
rec <- c("UniID", "SVitSrce","SVitF2Yr","SVitDate","SDthDt","SCsDth1",
         "SCsDth2","SCsDth3","SCsDth4","SCsDth5","SStatCur")
rec_public %<>% select(all_of(rec))



#RECODE SELECTED VARIABLES
#traumatic etiology
f1$ATrmEtio_s <- as.character(f1$ATrmEtio)

f1$ATrmEtio_s <- recode(f1$ATrmEtio_s, "1"="Vehicular", 
                                       "2"="Vehicular", 
                                       "3"="Vehicular", 
                                       "4"="Vehicular", 
                                       "5"="Vehicular",
                                       "6"="Vehicular",
                                       "7"="Vehicular",
                                       "8"="Vehicular",
                                       "9"="Vehicular",

                                       "10"="Violence",
                                       "11"="Violence",
                                       "12"="Violence",
                                       "15"="Violence",
          
                                       "20"="Sport/Rec",
                                       "21"="Sport/Rec",
                                       "22"="Sport/Rec",
                                       "23"="Sport/Rec",
                                       "24"="Sport/Rec",
                                       "26"="Sport/Rec",
                                       "27"="Sport/Rec",
                                       "28"="Sport/Rec",
                                       "29"="Sport/Rec",
                                       "70"="Sport/Rec",
                                       "71"="Sport/Rec",
                                       "72"="Sport/Rec",
                                       "73"="Sport/Rec",
                                       "74"="Sport/Rec",
                                       "75"="Sport/Rec",
                                       "76"="Sport/Rec",
                                       "77"="Sport/Rec",
                                       "78"="Sport/Rec",
                                       "25"="Sport/Rec",
          
                                       "30"="Fall",
                                       "31"="Falling/Flying Object",
          
                                       "40"="Pedestiran",
                                       "50"="Med. Complication", 
                                       "60"="Other", 
                                       "99"="Unknown")
#Race
f1$ARace <-  recode(f1$ARace,
                    "1" = "White", 
                    "2" = "Black",
                    "3" = "American Indian, Alaska Native",
                    "4" = "Asian, Pacific Islander", 
                    "5" = "Other, Multiracial",
                    "7" = "Declined",
                    "9" = "Unknown")

#Hispanic
f1$AHispnic <-  recode(f1$AHispnic,
                       "0" = "Not of Hispanic Origin", 
                       "1" = "Hispanic or Latino",
                       "7" = "Declined",
                       "9" = "Unknown")

#Education Level
f1$AEducLvl_txt <-  recode(f1$AEducLvl,
                           "1" = "<= 8th grade", 
                           "2" = "9- 12",
                           "3" = "H.S Diploma/ GED",
                           "4" = "Associate", 
                           "5" = "Bachelors",
                           "6" = "Masters",
                           "7" = "Doctorate",
                           "8" = "Other",
                           "9" = "Unknown")

#Neurologic Level of Injury at Rehab Admit
f1$ANurLvlR_rc <-  recode(f1$ANurLvlR,
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
                            "S03"= "S")

f1$ANurLvlR_rc <- factor(f1$ANurLvlR_rc,
                           levels = c("C", "T", "L", "S", "U"),ordered = TRUE)
