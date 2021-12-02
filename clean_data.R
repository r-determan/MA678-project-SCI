pacman::p_load( ggplot2, magrittr, SAScii, GGally,beepr, stringr,tidyr, dplyr)


#---IMPORT DATA--------------------------------------------------------------------------------------#
f1 <- read.csv("NSCISC_Public_Data/f1_public.csv")

form1 <- c("UniID", "AInjDt","ASex", "AInjAge", "ATrmEtio", "AHDaSyRb", 
           "AFScorRb", "AFScorDs", "AASATotR", "AHghtRhb", "AWghtRhb",
           "AHispnic","ARace","AEducLvl",  "ANurLvlR", "AFmIncLv",
           "AVertInj", "AAsscInj", "ASpinSrg", "AUMVAdm", "ANCatRhb",
           "ALTTotRh", "APPTotRh", 
           "AASATotA", "AASATotR", "AASATotD",
           "AI2RhADa",  "AHDaSyAc", "AHDaSyRb","AASAImRb")
f1 %<>% dplyr::select(all_of(form1))

# f2 <- read.csv("NSCISC_Public_Data/f2_public.csv")
# form2 <- c("UniID", "BYear","BFIMScor", "BSPHthRC", "BASATot", "BLTTotal", "BPPTotal", "BASAImp", "BSPHthSt", "BNImpCat",
#            "BDepress", "BAnxiety", "BLifSatT", "BWeight", "BLifSat1", "BLifSat2", "BLifSat3", "BLifSat4", "BLifSat5","BNurLvl" )
# f2 %<>% select(all_of(form2))
# 
# rec_public <- read.csv("NSCISC_Public_Data/rec_public.csv")
# rec <- c("UniID", "SDthDt")
# rec_public %<>% select(all_of(rec))

#National Health Interview Survey

#Import data for the first time

# AHSTATYR_nhis <- data.frame(year = c(), AHSTATYR = c())
# for (yr in c(2016, 2017)){
#   data <- paste0("samadult_", yr, "/samadult.csv")
#   dta <- read.csv(data)
#   y <- rep(yr, dim(dta)[1])
#   select <- data.frame(year = y, AHSTATYR = dta$AHSTATYR)
#   AHSTATYR_nhis <- rbind(AHSTATYR_nhis, select)
# }
# 
# for (yr in c(1997, 1998, 2007)){
#   data_url <- paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/", yr, "/samadult.zip")
#   sas_url <- paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/", yr, "/samadult.sas")
#   dta <- read.SAScii(fn = data_url, sas_ri = sas_url, zipped = TRUE)
#   #dta <- read.table(paste0("samadult_", yr, "/SAMADULT.DAT"))
#   y <- rep(yr, dim(dta)[1])
#   select <- data.frame(year = y, AHSTATYR = dta$AHSTATYR)
#   AHSTATYR_nhis <- rbind(AHSTATYR_nhis, select)
# }
# write.csv(AHSTATYR_nhis, "nhis_ahstatyr.csv")

#read in data for other times
nhis <- read.csv("nhis_ahstatyr.csv")

#Clean Variables---------------------------------------------------------------#

#F1--------------------------------------------------------------------#
#injury year
f1$AInjDt <- as.numeric(f1$AInjDt)
#Sex
f1$ASex[f1$ASex == 3 | f1$ASex == 9] = NA #exclude unknown and transgender/other(due to small number of individuals)
f1$ASex <- as.factor(f1$ASex)
f1$ASex <- recode(f1$ASex, 
                  "1"="Male", 
                  "2"="Female", 
                  "3"="Other, Transgender")

#Days 
f1$AI2RhADa[f1$AI2RhADa >887] = NA 
f1$AI2RhADa_log <- log(f1$AI2RhADa)

f1$AHDaSyAc[f1$AHDaSyAc >= 8888] = NA #

f1$AHDaSyRb[f1$AHDaSyRb >= 8888] = NA #days in rehab
f1$AHDaSyRb_log <- log(f1$AHDaSyRb)

#Race
f1$ARace[f1$ARace >6] = NA
f1$ARace <-  recode(f1$ARace,
                    "1" = "White",
                    "2" = "Black",
                    "3" = "American Indian, Alaska Native",
                    "4" = "Asian, Pacific Islander",
                    "5" = "Other, Multiracial")

#Hispanic
f1$AHispnic <-  recode(f1$AHispnic,
                       "0" = "Not of Hispanic Origin",
                       "1" = "Hispanic or Latino",
                       "7" = "Declined",
                       "9" = "Unknown")

#Education Level
f1$AEducLvl[f1$AEducLvl == 9] = NA
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

#Family Income Level
f1$AFmIncLv_txt <- recode(f1$AFmIncLv,
                          "1" = "< $25,000",
                          "2" = "$25,000 - $49,999",
                          "3" = "$50,000 - $74,999",
                          "4" = "> $75,000")

f1$AFmIncLv_txt <- factor(f1$AFmIncLv_txt,
                     levels = c("< $25,000", "$25,000 - $49,999", "$50,000 - $74,999", "> $75,000"),ordered = TRUE)

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

#traumatic etiology
f1$ATrmEtio_s <- as.character(f1$ATrmEtio)
f1$ATrmEtio_s <- as.factor(recode(f1$ATrmEtio_s, "1"="Vehicular", 
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
                        "99"="Unknown"))

#Category of neurological impairment at rehab admit
f1$ANCatRhb[f1$ANCatRhb >= 8] = NA
f1$ANCatRhb <-  as.factor(f1$ANCatRhb)

#total asia impairment score
f1$AASATotA[f1$AASATotA >= 888] = NA #admit
f1$AASATotR[f1$AASATotR >= 888] = NA #rehab
f1$AASATotD[f1$AASATotD >= 888] = NA #discharge

#asia light touch
f1$ALTTotRh[f1$ALTTotRh >= 888] = NA

#asia pinprick
f1$APPTotRh[f1$APPTotRh >= 888] = NA

#Functional Independence Score
f1$AFScorRb[f1$AFScorRb > 91] = NA
f1$AFScorDs[f1$AFScorDs > 91] = NA
#percent change
f1$AFScor_per_ch <- (f1$AFScorDs - f1$AFScorRb)/f1$AFScorRb

#Income level
f1$AFmIncLv[f1$AFmIncLv>=6] == NA

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

f1$ANurLvlR_rc <- factor(f1$ANurLvlR_rc,
                         levels = c("C", "T", "L", "S", "U"),ordered = TRUE)

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

# #F2--------------------------------------------------------------------#
# #Total ASIA Motor Index Score - keep only valid results
# f2$BASATot[f2$BASATot > 100] = NA #888 = not testable; 999 = unknown
# 
# #ASIA light touch; pin prick
# f2$BLTTotal[f2$BLTTotal > 112] = NA #888 = not testable; 999 = unknown
# f2$BPPTotal[f2$BPPTotal > 112] = NA #888 = not testable; 999 = unknown
# 
# 
# #Life Satisfaction
# f2$BLifSatT[f2$BLifSatT > 35] = NA
# 
# f2$BLifSat1[f2$BLifSat1 > 7] = NA
# f2$BLifSat2[f2$BLifSat2 > 7] = NA
# f2$BLifSat3[f2$BLifSat3 > 7] = NA
# f2$BLifSat4[f2$BLifSat4 > 7] = NA
# f2$BLifSat5[f2$BLifSat5 > 7] = NA
# 
# 
# #Depression
# f2$BDepress[f2$BDepress >= 7] = NA
# f2$BDepress <- as.factor(f2$BDepress)
# 
# 
# #Anxiety
# f2$BAnxiety[f2$BAnxiety >=7] = NA
# 
# 
# #Weight and Height
# f2$BWeight[f2$BWeight>887] = NA
# f2$BSPHthSt[f2$BSPHthSt>=6] = NA
# 
# 
# #---RECODE VARIABLES--------------------------------------------------------------------------#
# #Form 1
# 
# 
# 
# #Form 2 
# f2$BSPHthRC <- as.numeric(recode(na_if(f2$BSPHthRC,""),
#                            "1" = "1", #much better than
#                            "2" = "1", #somewhat better 
#                            "3" = "0", #about the same
#                            "4" = "-1", #somewhat worse
#                            "5" = "-1", #much worse
#                            "6" = "9", #participant doesnt know
#                            "7" = "9", #declined
#                            "9" = "9",  #unknown, interview not done, respondent is less than 18yo
#                            .missing = "10",
#                            .default = "10"))
# 
# 
# f2$BSPHthRC[f2$BSPHthRC>=9] = NA
# 
# f2$BAnxiety_s <- as.factor(recode(f2$BAnxiety,
#                                   "0" = "0",
#                                   "1" = "1",
#                                   "2" = "1",
#                                   "3" = "1",
#                                   "4" = "1"))
# 
# f2$BAnxiety <- as.factor(recode(f2$BAnxiety,
#                       "0" = "no",
#                       "1" = "PTSD",
#                       "2" = "Panic Disorder",
#                       "3" = "GAD",
#                       "4" = "Multiple, and 1st unknown"))
# 
# 
# 
# f2$BDep_Anx <- if_else(f2$BDepress == 1 | f2$BAnxiety_s ==1, 1, 0)
# 
# #NHIS
# nhis$AHSTATYR <-  recode(nhis$AHSTATYR,
#                            "1" = "1", #better than
#                            "2" = "-1", #worse than
#                            "3" = "0",  #about the same
#                            "7" = "9", #refused
#                            "8" = "9", #not ascertained
#                            "9" = "9") #don't know
# nhis$AHSTATYR <- as.numeric(nhis$AHSTATYR)
# 
# 
# 

#---CALCULATE SELECT FIELDS--------------------------------------------------------------------#
#CALCULATE CHANGE IN FUNCTIONAL INDEPENDENCE TOTAL MOTOR SCORE
valid <- f1$AFScorRb <= 91 & f1$AFScorDs<=91
f1$AFScor_rb_d <- ifelse(valid,f1$AFScorDs-f1$AFScorRb,NA)


#CALCULATE CHANGE IN ASIA 
#AASATotR: ASIA Motor Index Score at Inpatient Rehab Admit, Total
#AASATotD: ASIA Motor Index Score Discharge Total
valid <- f1$AASATotR <= 100 & f1$AASATotD<=100
f1$AASATot_change_RD <- ifelse(valid,f1$AASATotD-f1$AASATotR,NA)
#hist(f1$AASATot_change)
rm(valid)



#STANDARDIZE VARIABLES

stand <- function(data){
  std_data <- (data - mean(data, na.rm = TRUE))/sd(data, na.rm= TRUE)
  return(std_data)
}

#total asia impairment score
f1$AASATotR_st <-stand(f1$AASATotR)
#f2$BASATot_st <- stand(f2$BASATot)
#asia light touch
f1$ALTTotRh_st <- stand(f1$ALTTotRh)

#asia pinprick
f1$APPTotRh_st <- stand(f1$APPTotRh)

#height
f1$AHghtRhb_st <- stand(f1$AHghtRhb)

#weight
f1$AWghtRhb_st <- stand(f1$AWghtRhb)

#BMI
f1$ABMI_st<- stand(f1$ABMI)

#f2$BWeight_st <- stand(f2$BWeight)


#JOIN and PIVOT----------------------------------------------------------------#
# jn <- right_join(f1,f2)
# jn$BLifSatT <- as.numeric(jn$BLifSatT)
# 
# jn %<>% 
#   rowwise() %>% 
#   mutate(BLifSat_mean = mean(c(BLifSat1, BLifSat2, BLifSat3, BLifSat4, BLifSat5)))
# aasa <- f1 %>% 
#         select(UniID,ANurLvlR_rcc,ANurLvlR_rc, AASATotA, AASATotR, AASATotD, AI2ADays, AHDaSyAc, AHDaSyRb, AHghtRhb_st, AWghtRhb_st , AVertInj, AAsscInj) %>% #
#         na.omit() %>% 
#         mutate( 
#           d_AASATotA = AI2ADays, #number of days since injury to first admission (ASIA score at first admission)
#           d_AASATotR = AI2ADays+AHDaSyAc, # injury -> hosp admission -> rehab admit
#           d_AASATotD = AI2ADays+AHDaSyAc + AHDaSyRb) %>%  # injury -> hosp admission -> rehab admit -> rehab discharge
#          pivot_longer(cols = starts_with("d_"), 
#                        values_to = "days",
#                        names_to = "day_cat") %>% 
#           pivot_longer(cols = starts_with("AASATot"), 
#                        values_to = "AASATot",
#                        names_to = "AASATot_cat") %>% 
#           mutate(day_cat = str_sub(day_cat,3, 10)) %>% 
#           filter(day_cat == AASATot_cat) %>% 
#           mutate(log_days = log(days))

# 
# lifsat <- jn %>% 
#           select(UniID, ANurLvlR_rcc, AInjAge, AEducLvl_txt, ATrmEtio_s, 
#                  BYear, BDepress, BAnxiety_s, BSPHthRC, BLifSat1, BLifSat2, BLifSat3, BLifSat4, BLifSat5) %>% 
#           na.omit() %>% 
#           pivot_longer(starts_with("BLifSat"), names_to = "qnum", values_to = "resp")
#           

data <-  f1 %>% dplyr::select(c("AFScor_per_ch","AFScor_rb_d",
                         "AFScorRb","AFScorDs", "AASATotR","AHDaSyRb_log", 
                         "AInjAge", "ARace", "ASex", "AEducLvl_txt", "AFmIncLv_txt",
                         "AVertInj" ,"AAsscInj" , "ASpinSrg", "AUMVAdm_rcc", "ANurLvlR_rcc", "ANurLvlR_rc",
                         "AHghtRhb_st", "AWghtRhb_st", "AASAImRb")) %>% na.omit()


beep()
     
