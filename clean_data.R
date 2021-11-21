pacman::p_load(tidyr, dplyr, ggplot2, magrittr, SAScii, GGally,beepr)


#---IMPORT DATA--------------------------------------------------------------------------------------#
f1 <- read.csv("NSCISC_Public_Data/f1_public.csv")





form1 <- c("UniID", "AInjDt","ASex", "AInjAge", "ATrmEtio", "AHDaSyRb", 
           "AFScorRb", "AFScorDs", "AASATotR", "AHghtRhb", "AWghtRhb",
           "AHispnic","ARace","AEducLvl", "AASATotD", "ANurLvlR", 
           "AVertInj", "AAsscInj", "ASpinSrg", "AUMVAdm", "ANCatRhb", "AASATotR",
           "ALTTotRh", "APPTotRh")
f1 %<>% select(all_of(form1))

f1$ASex <- as.factor(f1$ASex)
f1$AHDaSyRb_log <- log(f1$AHDaSyRb)


f2 <- read.csv("NSCISC_Public_Data/f2_public.csv")
form2 <- c("UniID", "BYear", "BSPHthRC", "BASATot", "BLTTotal", "BPPTotal", "BASAImp")
f2 %<>% select(all_of(form2))

rec_public <- read.csv("NSCISC_Public_Data/rec_public.csv")
rec <- c("UniID", "SDthDt")
rec_public %<>% select(all_of(rec))

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

#Clean Unknown variables-------------------------------------------------------#
f1$ASex[f1$ASex == 9] = NA

f1$ARace[f1$ARace >6] = NA

f1$AEducLvl[f1$AEducLvl == 9] = NA

#Total Days Hospitalized in the System’s Inpatient Acute (and Subacute) Rehabilitation Unit
f1$AHDaSyRb[f1$AHDaSyRb >= 8888] = NA

#height
f1$AHghtRhb[f1$AHghtRhb > 100] = NA

#weight
f1$AWghtRhb[f1$AWghtRhb >= 888] = NA

#vertebral injury
f1$AVertInj[f1$AVertInj > 1] = NA

#associated injury
f1$AAsscInj[f1$AAsscInj > 1] = NA

#spinal surgery
f1$ASpinSrg[f1$ASpinSrg > 1] = NA

#use of mechanical ventilation
f1$AUMVAdm_rc <-  recode(f1$AUMVAdm,
                         "0" = "0",
                         "1" = "1",
                         "2" = "2",
                         "3" = "3",
                         "4" = "4",
                         "9" = "9")

f1$AUMVAdm_rc[f1$AUMVAdm_rc == 9] = NA
f1$AUMVAdm_rc <- factor(f1$AUMVAdm_rc,
                         levels = c("0", "1", "2", "3", "4"),ordered = TRUE)

#Category of neurologic impairment at rehab admit
f1$ANCatRhb[f1$ANCatRhb >= 8] = NA
f1$ANCatRhb <-  as.factor(f1$ANCatRhb)

#total asia impairment score
f1$AASATotR[f1$AASATotR >= 888] = NA

#asia light touch
f1$ALTTotRh[f1$ALTTotRh >= 888] = NA

#asia pinprick
f1$APPTotRh[f1$APPTotRh >= 888] = NA




#Total ASIA Motor Index Score - keep only valid results
f2$BASATot[f2$BASATot > 100] = NA #888 = not testable; 999 = unknown
f2$BLTTotal[f2$BLTTotal > 112] = NA #888 = not testable; 999 = unknown
f2$BPPTotal[f2$BPPTotal > 112] = NA #888 = not testable; 999 = unknown


#---RECODE VARIABLES--------------------------------------------------------------------------#
#Form 1

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

f1$AInjAge <- factor(f1$AInjAge,
                         levels = c("0-14y", "15-29y", "30-44y", "60-74y", "45-59y" ,"75+y" ),ordered = TRUE)


f1$ASex <- recode(f1$ASex, 
                        "1"="Male", 
                        "2"="Female", 
                        "3"="Other, Transgender")


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


#Form 2 
f2$BSPHthRC <- as.numeric(recode(na_if(f2$BSPHthRC,""),
                           "1" = "1", #much better than
                           "2" = "1", #somewhat better 
                           "3" = "0", #about the same
                           "4" = "-1", #somewhat worse
                           "5" = "-1", #much worse
                           "6" = "9", #participant doesnt know
                           "7" = "9", #declined
                           "9" = "9",  #unknown, interview not done, respondent is less than 18yo
                           .missing = "10",
                           .default = "10"))


#NHIS
nhis$AHSTATYR <-  recode(nhis$AHSTATYR,
                           "1" = "1", #better than
                           "2" = "-1", #worse than
                           "3" = "0",  #about the same
                           "7" = "9", #refused
                           "8" = "9", #not ascertained
                           "9" = "9") #don't know
nhis$AHSTATYR <- as.numeric(nhis$AHSTATYR)




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

#asia light touch
f1$ALTTotRh_st <- stand(f1$ALTTotRh)

#asia pinprick
f1$APPTotRh_st <- stand(f1$APPTotRh)

#height
f1$AHghtRhb_st <- stand(f1$AHghtRhb)

#weight
f1$AWghtRhb_st <- stand(f1$AWghtRhb)




#----------------------------------------------------------------------------------------------#


#play noise when done
beep()

     