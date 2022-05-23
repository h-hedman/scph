#title: "scph_covid_summary"
#author: "Hayden Hedman"
#date: "2022-05-11"
## ------------------------------------------------------------------------------------------------------
## LOAD PACKAGES  
pacman::p_load(dplyr, data.table, bit64, curl, tidyr, tidyverse, zoo, gsheet,googlesheets4, ggpubr, ggplot2)
## ------------------------------------------------------------------------------------------------------
## LOAD DATA SOURCES 
## GOOGLE SHEET DATA SOURCES
gv <- data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1nZe9ifZm45dUTKSLb8n_pkxraXO3ic1bR3c14Vw8zBo/edit?usp=sharing'))
ob <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1oC3O0dfyyr1sAoPT6Et1Htdg4wwhIR8WictHTX7Fv_0/edit#gid=0'))
sasmc <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing'))
## ------------------------------------------------------------------------------------------------------
## SET WD TO DOWNLOADS 
setwd("C:/Users/hayde/Downloads")
ceders <- as.data.frame(read.delim("cedrs_Summit.txt", sep="|"))
elr_df <- as.data.frame(read.delim("elr_rollup_Summit.txt", sep="|"))
vax <- read.delim("PatientImmunizations_Summit.txt", sep="|")
co_hosp <- read.delim("cophs_Summit.txt", sep="|")
variance <- read.delim("elr_variant_tests_Summit.txt", sep="|")
## ------------------------------------------------------------------------------------------------------
## OLD HTML PULLS [NOT WORKING: 2022-04-29]
#ceders <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
#elr_df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)
#vax <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/PatientImmunizations_Summit.txt', encoding="unknown"); dim(df)
## ------------------------------------------------------------------------------------------------------
## SET OUTPUT WORKING DIRECTORY 
setwd("G:/My Drive/Summit County - COVID-19/Database/Datawrapper")

## FILTER OUT DUPLICATE ENTRIES
ceders$unique_id <- with(ceders, paste0(profileid, first_name, last_name, date_of_birth,reporteddate))
ceders<- subset(ceders, !duplicated(unique_id))
## MAKE ceders$reporteddate INTO DATE STRUCTURE 
ceders$reporteddate <- as.Date(format(as.Date(ceders$reporteddate, "%Y-%m-%d"), "%Y-%m-%d"))
## MAKE ZIP CODE DATA CHARACTER
ceders$zip =0
ceders$zip[which(ceders$geocoded_zipcode=="80498")]<-"80498"
ceders$zip[which(ceders$geocoded_zipcode=="80435")]<-"80435"
ceders$zip[which(ceders$geocoded_zipcode=="80424")]<-"80424"
ceders$zip[which(ceders$geocoded_zipcode=="80497")]<-"80497"
ceders$zip[which(ceders$geocoded_zipcode=="80443")]<-"80443"
## CUTOFF DATES
curr_ceders = as.Date(max(ceders$reporteddate))
cutoff_index_date = as.Date("2020-03-12")
rec_28d_date = max(ceders$reporteddate) - 28
#########################################################################################################################
## SASMC HOSPITAL DATA FROM GOOGLE SHEETS
#sasmc <- sasmc[,c("date","covid_pac","covid_pac_trans")]
total_sasmc_cov_pat <- sum(sasmc$covid_pac)
## CO SC RESIDENT COUNT
count_co_hosp <- nrow(unique(co_hosp))

###############################################################################################################
## ELR TEST DATA SUMMARY 
TT_sum <- elr_df %>%
  filter(test_type == "PCR") %>% 
  select(dateadded, total_tests) %>% 
  distinct() %>% 
  mutate(sum = sum(total_tests))
TT <- tail(TT_sum, 1)$sum
###############################################################################################################
## OUTBREAK DATA 
names(ob) <- c("facility","first_case_date","cases","resolved_date")
## FORMAT OB DATE
ob$Date <- as.Date(format(as.Date(ob$first_case_date, "%m/%d/%Y"), "%Y-%m-%d"))
## FILTER DUPLICATE ENTRIES
ob <- ob %>%
  select(Date, facility,cases) %>% 
  distinct() 
## OB COUNT
OB <- nrow(ob)
## COUNT VAR
ob$freq=1
## SUMMARIZE NUMBER OF OUTBREAKS BY M-YYYY
ob_summ <- data.frame(group_by(ob, Date) %>% 
                        summarize(outbreak_sum = sum(freq=="1")))
###############################################################################################################
## DEATH SUM
ceders_death <- subset(ceders, outcome=="Patient died")
ceders_death$count=1
DA = length(ceders_death$count)
###############################################################################################################
## SUMMARIZE DAILY REPORT
total_summ <- data.frame(group_by(ceders) %>% 
                           summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                     prob_sum = sum(casestatus=="probable"),
                                     total_cases = sum(pos_confirm_sum, prob_sum)))

type_eng = c("Confirmed Positive Tests", "Probable Cases", "Total Cases", "Hospitalizations", "Total Tested", "Outbreaks", "Deaths Among Cases")
type_esp = c("Pruebas positivas confirmadas", "Casos probables", "Total de casos", "Hospitalizaciones", "Total de pruebas", "Brotes", "Muertes entre casos")

value = c(total_summ$pos_confirm_sum, total_summ$prob_sum, total_summ$total_cases, count_co_hosp, TT, OB, DA)
summary_eng <- data.frame(type_eng, value)
summary_esp <- data.frame(type_esp, value)
## SUMMARY TABLE (ENG)
write.csv(summary_eng, row.names = F, "summary_table_eng.csv")
## SUMMARY TABLE (ESP)
write.csv(summary_esp, row.names = F, "summary_table_esp.csv")
##############################################################################################
##(2) PCR TEST PROPORTION AND NEGATIVE TEST COUNTS
## FILTER DF OF ONLY PCR OUTCOMES
elr_df <- subset(elr_df, test_type=="PCR"); dim(df)
##############################################################################################
## Summarize sum of  tests by dateadded
total_tests <- data.frame(group_by(elr_df, dateadded) %>%
                            summarize(total_tested = sum(n_tests))); dim(total_tests)
##############################################################################################
## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
total_tests$sort_ID <- as.integer(factor(with(total_tests, paste(dateadded))))
## FILTER OUT DATES BEFORE 13-MARCH-2020
total_tests <- subset(total_tests, dateadded  >cutoff_index_date)
total_tests <- arrange(total_tests, -sort_ID)
##############################################################################################
## PULL OUT POSITIVE DF BUT FILTER BY DUPLICATED DATEADDED 
## THE SUM OF POSITIVE TESTS IS SAME ACCROSS CDPHE AND NON-CDPHE LAB TYPES (2X)
pos_tests <- elr_df[!duplicated(elr_df[3]),]
## PULL OUT MEANINGFUL VARIABLES
pos_tests <- pos_tests[,c("total_confirmed_cases", "county", "dateadded")]
## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
pos_tests$sort_ID <- as.integer(factor(with(pos_tests, paste(dateadded))))
## SORT OLDEST TO NEWEST M
pos_tests <- arrange(pos_tests, sort_ID); head(pos_tests)
## MAKE SURE 3/10 - 3/12 ARE REMOVED
pos_tests <- subset(pos_tests, dateadded > cutoff_index_date)
## SORT BACK TO NEWEST TO OLDEST
pos_tests <- arrange(pos_tests, -sort_ID); head(pos_tests)
## REMOVE DATEADDED VARIABLE - THIS IS HERE JUST FOR QUALITY CONTROL
pos_tests$dateadded = NULL
##############################################################################################
## CBIND UP THE POSITIVE AND TOTAL TESTS
elr_df2 <- cbind(total_tests, pos_tests)
## CORRECT FOR CDPHE DATA ENTRY ERRORS (SOMETIMES THEY HAVE 0 TOTAL & POS >0)
## IMPORTANT THAT NONE OF THE % >> 100 OR = INF
setDT(elr_df2)[total_tested <total_confirmed_cases, total_tested := total_confirmed_cases]
## SUBSTRACT TOTAL-POS = SUM NEG TESTS
elr_df2$total_neg_tests = elr_df2$total_tested-elr_df2$total_confirmed_cases
## CALCULATE PROPORTION OF POSITIVE TESTS [Total Number of PCR POSITIVE Tests/Total Number of PCR Tests]
elr_df2$positive_prop = (elr_df2$total_confirmed_cases/elr_df2$total_tested)*100
## CONVERT ALL NAN PROPORTIONS (I.E. 0/0) TO 0
elr_df2$positive_prop[is.na(elr_df2$positive_prop)] <- 0
## REMOVE PLACEHOLDER 'COUNTY' VARIABLE
elr_df2$county = NULL
############################################################################################
## CALCULATE MOVING 3-DAY MEAN OF POSITIVE PROP
elr_df2<- subset(elr_df2, sort_ID > 11)

elr_df2$positive_prop <- as.double(elr_df2$positive_prop)

elr_df2$avg_7day <- stats::filter(elr_df2$positive_prop, rep(1/7,7)); head(df2)

## SHIFT FUNCTION
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
## SHIFT UP
elr_df2$avg_7day <- shift(elr_df2$avg_7day, 3)
## REPLACE ALL NA WITH 0 
elr_df2$avg_7day[is.na(elr_df2$avg_7day)] <- 0
##############################################################################################
## PULL OUT VARIABLES FOR DF'S
## 1) POSITIVE PROPORTION
pos_percent_df <- elr_df2[,c("dateadded", "positive_prop", "avg_7day")]
## CLEAN COLUMN NAMES - ENG
colnames(pos_percent_df)[1] <- "Date"
colnames(pos_percent_df)[2] <- "Percentage of Tests Confirmed Positive"
colnames(pos_percent_df)[3] <- "7-day Moving Average Percentage of Confirmed Positive Tests"
##############################################################################################
## SUBSET LAST 30-DAY PERIOD
pos_percent_df$sort_ID <- as.integer(factor(with(pos_percent_df, paste(Date))))
## SORT BY TEMP ID
pos_percent_df <- arrange(pos_percent_df, -sort_ID); head(pos_percent_df)
pos_percent_df_sub <- head(pos_percent_df, 30); dim(pos_percent_df_sub)
pos_percent_df$sort_ID= NULL
pos_percent_df_sub$sort_ID= NULL
##############################################################################################
## CLEAN UP VARIABLES (ENG)
test_count_df <- elr_df2[,c("dateadded", "total_tested", "total_confirmed_cases")]
## CLEAN COLUMN NAMES
colnames(test_count_df)[1] <- "Date"
colnames(test_count_df)[2] <- "Total People Tested"
colnames(test_count_df)[3] <- "People Confirmed Positive"

test_count_df <- test_count_df[,c("Date", "Total People Tested")]
## SAVE PCR COUNT DF
total_pcr_pos <- elr_df2[,c("dateadded", "total_tested", "total_confirmed_cases")]

#write.csv(total_pcr_pos, "pcr_count_df.csv", row.names=F)
##############################################################################################
## SUBSET LAST 30-DAY PERIOD
test_count_df$sort_ID <- as.integer(factor(with(test_count_df, paste(Date))))
## SORT BY TEMP ID
test_count_df <- arrange(test_count_df, -sort_ID); head(test_count_df)
test_count_df_sub <- head(test_count_df, 30); dim(test_count_df_sub)
test_count_df$sort_ID= NULL
test_count_df_sub$sort_ID= NULL

##############################################################################################
## WRITE TO CSV (ENG)
#write.csv(pos_percent_df, row.names=FALSE, "pos_percent_eng.csv")
#write.csv(test_count_df, row.names=FALSE, "test_count_eng.csv")
## 30-DAY PERIOD - ENG
#write.csv(pos_percent_df_sub, row.names=FALSE, "pos_percent_30day_eng.csv")
#write.csv(test_count_df_sub, row.names=FALSE, "test_count_30day_eng.csv")
##############################################################################################
## 1) POSITIVE PROPORTION (ESP)
pos_percent_ESP_df <- pos_percent_df
colnames(pos_percent_ESP_df)[1] <- "Fecha"
colnames(pos_percent_ESP_df)[2] <- "Porcentaje de pruebas que son positivas"
colnames(pos_percent_ESP_df)[3] <- "Promedio de 7 dias"
## WRITE CSV
#write.csv(pos_percent_ESP_df, row.names=FALSE, "pos_percent_esp.csv")
########################################################################################################
## 30 DAY PERIOD - POS PERCENNT (ESP)
## 1.5) POSITIVE PROPORTION - ESP
pos_percent_ESP_sub <- pos_percent_df_sub
colnames(pos_percent_ESP_sub)[1] <- "Fecha"
colnames(pos_percent_ESP_sub)[2] <- "Porcentaje de pruebas que son positivas"
colnames(pos_percent_ESP_sub)[3] <- "Promedio de 3 dias"
## WRITE CSV
#write.csv(pos_percent_ESP_sub, row.names=FALSE, "pos_percent_30day_esp.csv")
########################################################################################################
## 2) TEST COUNT - (ESP)
test_count_ESP_df <- test_count_df
## CLEAN COLUMN NAMES
colnames(test_count_ESP_df)[1] <- "Fecha"
colnames(test_count_ESP_df)[2] <- "Total de Pruebas"
test_count_ESP_df <- test_count_ESP_df[,c("Fecha", "Total de Pruebas")]
## WRITE CSV (ESP)
#write.csv(test_count_ESP_df, row.names=FALSE, "test_count_esp.csv")
########################################################################################################
## 30 DAY PERIOD - TESTING (ESP)
test_count_ESP_sub <- test_count_ESP_df
test_count_ESP_sub <- head(test_count_ESP_sub, 30); dim(test_count_ESP_sub)

## WRITE CSV
#write.csv(test_count_ESP_sub, row.names=FALSE, "test_count_30day_esp.csv")
########################################################################################################
##(3) DAILY COUNTY CASES, HOSPITALIZATIONS, DEATHS
#CREATE COMPLETE DATE DF
start_date = as.Date("2020-03-10")
end_date = Sys.Date()-1
complete_date_df <- tibble(
  Date = seq.Date(min(start_date),
                          max(end_date),
                          by = "day"))

complete_date_df <- data.frame(complete_date_df)
########################################################################################################
## DAILY DEATH COUNTS
ceders_death2 <- ceders_death[,c("outcome", "deathdate")]
ceders_death2$reporteddate <- ceders_death2$deathdate
ceders_death2$deathdate <- NULL
ceders_death2$death_count = 1
########################################################################################################
## 3. SUMMARIZE COUNT POSITIVES, NEGATIVES, TOTAL TESTS, AND PROP POSITIVES BY DATE
day_summ <- data.frame(group_by(ceders, reporteddate) %>% 
                         summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                   prob_sum = sum(casestatus=="probable"),
                                   total_pos_case_sum = sum(pos_confirm_sum,prob_sum)))
########################################################################################################
## 4. POSITIVE DATA MERGED WITH DEATH DATA
## IMPORTANT MAKE SURE DATES ARE ALL YYYY-MM-DD
summ_death_pos <- merge(x= day_summ, y=ceders_death2, by=c("reporteddate"), all.x=T)
## REPLACE NA WITH 0 FOR DEATH COUNT
summ_death_pos$outcome= NULL
summ_death_pos[is.na(summ_death_pos)]<- 0
########################################################################################################
## 5. ADD HOSPITALIZATION DF WITH SUMM_DEATH_ADD
colnames(co_hosp)[5]<-"admission_date"
co_hosp$Date <- format(as.Date(co_hosp$admission_date, "%Y-%m-%d"), "%Y-%m-%d")
co_hosp$freq=1
co_hosp_summ <- data.frame(group_by(co_hosp, Date) %>% 
                         summarize(hosp_count = sum(freq=="1")))
co_hosp_summ$Date <- as.Date(format(as.Date(co_hosp_summ$Date, "%Y-%m-%d"), "%Y-%m-%d"))

## COMPLETE TIME SERIES
co_hosp_summ <-  co_hosp_summ %>%
  complete(Date = seq(cutoff_index_date, curr_ceders, by ="day")) 
hosp_comp <- subset(co_hosp_summ, !is.na(co_hosp_summ$Date))
hosp_comp[is.na(hosp_comp)] <-0
hosp_comp <- data.frame(hosp_comp) 
#####################################################################################################
## OUTBREAK COMPLETE TIME SERIES
        ob_comp <-  ob_summ %>%
             complete(Date = seq(cutoff_index_date, curr_ceders, by = 'day')) 
          ob_comp <- data.frame(ob_comp) 
          ob_comp[is.na(ob_comp)] <-0
          ob_comp = arrange(ob_comp, Date)
########################################################################################################
## CLEAN UP CASE DATE NAME
    colnames(summ_death_pos)[1]<-"Date"
## CASE & DEATH DATA
    summ_death_pos$Date <- as.Date(format(as.Date(summ_death_pos$Date, "%Y-%m-%d"), "%Y-%m-%d"))
    cd_comp <-  summ_death_pos %>%
    complete(Date = seq(cutoff_index_date, end_date, by = 'day')) 
    cd_comp <- data.frame(cd_comp) 
    cd_comp[is.na(cd_comp)] <-0
    cd_comp = arrange(cd_comp, Date) 
########################################################################################################
##MERGE IN OB DATA
          cd_comp2 <- merge(x=cd_comp, y=ob_comp,by="Date", all.x=T)
########################################################################################################        
## MERGE IN HOSPITAL DATA
          cd_comp3 <- merge(x=cd_comp2, y=hosp_comp,by="Date", all.x=T)
## CORRECT NAs
          cd_comp3[is.na(cd_comp3)]<-0
########################################################################################################
## COMPILE DAILY DATA
          compiled_df1 <- cd_comp3[,c("Date","total_pos_case_sum","death_count", "hosp_count", "outbreak_sum")]
          ## EDIT: 2022-02-16
          compiled_df1 <- compiled_df1[!duplicated(compiled_df1[1]),]
          
########################################################################################################
## CLEAN UP VARIABLE (ENG)
colnames(compiled_df1)[1] <- "Date"
colnames(compiled_df1)[2] <- "Cases"
colnames(compiled_df1)[3] <- "Deaths"
colnames(compiled_df1)[4] <- "Hospitalizations"
colnames(compiled_df1)[5] <- "Outbreaks"
## SORT BY TEMP ID
compiled_df_sub <- tail(compiled_df1, 30)
## WRITE CSV (ENG)
write.csv(compiled_df1,row.names=FALSE, "daily_count_eng.csv")
## WRITE CSV 30-DAY PERIOD
write.csv(compiled_df_sub,row.names=FALSE, "daily_count_30d_eng.csv")
#####################################################################################
## CREATE SPANISH FILE
compiled_df_esp <- compiled_df1
## CHANGE VARIABLE NAMES TO SPANISH
colnames(compiled_df_esp)[1] <- "Fecha"
colnames(compiled_df_esp)[2] <- "Casos"
colnames(compiled_df_esp)[3] <- "Muertes"
colnames(compiled_df_esp)[4] <- "Hospitalizaciones"
colnames(compiled_df_esp)[5] <- "Brotes"
## WRITE CSV (ESP)
write.csv(compiled_df_esp,row.names=FALSE, "daily_count_esp.csv")
## 30-DAY SUB (ESP)
compiled_df_esp_sub <- tail(compiled_df_esp, 30)
write.csv(compiled_df_esp_sub,row.names=FALSE, "daily_count_30d_esp.csv")
##############################################################################################
##(4) DEMOGRAPHICS SUMMARY
## EXCLUDE UNKNOWN SEX 
ceders$exclude = 0
ceders$exclude[which(ceders$gender=="Unknown")] <- "1"
##############################################################################################
## AVOID RISK OF REPORTING NON-BINARY GENDERS IN DATA PUBLIC TO COMMUNITY  
## SUBSET & SUMMARIZE BY 1) AGE, 2) SEX, 3) RACE/ETHNIC
ceders <- subset(ceders, exclude=="0")
## MADE gv$A DUMMY VARIABLE FOR INDEX MEASUREMENTS BY DEMOGRAPHICS
ceders$index_measure=1
##############################################################################################
## AGE CLASS
## ASSIGN AGE CLASS VARIABLE IN BINS OF 10 YEARS
ceders$age_group = 0
ceders$age_group[which(ceders$age_at_reported < 10)] <- "0-9"
ceders$age_group[which(ceders$age_at_reported > 9 & ceders$age_at_reported < 20)] <- "10-19"
ceders$age_group[which(ceders$age_at_reported > 19 & ceders$age_at_reported < 30)] <- "20-29"
ceders$age_group[which(ceders$age_at_reported > 29 & ceders$age_at_reported < 40)] <- "30-39"
ceders$age_group[which(ceders$age_at_reported > 39 & ceders$age_at_reported < 50)] <- "40-49"
ceders$age_group[which(ceders$age_at_reported > 49 & ceders$age_at_reported < 60)] <- "50-59"
ceders$age_group[which(ceders$age_at_reported > 59 & ceders$age_at_reported < 70)] <- "60-69"
ceders$age_group[which(ceders$age_at_reported > 69 & ceders$age_at_reported < 80)] <- "70-79"
ceders$age_group[which(ceders$age_at_reported >= 80)] <- "80+"
##############################################################################################
## SUMMARIZE BY AGE CLASS
age_summ <- data.frame(group_by(ceders, age_group) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))
## PREPARE AGE HEADER FOR .CSV
colnames(age_summ)[1] <- "Metric"
colnames(age_summ)[2] <- "Number of Cases"
age_summ$Group = "Age Group"
age_summ <- age_summ[,c("Group", "Metric", "Number of Cases")]

age_summ_esp <- age_summ
age_summ_esp$Group = "Edad de Grupo"
names(age_summ_esp) <- c("Grupo","Metrico","Casos")
#write.csv(age_summ,row.names=FALSE, "age_summ_eng.csv")
###############################################################################
## SUMMARIZE AGE GROUP - 28 DAYS OF DATA
age_gr_daily <- data.frame(group_by(ceders, age_group, reporteddate) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))
age_gr_daily_sub <- subset(age_gr_daily, reporteddate > rec_28d_date)

age_gr_sub_summ <- data.frame(group_by(age_gr_daily_sub, age_group) %>% 
                             summarize(sum_cases = sum(sum_cases)))

## WRITE.CSV (ENG)
colnames(age_gr_sub_summ)[1] <- "Metric"
colnames(age_gr_sub_summ)[2] <- "Number of Cases"
age_gr_sub_summ$Group = "Age Group"
age_gr_sub_summ <- age_gr_sub_summ[,c("Group","Metric","Number of Cases")]
age_gr_sub_summ_esp <- age_gr_sub_summ
names(age_gr_sub_summ_esp)<-c("Grupo","Metrico","Casos")
age_gr_sub_summ_esp$Grupo="Grupo de Edad"
##############################################################################################
## SEX
sex_summ <- data.frame(group_by(ceders, gender) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))
## SUBSET ONLY CISS (ENG)
sex_summ <- subset(sex_summ, gender == "Female"  | gender == "Male")

## CLEAN UP NAMES (ENG)
colnames(sex_summ)[1] <- "Metric" 
colnames(sex_summ)[2] <- "Number of Cases"
sex_summ$Group = "Sex"
sex_summ <- sex_summ[,c("Group","Metric","Number of Cases")]
## HIST (ESP)
sex_summ_esp <- sex_summ
sex_summ_esp$Group = "Sexo"
names(sex_summ_esp) <- c("Grupo","Metrico","Casos")
sex_summ_esp$Metrico <- c("Mujeres","Hombres")

## WRITE CSV (ENG)
#write.csv(sex_summ,row.names=FALSE, "sex_summ_eng.csv")
##############################################################################################
## CLEAN UP NAMES (ESP)
## WRITE .CSV
#write.csv(sex_summ_esp,row.names=FALSE, "sex_summ_esp.csv")
####################################################################################
## SEX SUM (28 DAYS) 
sex_sum_28d <- subset(ceders, reporteddate > rec_28d_date)
sex_summ_daily <- data.frame(group_by(sex_sum_28d, gender) %>% 
                               summarize(sum_cases = sum(index_measure=="1")))

## WRITE.CSV (ENG)
#write.csv(sex_summ_daily, "sex_case_28day_eng.csv", row.names = FALSE)

## CLEAN UP VARS
sex_summ_daily$Group = "Sex"
sex_summ_daily<- sex_summ_daily[c("Group","gender","sum_cases")]
names(sex_summ_daily) <- c("Group","Metric","Number of Cases")

sex_summ_daily_esp <- sex_summ_daily
names(sex_summ_daily_esp)<-c("Grupo","Metrico","Casos")
sex_summ_daily_esp$Grupo = "Sexo"
sex_summ_daily_esp$Metrico = c("Mujeres","Hombres")


##############################################################################################
## CREATE NEW VARIABLE (race_ethnic) FOR RACE+ETHNICITY 
ceders$race_ethnic = 0
ceders$race_ethnic[which(ceders$single_race_ethnicity_with_ciis=="White - Non Hispanic")] <- "White"
ceders$race_ethnic[which(ceders$single_race_ethnicity_with_ciis=="Hispanic, All Races")] <- "Hispanic/Latinx"
ceders$race_ethnic[which(ceders$single_race_ethnicity_with_ciis=="Unknown")] <- "Unknown"
ceders$race_ethnic[which(ceders$single_race_ethnicity_with_ciis!="Hispanic, All Races" & ceders$single_race_ethnicity_with_ciis!="White - Non Hispanic" & ceders$single_race_ethnicity_with_ciis!="Unknown")] <- "Other"
## SUMMARIZE BY RACE & ETHNICITY
race_summ <- data.frame(group_by(ceders, race_ethnic) %>% 
                          summarize(sum_cases = sum(index_measure=="1")))
## CLEAN UP VARS
race_summ$Group = "Race/Ethnicity"
colnames(race_summ)[1]<-"Metric"
colnames(race_summ)[2]<-"Number of Cases"

race_summ <- race_summ[,c("Group","Metric","Number of Cases")]

## RACE (ESP)
race_summ_esp <- race_summ
names(race_summ_esp)<- c("Grupo","Metrico","Casos")
race_summ_esp$Metrico = c("hispano/latinx","otro", "desconocido", "blanco")
##############################################################################################
## WRITE CSV (ENG)
#write.csv(race_summ,row.names=FALSE, "race_summ_eng.csv")
##############################################################################################
## CLEAN UP NAMES (ESP)

## PULL ONLY MEANINGFUL VARIABLES
#race_summ_esp <- race_summ_esp[,c("race_ethnic_esp", "sum_cases")]
## WRITE CSV (ESP)
#write.csv(race_summ_esp, row.names=FALSE, "race_summ_esp.csv")
##############################################################################################
## SUMMARIZE RACE/ETHNICITY LAST 28 DAYS 
race_daily_sub <- subset(ceders, reporteddate > rec_28d_date)

## SUMMARIZE RACE/ETHNIC DAILY
race_sub_daily <- data.frame(group_by(ceders, race_ethnic, reporteddate) %>% 
                             summarize(sum_cases = sum(index_measure=="1")))
## SUBSET RACE-DAILY BY CUTOFF
race_sub_daily_sub <- subset(race_sub_daily, reporteddate > rec_28d_date)
## RACE/ETHNICITY - 28 DAY SUMMARY
race_ethnic_28d_summ <- data.frame(group_by(race_sub_daily_sub, race_ethnic) %>% 
                                summarize(sum_cases = sum(sum_cases)))

race_ethnic_28d_summ$Group = "Race/Ethnicity"
colnames(race_ethnic_28d_summ)[1]<-"Metric"
colnames(race_ethnic_28d_summ)[2]<-"Number of Cases"
race_ethnic_28d_summ <- race_ethnic_28d_summ[,c("Group","Metric","Number of Cases")]
#################################################################################
## WRITE.CSV (ENG)
#write.csv(race_ethnic_28d_summ, "race_summ_28day_eng.csv", row.names=F)
## CLEAN UP NAMES (ESP)
race_ethnic_28d_esp <- race_ethnic_28d_summ
race_ethnic_28d_esp$Group <- "raza/etnicidad"
names(race_ethnic_28d_esp) <-  c("Grupo","Metrico","Casos")
race_ethnic_28d_esp$Metrico <-  c("hispano/latinx","otros","desconocido","blanco")
names(race_ethnic_28d_esp)<- c("Grupo","Metrico","Casos")
#write.csv(race_ethnic_28d_esp, "race_summ_28day_esp.csv", row.names=F)
##############################################################################################
## COMPILE DEMOGRAPHIC CASE DATA 
## (1) HISTORICAL CUMULATIVE DATA
hist_demographic_case_eng <- rbind(age_summ,race_summ,sex_summ)
write.csv(hist_demographic_case_eng, "hist_demographic_case_eng.csv", row.names=F)
hist_demographic_case_esp <- rbind(age_summ_esp,race_summ_esp,sex_summ_esp)
write.csv(hist_demographic_case_esp, "hist_demographic_case_esp.csv", row.names=F)
## (2) 28-DAY DATA (ENG)
demographic_case_28d_eng <- rbind(age_gr_sub_summ,race_ethnic_28d_summ,sex_summ_daily)
write.csv(demographic_case_28d_eng, "demographic_case_28d_eng.csv", row.names=F)
## (ESP)
demographic_case_28d_esp <- rbind(age_gr_sub_summ_esp,race_ethnic_28d_esp,sex_summ_daily_esp)
write.csv(demographic_case_28d_esp, "demographic_case_28d_esp.csv", row.names=F)

##############################################################################################
##PON MILESTONE #1 - HOSPITAL OCCUPANCY & HOSPITAL ADMISSIONS
## SUMMARIZE DAILY % HOSPITAL BED OCCUPANCY (N, BEDS = 34) 
colnames(sasmc)[1]<-"Date"
sasmc$max_threshold= 80.0
hosp_occ <- sasmc[,c("Date","prop_occ","max_threshold")]
hosp_occ$min = 0
hosp_occ$max = 100
##############################################################################################
## CREATE PROXY TEMP_ID 
hosp_occ$sort_ID <- as.integer(factor(with(hosp_occ, paste(Date))))
##############################################################################################
## HISTORICAL HOSPITAL OCCUPANCY (ENG)
hosp_occ <- na.omit(hosp_occ)
colnames(hosp_occ)[1] <- "Date"
colnames(hosp_occ)[2] <- "Bed Occupancy (%)"
colnames(hosp_occ)[3] <- "Goal (80% or lower)"
## WRITE CSV - HISTORICAL HOSPITAL OCCUPANCY
hosp_occ$sort_ID = NULL
#write.csv(hosp_occ,row.names=FALSE, "pon_hosp_occ_hist_eng.csv")
##############################################################################################
## HISTORICAL HOSPITAL OCCUPANCY (ESP)
hosp_occ_esp <- hosp_occ
colnames(hosp_occ_esp)[1] <- "Fecha"
colnames(hosp_occ_esp)[2] <- "Cupo de camas (%)"
colnames(hosp_occ_esp)[3] <- "Meta (80% o menos)"
## WRITE CSV - HISTORICAL HOSPITAL OCCUPANCY (ESP)
#write.csv(hosp_occ_esp,row.names=FALSE, "pon_hosp_occ_hist_esp.csv")
##############################################################################################
## SUBSET 8-WEEK DATA (ENG)
hosp_occ_8wk_eng <- tail(hosp_occ, 28)
#write.csv(hosp_occ_8wk_eng,row.names=FALSE, "pon_hosp_occ_8wk_eng.csv")
##############################################################################################
## SUBSET 8-WEEK DATA (ESP)
hosp_occ_8wk_esp <- tail(hosp_occ_esp, 28)
#write.csv(hosp_occ_8wk_esp,row.names=FALSE, "pon_hosp_occ_8wk_esp.csv")
##############################################################################################
##############################################################################################
##PON MILESTONE #2 - CUMULATIVE 7-D INCIDENCE PER 100,000
## COVID-19 7-DAY CUMULATIVE INCIDENCE (POSITIVE TESTS + PCR)
## PULL OUT MEANINGFUL VARIABLES 
incid_7d <- compiled_df1[,c("Date","Cases")]

## CORRECT DATE HEADER [UPDATED: 2022-03-22]
##incid_7d <- head(incid_7d,-1)

## CREATE PROXY TEMP_ID 
incid_7d$sort_ID <- as.integer(factor(with(incid_7d, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
incid_7d <- arrange(incid_7d, -sort_ID); head(incid_7d)
# ASSIGN ID FOR 7-DAY INTERVALS 
incid_7d$ID <-rep(1:100, each=7, length.out=nrow(incid_7d))
## FILTER MOST RECENT DATE TO FOLLOW CDPHE'S DIAL REPORTING
##pos_df<- tail(pos_df, -2)
##############################################################################################
## 7-DAY MOVING SUM
incid_7d <- incid_7d %>%
  arrange(sort_ID, Date) %>%
  mutate(rollapply_sum =zoo::rollapplyr(Cases, 7, sum, partial = TRUE)) # requires package zoom
##############################################################################################
##############################################################################################
## CUMULATIVE 7-DAY INCIDENCE (TOTAL NUMBER OF POSITIVE CASES PER 7D) 
## MUST MULTIPLE BY SUM OF POSITIVE CASES AND POPULATION FACTOR
## POPULATION VALUE IS TAKEN FROM CO DEMOGRAPHY 2018 POPULATION SIZE
incid_7d$pos_case_7d <- incid_7d$rollapply_sum*(100000/gv$TP)
##############################################################################################
## HISTORICAL INCIDENCE (ENG)
incid_7d_hist <- incid_7d
## PREPARE FILE FOR CSV
incid_7d_hist$min = 0
incid_7d_hist$target1=200
incid_7d_hist$max = 4000
incid_7d_hist <- incid_7d_hist[,c("Date","pos_case_7d","min","target1","max")]
colnames(incid_7d_hist)[2] <- "Rolling 7-Day Positive Cases per 100,000"
colnames(incid_7d_hist)[4] <- "Goal: fewer than 200 new cases per 100,000 people"

##WRITE CSV
write.csv(incid_7d_hist, row.names=FALSE,"pon_incidence_hist_eng.csv")
##############################################################################################
## HISTORICAL INCIDENCE (ESP)
incid_7d_hist_esp <- incid_7d_hist
colnames(incid_7d_hist_esp)[1] <- "Fecha"
colnames(incid_7d_hist_esp)[2] <- "positivos nuevos por 100,000 personas en los 7 dias anteriores"
colnames(incid_7d_hist_esp)[3] <- "min"
colnames(incid_7d_hist_esp)[4] <- "Meta: menos de 200 casos positivos nuevos por 100,000 personas"
## WRITE CSV
write.csv(incid_7d_hist_esp, row.names=FALSE,"pon_incidence_hist_esp.csv")
##############################################################################################
## SUBSET 8-WEEK DATA
incid_7d_sub <- arrange(incid_7d, -sort_ID)
## SELECT FIRST 28 ROWS (8WKS OF DATA)
incid_7d_sub <- head(incid_7d_sub, 28)
## PREPARE FILE FOR CSV
incid_7d_sub$target1 = 200
incid_7d_sub$target2 = 400

#incid_7d_sub$min=0
#incid_7d_sub$max = 250
incid_7d_sub <- incid_7d_sub[,c("Date","pos_case_7d","target1","target2")]
##############################################################################################
## 8-WEEK INCIDENCE (ENG) 
colnames(incid_7d_sub)[2] <- "Rolling 7-Day Positive Cases per 100,000"
colnames(incid_7d_sub)[3] <- "Level Green: Low"
colnames(incid_7d_sub)[4] <- "Level Orange: High"
## WRITE CSV (ENG)
write.csv(incid_7d_sub, row.names=FALSE,"pon_incidence_8wk_eng.csv")
##############################################################################################
## 8-WEEK INCIDENCE (ESP)
incid_7d_sub_esp <- incid_7d_sub
colnames(incid_7d_sub_esp)[1] <- "Fecha"
colnames(incid_7d_sub_esp)[2] <- "positivos nuevos por 100,000 personas en los 7 dias anteriores"

colnames(incid_7d_sub_esp)[3]<-"Nivel Verde: Bajo"
colnames(incid_7d_sub_esp)[4]<-"Nivel Anaranjado: Alto"
## WRITE CSV (ESP)
write.csv(incid_7d_sub_esp, row.names=FALSE,"pon_incidence_8wk_esp.csv")
##############################################################################################
## 7-DAY PCR POSITIVITY RATE
## CLEAN UP VARIABLE NAMES FOR SIMPLICITY
total_pcr_pos <- data.frame(total_pcr_pos)
colnames(total_pcr_pos)[1]<-"Date" # Date
colnames(total_pcr_pos)[2]<-"total_pcr" # TOTAL NUMBER OF PCR TESTS
colnames(total_pcr_pos)[3]<-"pos_pcr"   # NUMBER OF POSITIVE CONFIRMED TESTS
##############################################################################################
## REMOVE GARBAGE DATES 
total_pcr_pos$Date <- as.Date(total_pcr_pos$Date)
total_pcr_pos <- subset(total_pcr_pos, Date > start_date)
##############################################################################################
## CREATE PROXY ID FOR SORTING - DF SHOULD ALREADY BE ORDERED NEWEST TO OLDEST BUT JUST AS gv$A CHECK
total_pcr_pos$sort_ID <- as.integer(factor(with(total_pcr_pos, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
total_pcr_pos <- arrange(total_pcr_pos, -sort_ID); head(total_pcr_pos)
##############################################################################################
## 7-DAY MOVING SUM
total_pcr_pos <- total_pcr_pos %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_pos_sum =zoo::rollapplyr(pos_pcr, 7, sum, partial = TRUE))  %>%
  mutate(rollapply_total_sum =zoo::rollapplyr(total_pcr, 7, sum, partial = TRUE))
##############################################################################################
## 1)7-DAY POSITIVITY RATE 
total_pcr_pos$pos_pcr_rate <- (total_pcr_pos$rollapply_pos_sum/total_pcr_pos$rollapply_total_sum)*100
total_pcr_pos$pcr_pos_target1 = 5
total_pcr_pos$pcr_pos_target2 = 7.5
total_pcr_pos$pcr_pos_target3 = 10
total_pcr_pos$min = 0
## ARRANGE NEWEST TO OLDEST DATE
total_pcr_pos <- arrange(total_pcr_pos, -sort_ID)
##############################################################################################
## HISTORICAL DATA (ENG)
pcr_pos_perc_hist_eng <- total_pcr_pos[,c("Date","pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
pcr_pos_perc_hist_eng$max = max(pcr_pos_perc_hist_eng$pos_pcr_rate)+2
colnames(pcr_pos_perc_hist_eng)[2] <- "Rolling 7-Day Percent of Tests Positive"
colnames(pcr_pos_perc_hist_eng)[4] <- "Level Blue: Caution"
colnames(pcr_pos_perc_hist_eng)[5] <- "Level Yellow: Concern"
colnames(pcr_pos_perc_hist_eng)[6] <- "Level Orange: High Risk"
colnames(pcr_pos_perc_hist_eng)[7] <- "Level Red: Severe Risk"
## WRITE CSV (ENG)
#write.csv(pcr_pos_perc_hist_eng, row.names=FALSE,"pon_pos_rate_hist_eng.csv")
##############################################################################################
## HISTORICAL DATA (ESP)
pcr_pos_perc_hist_esp <- total_pcr_pos[,c("Date","pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
pcr_pos_perc_hist_esp$max = max(pcr_pos_perc_hist_esp$pos_pcr_rate)+2
colnames(pcr_pos_perc_hist_esp)[1] <- "Fecha"
colnames(pcr_pos_perc_hist_esp)[2] <- "Porcentaje de pruebas positivas durante los 7 dias anteriores"
colnames(pcr_pos_perc_hist_esp)[4] <- "Nivel Azul: Precacion"
colnames(pcr_pos_perc_hist_esp)[5] <- "Nivel Amarillo: Preocupaction"
colnames(pcr_pos_perc_hist_esp)[6] <- "Nivel Anaranjado: Alto Riesgo"
colnames(pcr_pos_perc_hist_esp)[7] <- "Nivel Rojo: Reisgo severo"
## WRITE CSV (ESP)
#write.csv(pcr_pos_perc_hist_esp, row.names=FALSE,"pon_pos_rate_hist_esp.csv")
##############################################################################################
## SUBSET OUT LAST 8 WEEKS
pcr_pos_8wk <- head(total_pcr_pos, 28)
##############################################################################################
## 8-WEEK POSITIVITY (ENG)
pcr_pos_8wk <- pcr_pos_8wk[,c("Date", "pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
pcr_pos_8wk$max = 50
## CLEAN UP NAMES (ENG)
colnames(pcr_pos_8wk)[2] <- "Rolling 7-Day Percent of Tests Positive"
colnames(pcr_pos_8wk)[4] <- "Level Blue: Caution"
colnames(pcr_pos_8wk)[5] <- "Level Yellow: Concern"
colnames(pcr_pos_8wk)[6] <- "Level Orange: High Risk"
colnames(pcr_pos_8wk)[7] <- "Level Red: Severe Risk"
##  WRITE CSV (ENG)
write.csv(pcr_pos_8wk,row.names=FALSE, "pon_pos_rate_8wk_eng.csv")
##############################################################################################
## 8-WEEK POSITIVITY (ESP)
pcr_pos_perc_esp <- pcr_pos_8wk
colnames(pcr_pos_perc_esp)[1] <- "Fecha"
colnames(pcr_pos_perc_esp)[2] <- "Porcentaje de pruebas positivas durante los 7 dias anteriores"
colnames(pcr_pos_perc_esp)[4] <- "Nivel Azul: Precacion"
colnames(pcr_pos_perc_esp)[5] <- "Nivel Amarillo: Preocupaction"
colnames(pcr_pos_perc_esp)[6] <- "Nivel Anaranjado: Alto Riesgo"
colnames(pcr_pos_perc_esp)[7] <- "Nivel Rojo: Reisgo severo"
##  WRITE CSV (ESP)
write.csv(pcr_pos_perc_esp, row.names=FALSE,"pon_pos_rate_8wk_esp.csv")
##############################################################################################
##TESTING RATE PER 1,000
## CLEAN UP VARIABLE NAMES FOR SIMPLICITY
testing <- test_count_df
colnames(testing)[2]<-"total_pcr" 
colnames(testing)[1]<-"Date" 
## SELECT OUT VARIABLES OF INTEREST
testing <- testing[,c("Date", "total_pcr")]
##############################################################################################
## CREATE PROXY ID FOR SORTING
testing$sort_ID <- as.integer(factor(with(testing, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
testing <- arrange(testing, -sort_ID); head(testing)
##############################################################################################
## 7-day MOVING TESTING AVERAGE
testing <- testing %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_7d_avg =zoo::rollapplyr(total_pcr, 7, mean, partial = TRUE)) # need package zoom
#TESTING POPULATION FACTOR PROPORTION
PF_1K = (1000/gv$TP) 
## 7 DAY AVG TESTING RATE 
testing$test_rate7d <- (testing$rollapply_7d_avg)*PF_1K
## DAILY TESTING RATE
testing$test_rate_daily<- (testing$total_pcr)*PF_1K
## TESTING POPULATION FACTOR PER 0.75, 1,000 PEOPLE
testing$test_target = 0.75
##############################################################################################
## SORT DATE NEWEST TO OLDEST
testing <- arrange(testing, -sort_ID)
testing$max = max(testing$test_rate_daily)+2
testing$min = 0
##############################################################################################
## HISTORICAL TESTING (ENG)
pcr_test_hist_eng <- testing[,c("Date","test_rate_daily", "test_rate7d", "test_target", "min", "max")]
colnames(pcr_test_hist_eng)[2] <- "Daily Testing Conducted per 1,000 population"
colnames(pcr_test_hist_eng)[3] <- "7 Day Moving Average"
colnames(pcr_test_hist_eng)[4] <- "Goal (0.75 or higher)"
## WRITE CSV - HISTORICAL TESTING (ENG)
#write.csv(pcr_test_hist_eng, row.names=FALSE,"pon_test_rate_hist_eng.csv")
##############################################################################################
## HISTORICAL TESTING (ESP)
pcr_test_hist_esp <- pcr_test_hist_eng
colnames(pcr_test_hist_esp)[1] <- "Fecha"
colnames(pcr_test_hist_esp)[2] <- "Pruebas por 1,000 Personas"
colnames(pcr_test_hist_esp)[3] <- "Promedio de 7 dias"
colnames(pcr_test_hist_esp)[4] <- "Meta (0.75 o mayor)"
## WRITE CSV - HISTORICAL TESTING (ENG)
#write.csv(pcr_test_hist_esp, row.names=FALSE,"pon_test_rate_hist_esp.csv")
##############################################################################################
## SUBSET RECENT 8-WEEK DATA
testing_8wk <- head(pcr_test_hist_eng, 28)
##############################################################################################
## WRITE CSV
#write.csv(testing_8wk, row.names=FALSE,"pon_test_rate_8wk_eng.csv")
##############################################################################################
## 8-WEEK TESTING (ESP)
pcr_test_esp <- head(pcr_test_hist_esp, 28)
colnames(pcr_test_esp)[1] <- "Fecha"
colnames(pcr_test_esp)[2] <- "Pruebas por 1,000 Personas"
colnames(pcr_test_esp)[3] <- "Promedio de 7 dias"
colnames(pcr_test_esp)[4] <- "Meta (0.75 o mayor)"
## WRITE CSV
#write.csv(pcr_test_esp, row.names=FALSE,"pon_test_rate_8wk_esp.csv")
##############################################################################################
##CREATE TABLE OF MOST RECENT ONE-DAY SUMMARY
curr_7day <-tail(compiled_df1, 7)
max_7d = max(curr_7day$Date)

#curr_7day$Date <- as.Date(format(as.Date(curr_7day$Date, "%Y-%m-%d"), "%m/%d/%y"))
curr_7day$Date = as.Date(curr_7day$Date)
## CURRENT DAY SUMMARY TABLE
curr_day <- tail(curr_7day, 1)

## CREATE MIN/MAX DATES
min_7d = format(min(curr_7day$Date), "%m/%d/%y")
max_7d = format(max(curr_7day$Date), "%m/%d/%y")

## INCIDENCE
incid_rec <- tail(incid_7d, 1)
curr_day$`Incidence per 100,000`= signif(incid_rec$pos_case_7d, digits=4)
# POSITVITY (%) duck
pos_rate_rec <- head(pcr_pos_8wk, 1)
curr_day$`Positivity (%)`= signif(pos_rate_rec[,2], digits=3)
## MAKE TABLE DATA EYE-FRIENDLY
curr_day$Date = format(curr_day$Date, "%m/%d/%y")
## COMPILE GGTABLE
curr_day_table <- ggtexttable(curr_day, rows=NULL,
                                  theme = ttheme("lBlackWhite",base_size=8)) 

## CREATE AUTO-BARPLOT TITLE
auto_bar_title <- paste("Summit County Cases: ",min_7d," ", "to", " ", max_7d, sep = "")
case7d_max = max(curr_7day$Cases)+2
## BARPLOT OF LAST 7 DAYS OF CASES
case7d_barplot <- ggplot(curr_7day, aes(x=Date, y=Cases)) + 
  geom_bar(stat="identity", color="black")+
  ylim(0,case7d_max)+
  geom_text(aes(label=Cases), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_x_date(date_labels = "%b-%d", date_breaks= "1 day")+
  ggtitle(auto_bar_title)+
  theme_bw(base_size = 12)
#################################################################################
## ARRANGE PLOT + TABLE SUMMARY REPORT
summ_report_1 <-  ggarrange(case7d_barplot, curr_day_table,
                        ncol = 1, nrow = 2,
                        heights = c(2, 0.8))
#################################################################################
## SAVE DAILY SUMMARY TO PDF
ggexport(summ_report_1,filename = "SCPH_Daily_COVID-19_Report.pdf")
#################################################################################
## SASMC DATA (ESP)
sasmc_esp <- sasmc_report
colnames(sasmc_esp)[1]<-"Fecha"
colnames(sasmc_esp)[2]<-"Paciente COVID-19"
colnames(sasmc_esp)[3]<-"Traslados de pacientes COVID-19"
#write.csv(sasmc_esp, "sasmc_historical_esp.csv", row.names=F)
#################################################################################
## 30-DAY PERIOD (ENG)
sasmc_report_30d_eng <- tail(sasmc_report, 30)
## WRITE CSV (ENG)
write.csv(sasmc_report_30d_eng, "sasmc_30d_eng.csv", row.names=F)
## WRITE CSV (ESP)
sasmc30d_esp <- sasmc_esp
sasmc30d_esp <- tail(sasmc30d_esp, 30)
## WRITE CSV (ESP)
write.csv(sasmc30d_esp, "sasmc_30d_esp.csv", row.names=F)
#########################################################################################################################
## VACCINE SUMMARY 
## FREQ VARIABLE
vax$dose_count =1
## FILTER ROW DUPLICATES
vax$DUP_ROW_FILTER_STRING <- as.integer(factor(with(vax, paste(patient_first_name, patient_last_name, patient_dob,vaccination_date, clinic_id, clinic_desc, dose_count, vaccination_code, clinic_county, gender_code, age_at_1stvaccination, race_ethnicity, patient_street_number, patient_street_prefix,patient_street_name,patient_street_name,patient_street_suffix,patient_address,patient_city))))
vax <- subset(vax, !duplicated(DUP_ROW_FILTER_STRING))
#############################################################################################################################
## FILTER BY SUMMIT COUNTY
vax <- subset(vax, patient_county=="SUMMIT" | patient_county=="Summit")
#############################################################################################################################
## AGE CLASS
## ASSIGN AGE CLASS VARIABLE IN BINS OF 10 YEARS
vax$age_class = 0
vax$age_class[which(vax$age_at_1stvaccination < 10)] <- "0-9"
vax$age_class[which(vax$age_at_1stvaccination > 9 & vax$age_at_1stvaccination < 20)] <- "10-19"
vax$age_class[which(vax$age_at_1stvaccination > 19 & vax$age_at_1stvaccination < 30)] <- "20-29"
vax$age_class[which(vax$age_at_1stvaccination > 29 & vax$age_at_1stvaccination < 40)] <- "30-39"
vax$age_class[which(vax$age_at_1stvaccination > 39 & vax$age_at_1stvaccination < 50)] <- "40-49"
vax$age_class[which(vax$age_at_1stvaccination > 49 & vax$age_at_1stvaccination < 60)] <- "50-59"
vax$age_class[which(vax$age_at_1stvaccination > 59 & vax$age_at_1stvaccination < 70)] <- "60-69"
vax$age_class[which(vax$age_at_1stvaccination > 69 & vax$age_at_1stvaccination < 80)] <- "70-79"
vax$age_class[which(vax$age_at_1stvaccination >= 80)] <- "80+"
## ADD RACE GROUP
vax$race_ethnic=0
vax$race_ethnic[which(vax$race_ethnicity=="American Indian or Alaskan Native - Non Hispanic")]<-"Native American/Alaskan Native"
vax$race_ethnic[which(vax$race_ethnicity=="Black or African American - Non Hispanic")]<-"Black/African American"
vax$race_ethnic[which(vax$race_ethnicity=="Multi Race - Non Hispanic Native Hawaiian or Other Pacific Islander - Non Hispanic")]<-"Asian/NHPI"
#vax$race_ethnic[which(vax$race_ethnicity=="Multi Race - Non Hispanic")]<-"Other"
vax$race_ethnic[which(vax$race_ethnicity=="Other")]<-"Other"
vax$race_ethnic[which(vax$race_ethnicity=="White - Non Hispanic")]<-"White"
vax$race_ethnic[which(vax$race_ethnicity=="Asian - Non Hispanic")]<-"Asian/NHPI"
vax$race_ethnic[which(vax$race_ethnicity=="Hispanic, All Races")]<-"Hispanic/Latinx"
vax$race_ethnic[which(vax$race_ethnicity=="Native Hawaiian or Other Pacific Islander - Non Hispanic")]<-"Asian/NHPI"
## CLEAN UP ZIP CODE VARIABLE
vax$zip <- vax$zip_code
vax$zip[which(startsWith(vax$zip, "80435"))]<-"80435"
vax$zip[which(startsWith(vax$zip, "80424"))]<-"80424"
vax$zip[which(startsWith(vax$zip, "80498"))]<-"80498"
vax$zip[which(startsWith(vax$zip, "80497"))]<-"80497"
vax$zip[which(startsWith(vax$zip, "80443"))]<-"80443"

#############################################################################################################################
## MODIFY KID VACCINE ("COVID AGE 5-11 ORANGE CAP" TO "COVID-19 mRNA (PFR)")
vax$vaccination_code[which(vax$vaccination_code == "COVID AGE 5-11 ORANGE CAP")] <- "COVID-19 mRNA (PFR)"
#############################################################################################################################
## VACCINE TYPE
## CORRECT PFR RENAMING
vax$vaccination_code[which(vax$vaccination_code=="COVID 12+yrs GRAY CAP")] <-   "COVID-19 mRNA (PFR)"
vax$vaccination_code[which(vax$vaccination_code=="COVID 12+yrs PURPLE CAP")] <- "COVID-19 mRNA (PFR)"

vax$vaccine_level = 0
vax$vaccine_level[which(vax$vaccination_code =="COVID-19 Vector-NR (JSN)" | vax$dose_count=="2")] <- "1"
## 1 DOSE PH
vax$p1=0
vax$p1[which(vax$vaccination_code=="COVID-19 mRNA (PFR)" & vax$dose_count == "1")] <- "1"
## 2 DOSE PH
vax$p2=0
vax$p2[which(vax$vaccination_code=="COVID-19 mRNA (PFR)" & vax$dose_count == "2")] <- "1"
## 3 DOSE PH
vax$p3=0
vax$p3[which(vax$vaccination_code=="COVID-19 mRNA (PFR)" & vax$dose_count == "3")] <- "1"

## DOSE 3 MO
vax$p4=0
vax$p4[which(vax$vaccination_code=="COVID-19 mRNA (PFR)" & vax$dose_count == "4")] <- "1"

## 1 DOSE MO
vax$m1=0
vax$m1[which(vax$vaccination_code=="COVID-19 mRNA (MOD)" & vax$dose_count == "1")] <- "1"
## 2 DOSE MO
vax$m2=0
vax$m2[which(vax$vaccination_code=="COVID-19 mRNA (MOD)" & vax$dose_count == "2")] <- "1"
## DOSE 3 MO
vax$m3=0
vax$m3[which(vax$vaccination_code=="COVID-19 mRNA (MOD)" & vax$dose_count == "3")] <- "1"

## DOSE 4 MO
vax$m4=0
vax$m4[which(vax$vaccination_code=="COVID-19 mRNA (MOD)" & vax$dose_count == "4")] <- "1"

## 1 DOSE JJ
vax$j1=0
vax$j1[which(vax$vaccination_code=="COVID-19 Vector-NR (JSN)" & vax$dose_count == "1")] <- "1"
## 2 DOSE JJ
vax$j2=0
vax$j2[which(vax$vaccination_code=="COVID-19 Vector-NR (JSN)" & vax$dose_count == "2")] <- "1"
## DOSE 3 JJ
vax$j3=0
vax$j3[which(vax$vaccination_code=="COVID-19 Vector-NR (JSN)" & vax$dose_count == "3")] <- "1"

## CREATE BOOSTER VARIABLES
vax$booster_1=0
vax$booster_1[which(vax$j2=="1" | vax$m3 == "1" |vax$p3 == "1")] <- "1"
vax$booster_2=0
vax$booster_2[which(vax$j3=="1" | vax$m4 == "1" |vax$p4 == "1")] <- "1"
#############################################################################################################################
## SUMMARIZE TOTAL DOSES
dose_sum <- as.data.frame(group_by(vax,) %>%
                            summarize(sum_p1 = sum(p1=="1"),
                                      sum_p2 = sum(p2=="1"),
                                      sum_p3 = sum(p3=="1"),
                                      sum_p4 = sum(p4=="1"),
                                      sum_m1 = sum(m1=="1"),
                                      sum_m2 = sum(m2=="1"),
                                      sum_m3 = sum(m3=="1"),
                                      sum_m4 = sum(m4=="1"),
                                      
                                      sum_j1 = sum(j1=="1"),
                                      sum_j2 = sum(j2=="1"),
                                      sum_j3 = sum(j3=="1"),
                                      
                                      sum_booster_1 = sum(booster_1=="1"), 
                                      sum_booster_2 = sum(booster_2=="1"), 
                                      
                                      total_doses = sum(sum_p1,sum_p2, sum_m1, sum_m2, sum_j1,sum_j2,sum_p3,sum_m3,sum_p4,sum_m4,sum_booster_1,sum_booster_2),
                                      sum_full = sum(sum_p2, sum_m2, sum_j1),
                                      diff_p = sum_p1-sum_p2,
                                      diff_m = sum_m1-sum_m2,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/gv$AP*100,
                                      vaccinated_perc = (sum_full/gv$AP)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))
#############################################################################################################################
## SUMMARIZE DOSES BY PATIENT-ID
## SUMMARIZE TOTAL DOSES 
dose_sum_pi <- as.data.frame(group_by(vax,patient_id) %>%
                               summarize(sum_p = sum(p1 =="1"),
                                         sum_m = sum(m1 =="1"),
                                         sum_j = sum(j1 =="1"),
                                         sum_pm = sum(sum_p,sum_m),
                                         sum_pj = sum(sum_p,sum_j),
                                         sum_mj = sum(sum_m,sum_j),
                                         total_dose = sum(sum_p,sum_m,sum_j)))
################################################################################################################################
## UP-TO-DATE (ogs) [2022-04-23; 2 (series 2-dose/ 1-dose + 1 booster)
## CDC SOURCE: https://www.cdc.gov/coronavirus/2019-ncov/vaccines/stay-up-to-date.html?s_cid=11747:cdc%20up%20to%20date%20vaccine:sem.ga:p:RG:GM:gen:PTN:FY22
dose_sum_pi$ogs = 0 
dose_sum_pi$ogs[which(dose_sum_pi$sum_p >= 3 | dose_sum_pi$sum_m >= 3 | dose_sum_pi$sum_j >= 2 | dose_sum_pi$sum_pm >= 3)] <- "1"
dose_sum_pi$ogs[which(dose_sum_pi$sum_j >= 1 | dose_sum_pi$total_dose >= 2)] <- "1"
## AT LEAST ONE DOSE (AL1) 
dose_sum_pi$al1 = 0 
dose_sum_pi$al1[which(dose_sum_pi$total_dose > 0)] <- "1"
## ORIGINAL SERIES (2022-04-29)
dose_sum_pi$og_series=0
dose_sum_pi$og_series[which(dose_sum_pi$sum_p >= 2 | dose_sum_pi$sum_m >= 2 | dose_sum_pi$sum_j >= 1)] <- "1"

dose_sum_pi$booster_1 = 0
## P/M - BOOSTER 1
dose_sum_pi$booster_1[which(dose_sum_pi$sum_pm >= gv$mp_vax_booster_1)] <- "1"
## JJ - BOOSTER 1
dose_sum_pi$ad[which(dose_sum_pi$sum_j >= 1 & dose_sum_pi$total_dose >= gv$jj_vax_booser_1)] <- "1"
dose_sum_pi$booster_2 = 0
## P/M - BOOSTER 1
dose_sum_pi$booster_2[which(dose_sum_pi$sum_pm >= gv$mp_vax_booster_2)] <- "1"
## JJ - BOOSTER 1
dose_sum_pi$booster_2[which(dose_sum_pi$sum_j >= 1 & dose_sum_pi$total_dose >= gv$jj_vax_booser_2)] <- "1"
################################################################################################################################
## SUMMARIZE PATIENT VAX HISTORY FOR VAX TABLE 
dose_sum_pi$partial_dose_prop = signif((al1/gv$AP)*100,digits=3)

dose_sum_pi[is.na(dose_sum_pi)] <- 0


ogs_summ <- as.data.frame(group_by(dose_sum_pi,) %>%
                            summarize(sum_al1       =  sum(al1=="1"),
                                      sum_ogs       =  sum(ogs=="1"),
                                      sum_og_series =  sum(og_series=="1"),
                                      sum_b1        =  sum(booster_1=="1"),
                                      sum_b2        =  sum(booster_2=="1"),
                                      prop_ogs      =  round((sum_og_series/gv$AP)*100,digits=1),
                                      prop_al1      =  round((sum_al1/gv$AP)*100,digits=1),
                                      prop_ogs      =  round((sum_ogs/gv$AP)*100,digits=1),
                                      prop_b1       =  round(sum_b1/gv$APB*100,digits=1),
                                      prop_b2       =  round(sum_b2/gv$AP50*100,digits=1)))
#############################################################################################################################
## (1) TOTAL VACCINE SUMMARY TABLE
tp_names <- c("Level of Vaccine Coverage", "Cumulative Count", "Percent of Residents Vaccinated")

tp_al1 <- c("At least 1 dose (ages: 5+)",ogs_summ$sum_al1, ogs_summ$prop_al1)
tp_ogs <- c("Original vaccine series (ages: 5+)",ogs_summ$sum_og_series, ogs_summ$prop_ogs)
tp_b1 <- c("Booster dose 1 (ages: 12+)",ogs_summ$sum_b1, ogs_summ$prop_b1)
tp_b2 <- c("Booster dose 2 (ages: 50+)",ogs_summ$sum_b2, ogs_summ$prop_b2)


tp_header <- c("Level of Vaccine Coverage", "Cumulative Count","Percent of Residents Vaccinated")

tp_names <- c("At least 1 dose (ages: 5+)","Original vaccine series (ages: 5+)","Booster dose 1 (ages: 12+)","Booster dose 2 (ages: 50+)")
tp_count <- c(ogs_summ$sum_al1, ogs_summ$sum_ogs, ogs_summ$sum_b1, ogs_summ$sum_b2)
tp_perc <- c(ogs_summ$prop_al1,ogs_summ$prop_ogs, ogs_summ$prop_b1,ogs_summ$prop_b2)

tp_comb <- data.frame(tp_names,tp_count,tp_perc)
names(tp_comb)<- tp_header

## WRITE CSV (ENG)
write.csv(tp_comb, "vaccine_table_coverage_eng.csv", row.names=F)
#############################################################################################################################
## (1) TOTAL % TABLE (ESP)
tp_nombres <- c("Al menos 1 dosis (edad: 5+)","Serie original de vacunas (edad: 5+)","Dosis de refuerzo 1", "Dosis de refuerzo 2")
tp_header_esp <- c("Nivel de cobertura vacunal", "Recuento acumulativo","Porcentaje de residentes vacunados")

tp_comb_esp <- data.frame(tp_nombres,tp_count,tp_perc)
names(tp_comb_esp)<- tp_header_esp
## WRITE CSV (ESP)
write.csv(tp_comb_esp, "vaccine_table_coverage_esp.csv", row.names=F)
#############################################################################################################################
## SUMMARIZE VAX DATA BY DEMOGRAPHICS ## 
demographic_vax <- vax[,c("patient_id", "age_class","race_ethnic","zip","gender_code")]
## MERGE DEMOGRAPHICS WITH PATIENT LEVEL VAX HISTORY
dose_sum_pi2 <- merge(x = dose_sum_pi, y = demographic_vax, by = c("patient_id"))
## FILTER DUPLICATES
dose_sum_pi3 <- subset(dose_sum_pi2, !duplicated(dose_sum_pi2$patient_id))
## PROXY COUNT VAR
dose_sum_pi3$count = 1
#############################################################################################################################
## SUMMARIZE DEMOGRAPHCIS 
vax_polished_names <- c("At least 1 dose","Original vaccine series", "Booster dose 1", "Booster dose 2")  

##############################################################################
## SUMMARIZE SEX
dose_sum_pi_sex <- as.data.frame(group_by(dose_sum_pi3,gender_code) %>%
                            summarize(sum_al1       =  sum(al1=="1"),
                                      sum_ogs       =  sum(ogs=="1"),
                                      sum_og_series =  sum(og_series=="1"),
                                      sum_b1        =  sum(booster_1=="1"),
                                      sum_b2        =  sum(booster_2=="1")))

sum_pi_sex <- subset(dose_sum_pi_sex, gender_code == "M" | gender_code == "F")

sum_pi_sex$census_pop = gv$TP
sum_pi_sex$census_prop = c(0.453,0.547)
sum_pi_sex$count_estimate = sum_pi_sex$census_pop*sum_pi_sex$census_prop
sum_pi_sex$al1_prop = round(sum_pi_sex$sum_al1/sum_pi_sex$count_estimate*100, digits=1)
sum_pi_sex$ogs_prop = round(sum_pi_sex$sum_ogs/sum_pi_sex$count_estimate*100, digits=1)
sum_pi_sex$b1_prop = round(sum_pi_sex$sum_b1/sum_pi_sex$count_estimate*100, digits=1)
sum_pi_sex$b2_prop = round(sum_pi_sex$sum_b2/sum_pi_sex$count_estimate*100, digits=1)

## PULL OUT VARS OF INTEREST
vax_sex <- sum_pi_sex[,c("gender_code","al1_prop","ogs_prop","b1_prop", "b2_prop")]
## MALE
vax_sex_m <- subset(vax_sex, gender_code=="M")
vax_sex_m$gender_code = "Male"
#names(vax_sex_m) <- c("Sex","At least 1 dose", "Up to date","Additional dose")
## FEMALE
vax_sex_f <- subset(vax_sex, gender_code=="F")
vax_sex_f$gender_code = "Female"
#names(vax_sex_f) <- c("Sex","At least 1 dose", "Up to date","Additional dose")
## CLEAN UP OUTPUT VARIABLES
vax_comp_sex <- rbind(vax_sex_f,vax_sex_m)
colnames(vax_comp_sex)[1] <- "Metric"
vax_comp_sex$Group = "Sex"
vax_comp_sex <- vax_comp_sex[,c("Group","Metric","al1_prop","ogs_prop", "b1_prop","b2_prop")]

## WRITE CSV (ENG)
#write.csv(vax_comp_sex, "vax_sex_eng.csv", row.names=F)
## WRITE CSV (ESP) 
vax_comp_sex_esp <- vax_comp_sex
names(vax_comp_sex_esp) <- c("Grupo","Metrico", "al1_prop","ogs_prop", "b1_prop", "b2_prop")
vax_comp_sex_esp$Metrico = c("Mujeres", "Hombres")
vax_comp_sex_esp$Grupo="Sexo"
#write.csv(vax_comp_sex_esp, "vax_sex_esp.csv", row.names=F)
##############################################################################
## SUMMARIZE AGE GROUP  
dose_sum_pi_age <- as.data.frame(group_by(dose_sum_pi3,age_class) %>%
                                   summarize(sum_al1      =  sum(al1=="1"),
                                             sum_ogs      =  sum(ogs=="1"),
                                             sum_b1       =  sum(booster_1=="1"),
                                             sum_b2       =  sum(booster_2=="1")))



# 
dose_sum_pi_age$census_age_prop <- c(gv$c09, gv$c19, gv$c29, gv$c39, gv$c49, gv$c59, gv$c69, gv$c79, gv$c80)
dose_sum_pi_age$al1_prop <- round(dose_sum_pi_age$sum_al1/dose_sum_pi_age$census_age*100)
dose_sum_pi_age$ogs_prop <- round(dose_sum_pi_age$sum_ogs/dose_sum_pi_age$census_age*100)
dose_sum_pi_age$b1_prop <- round(dose_sum_pi_age$sum_b1/dose_sum_pi_age$census_age*100)
dose_sum_pi_age$b2_prop <- round(dose_sum_pi_age$sum_b2/dose_sum_pi_age$census_age*100)

## CORRECT FOR VALUES OVER 100
#dose_sum_pi_age$vax_prop_al1[which(dose_sum_pi_age$vax_prop_al1 > 99.9)] <- 99
#dose_sum_pi_age$vax_prop_ogs[which(dose_sum_pi_age$vax_prop_ogs > 99.9)] <- 99
#dose_sum_pi_age$vax_prop_ad[which(dose_sum_pi_age$vax_prop_ad > 99.9)] <- 99
## PULL VARS OF INTEREST
dose_sum_pi_age$Group = "Age"
colnames(dose_sum_pi_age)[1]<-"Metric"
dose_sum_pi_age <- dose_sum_pi_age[,c("Group", "Metric","al1_prop","ogs_prop","b1_prop","b2_prop")]
## PREP ESP VARS
dose_sum_pi_age_esp <- dose_sum_pi_age
colnames(dose_sum_pi_age_esp)[1]<-"Grupo"
colnames(dose_sum_pi_age_esp)[2]<-"Metrico"
dose_sum_pi_age_esp$Grupo = "Edad"
## RACE/ETHNICITY
dose_sum_pi_race <- as.data.frame(group_by(dose_sum_pi3,race_ethnic) %>%
                                   summarize(sum_al1      =  sum(al1=="1"),
                                             sum_ogs      =  sum(ogs=="1"),
                                             sum_b1       =  sum(booster_1=="1"),
                                             sum_b2       =  sum(booster_2=="1")))



vax_race_sub <- subset(dose_sum_pi_race, race_ethnic != "0" & race_ethnic != "Other")
vax_race_sub$census_count <- c(gv$A_NHPI,gv$B,gv$H,gv$AI,gv$W)

vax_race_sub$al1_prop <- round(vax_race_sub$sum_al1/vax_race_sub$census_count*100)
vax_race_sub$ogs_prop <- round(vax_race_sub$sum_ogs/vax_race_sub$census_count*100)
vax_race_sub$b1_prop <- round(vax_race_sub$sum_b1/vax_race_sub$census_count*100)
vax_race_sub$b2_prop <- round(vax_race_sub$sum_b2/vax_race_sub$census_count*100)

## PULL OUT VARS OF INTEREST
vax_race_sub <- vax_race_sub[,c("race_ethnic","al1_prop","ogs_prop","b1_prop","b2_prop")]
vax_race_sub$Group = "Race/Ethnicity"
colnames(vax_race_sub)[1]<-"Metric"
vax_race_sub <- vax_race_sub[,c("Group","Metric","al1_prop","ogs_prop","b1_prop","b2_prop")]



vax_race_sub_esp <- vax_race_sub
vax_race_sub_esp$Metric <- c("asiatica/NHPI", "negra/afroamericana", "hispanica/Latinx", "nativa americana/india americana", "blanca")
colnames(vax_race_sub_esp)[1]<-"Grupo"
colnames(vax_race_sub_esp)[2]<-"Metrico"
vax_race_sub_esp$Grupo = "Raza/Etnicidad" 

### VAX SUMMARY ZIP CODE 
dose_sum_pi_zip <- as.data.frame(group_by(dose_sum_pi3,zip) %>%
                                    summarize(sum_al1      =  sum(al1=="1"),
                                              sum_ogs      =  sum(ogs=="1"),
                                              sum_b1       =  sum(booster_1=="1"),
                                              sum_b2       =  sum(booster_2=="1")))

zip_vax_df <- subset(dose_sum_pi_zip, zip == "80498" | zip == "80497" | zip == "80424" | zip == "80435" | zip == "80443")

zip_vax_df$census_zip_count <- c(gv$c80424, gv$c80435, gv$c80443, gv$c80497,gv$c80498)
zip_vax_df$al1_prop <- round(zip_vax_df$sum_al1/zip_vax_df$census_zip_count*100)
zip_vax_df$ogs_prop <- round(zip_vax_df$sum_ogs/zip_vax_df$census_zip_count*100)
zip_vax_df$b1_prop <- round(zip_vax_df$sum_b1/zip_vax_df$census_zip_count*100)
zip_vax_df$b2_prop <- round(zip_vax_df$sum_b2/zip_vax_df$census_zip_count*100)

## PULL VARS OF INTEREST
zip_vax_sub <- zip_vax_df[,c("zip","al1_prop","ogs_prop","b1_prop","b2_prop")]
colnames(zip_vax_sub)[1]<- "Metric"
zip_vax_sub$Group = "Zip Code"
zip_vax_sub <- zip_vax_sub[,c("Group","Metric","al1_prop","ogs_prop","b1_prop","b2_prop")]
zip_vax_sub_esp <- zip_vax_sub
colnames(zip_vax_sub_esp)[2]<-"Metrico"
colnames(zip_vax_sub_esp)[1]<-"Grupo"
zip_vax_sub_esp$Grupo="Codigo de Zip"
## COMPILE DEMOGRAPHIC DATA

comp_demo_vax <- rbind(dose_sum_pi_age,vax_race_sub,vax_comp_sex,zip_vax_sub)
## CLEAN UP VALUES ABOVE 99 
#comp_demo_vax$al1_prop[which(comp_demo_vax$al1_prop > 99)] <- 99
##comp_demo_vax$ogs_prop[which(comp_demo_vax$ogs_prop > 99)] <- 99
#comp_demo_vax$ad_prop[which(comp_demo_vax$ad_prop > 99)] <- 99

## CLEAN UP HEADER
names(comp_demo_vax) <- c("Group","Metric", "At least 1 dose", "Original vaccine series","Booster dose 1", "Booster dose 2")
## WRITE.CSV (ENG)
write.csv(comp_demo_vax, "vax_demographics_eng.csv", row.names=F)
## WRITE CSV (ESP)
comp_demo_vax_esp <- rbind(dose_sum_pi_age_esp,vax_race_sub_esp,vax_comp_sex_esp,zip_vax_sub_esp)
names(comp_demo_vax_esp) <- c("Grupo","Metrico", "Al menos 1 dosis", "Serie original de vacunas","Dosis de refuerzo 1","Dosis de refuerzo 2")
write.csv(comp_demo_vax_esp, "vax_demographics_esp.csv", row.names=F) 
#############################################################################################################################
## MILESTONE SUMMARY TABLE
## INCIDENCE
incid <- tail(incid_7d, 1)
incid$Status = 0
incid$Status[which(incid$pos_case_7d > 500)] <- "Level Orange: High Risk"
incid$Status[which(incid$pos_case_7d <= 500)] <- "Level Yellow: Concern"
incid$Status[which(incid$pos_case_7d <= 250)] <- "Level Blue: Caution"
incid$Status[which(incid$pos_case_7d <= 100)] <- "Level Green: Little to No Threat"

incid$Milestone="7-day Cumulative Incidence Rate"
incid$Metric <- format(round(incid$Metric, 1), nsmall = 1)
colnames(incid)[6]<-"Metric"
incid <- incid[,c("Milestone","Metric","Status")]
## POSITIVITY 
#pos_rate <- head(pcr_pos_8wk, 1)
#pos_rate$Status = 0
#pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` >= 10)] <- "Level Red: Severe Risk"
#pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive`< 10)] <- "Level Orange: High Risk"
##pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` <= 7.5)] <- "Level Yellow: Concern"
#pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` <= 5)] <- "Level Blue: Caution"
#pos_rate$Milestone="7-day Average Positivity (%)"
#pos_rate <- pos_rate[,c(9,2,8)]
#colnames(pos_rate)[2]<-"Metric"
#pos_rate$Metric <- format(round(pos_rate$Metric, 1), nsmall = 1)
###################################################################################################################
## HOSPITAL OCCUPANCY
hosp_occ_perc<- tail(sasmc, 1) 
hosp_occ_perc$Status = 0
hosp_occ_perc$Status[which(hosp_occ_perc$prop_occ > 80 | hosp_occ_perc$prop_occ == 80)] <- "Level Red: Severe Risk"
hosp_occ_perc$Status[which(hosp_occ_perc$prop_occ < 80)] <- "Level Blue: Caution"
hosp_occ_perc$Milestone = "Hospital Occupancy (%)"
colnames(hosp_occ_perc)[8]<-"Metric"
hosp_occ_perc$Metric <- format(round(hosp_occ_perc$Metric, 1), nsmall = 1)

hosp_occ_perc <- hosp_occ_perc[,c("Milestone","Metric","Status")]
##########################################################################################################################

## OUTBREAK SUMMARY 


ob2 <- ob 
####################################################################################################
## CONVERT DATE
ob2$Full_Date <- format(as.Date(ob2$Full_Date, "%m/%d/%Y"), "%Y-%m-%d")
ob2$Date <- ob2$Full_Date
ob2$Date <- format(as.Date(ob2$Date, "%Y-%m-%d"), "%m-%Y")
ob2$freq=1
## SUMMARIZE NUMBER OF OUTBREAKS BY M-YYYY
ob_summ <- data.frame(group_by(ob2, Date) %>% 
                        summarize(outbreak_sum = sum(freq=="1")))
## RENAME OB DATE
colnames(ob_summ)[1]<-"Date"

##################################################################################################################
## DIAGNOSTIC TEST SUMMARY
##############################################################################################
## Summarize sum of  tests by dateadded
diag_tests <- data.frame(group_by(elr_df, dateadded, test_type) %>%
                            summarize(total_tested = sum(n_tests))); dim(total_tests)


diag_tests_wide <- diag_tests %>%
  spread(dateadded, test_type, total_tested)

pcr_tests <- subset(diag_tests, test_type =="PCR"); dim(pcr_tests)
pcr_tests$test_type = NULL
colnames(pcr_tests)[2]<-"PCR"

ser_tests <- subset(diag_tests, test_type =="serology"); dim(ser_tests)
ser_tests$test_type = NULL
colnames(ser_tests)[2]<-"Serology"

ant_tests <- subset(diag_tests, test_type =="antigen"); dim(ant_tests)
ant_tests$test_type = NULL
colnames(ant_tests)[2]<-"Antigen"
################################################################################
## 7-DAY AVERAGES
pcr_tests$avg_7d <- stats::filter(pcr_tests$PCR, rep(1/7,7)); head(pcr_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
pcr_tests$sort_ID <- as.integer(factor(with(pcr_tests, paste(dateadded))))
pcr_tests <- arrange(pcr_tests, -sort_ID); head(pcr_tests)

## SHIFT FUNCTION
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
## SHIFT UP
pcr_tests$avg_7d <- shift(pcr_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
pcr_tests$avg_7d[is.na(pcr_tests$avg_7d)] <- 0
pcr_tests$sort_ID=NULL
## RENAME VARIABLES
colnames(pcr_tests)[2]<-"Molecular"
colnames(pcr_tests)[3]<-"Molecular 7-day Average"
####################################################################################
## SEROLOGY TEST - 7DAY AVG CALCUALTIONS
ser_tests$avg_7d <- stats::filter(ser_tests$Serology, rep(1/7,7)); head(ser_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
ser_tests$sort_ID <- as.integer(factor(with(ser_tests, paste(dateadded))))
ser_tests <- arrange(ser_tests, -sort_ID); head(ser_tests)

## SHIFT UP
ser_tests$avg_7d <- shift(ser_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
ser_tests$avg_7d[is.na(ser_tests$avg_7d)] <- 0
ser_tests$sort_ID=NULL
colnames(ser_tests)[3]<-"Serology 7-day Average"
####################################################################################
## SEROLOGY TEST - 7DAY AVG CALCUALTIONS
ant_tests$avg_7d <- stats::filter(ant_tests$Antigen, rep(1/7,7)); head(ant_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
ant_tests$sort_ID <- as.integer(factor(with(ant_tests, paste(dateadded))))
ant_tests <- arrange(ant_tests, -sort_ID); head(ant_tests)

## SHIFT UP
ant_tests$avg_7d <- shift(ant_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
ant_tests$avg_7d[is.na(ant_tests$avg_7d)] <- 0
ant_tests$sort_ID=NULL
colnames(ant_tests)[3]<-"Antigen 7-day Average"
####################################################################################
## COMPILE TESTS
comp_tests <- merge(x=pcr_tests,y=ser_tests,by=c("dateadded"),all.x=T);dim(comp_tests)
comp_tests2 <- merge(x=comp_tests,y=ant_tests,by=c("dateadded"),all.x=T);dim(comp_tests)
df[is.na(df)] <- 0

comp_tests2[is.na(comp_tests2)] <- 0
comp_tests3 <- subset(comp_tests2, dateadded > "2020-03-12")
colnames(comp_tests3)[1] <-"Date"
## WRITE.CSV (ENG)
write.csv(comp_tests3, "diagnostic_testing_hist_eng.csv", row.names=F)
comp_tests3_30d <- tail(comp_tests3, 30)
write.csv(comp_tests3_30d, "diagnostic_testing_30d_eng.csv", row.names=F)
####################################################################################
## TRANSLATE TO ESP
comp_tests3_esp <- comp_tests3
##RENAME VARIABELS
colnames(comp_tests3_esp)[1]<-"Fecha"
colnames(comp_tests3_esp)[3]<-"Promedio molecular de 7 dias"
colnames(comp_tests3_esp)[4]<-"Serologia"
colnames(comp_tests3_esp)[5]<-"Promedio serologia de 7 dias"
colnames(comp_tests3_esp)[6]<-"Antigeno"
colnames(comp_tests3_esp)[7]<-"Promedio serologia de 7 dias"
## WRITE CSV (ESP - HIST)
write.csv(comp_tests3_esp, "diagnostic_testing_hist_esp.csv", row.names=F)
comp_tests3_esp_30d <- tail(comp_tests3_esp, 30)
write.csv(comp_tests3_esp_30d, "diagnostic_testing_30d_esp.csv", row.names=F)
##############################################################################################
## VARIANT SUMMARY 
vd <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1jz1kV3WIepOHOSiOrgdfE-CnIo-Kz1YX0B8HBw4YwmU/edit#gid=0')
vd <- as.data.frame(vd)
vd <- vd[,c(3,2)]

colnames(vd)[1]<-"date"
vd$count = 1

###############################################################################################################
## CHANGE VARIABLE NAMES
colnames(vd)[2]<-"raw"
vd$variant <- 0

vd$variant[which(vd$raw=="B.1.1.7")] <- "B.1.1.7 (Alpha)"
vd$variant[which(vd$raw=="B.1.351")] <- "B.1.351 (Beta)"
vd$variant[which(vd$raw=="P.1")] <- "P.1 (Gamma)"
vd$variant[which(vd$raw=="B.1.617.2")] <- "B.1.617.2 (Delta)"
vd$variant[which(vd$raw=="B.1.427/B.1.429")] <- "B.1.427/B.1.429 (Epsilon)"
vd$variant[which(vd$raw=="B.1.617.1")] <- "B.1.617.1 (Kappa)"
vd$variant[which(vd$raw=="B.1.525")] <- "B.1.525 (Eta)"
vd$variant[which(vd$raw=="B.1.526")] <- "B.1.526 (Iota)"
vd$variant[which(vd$raw=="AY.1")] <- "B.1.617.2 (Delta)"
vd$variant[which(vd$raw=="AY.2")] <- "B.1.617.2 (Delta)"
vd$variant[which(vd$raw=="B.1.621")] <- "B.1.621"


vd$variant[which(vd$raw=="Other")] <- "Other"

vd$categorization = 0
vd$categorization[which(vd$raw=="B.1.427")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.429")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.525")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.526")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.617.1")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.617.1")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="P.2")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="B.1.427/B.1.429")] <- "Variant of Interest"
vd$categorization[which(vd$raw=="Other")] <- "Variant of Interest"

## VARIANTS OF CONCERN
vd$categorization[which(vd$raw=="B.1.1.7")] <- "Variant of Concern"
vd$categorization[which(vd$raw=="B.1.351")] <- "Variant of Concern"
vd$categorization[which(vd$raw=="B.1.617.2")] <- "Variant of Concern"
vd$categorization[which(vd$raw=="P.1")] <- "Variant of Concern"
vd$categorization[which(vd$raw=="B.1.621")] <- "Variant of Concern"

###############################################################################################################
## SUMMARIZE TOTAL VARIANTS
total_var <- data.frame(group_by(vd, variant) %>% 
                          summarize(count_sum = sum(count=="1"))); print(total_var)

## CLASSIFICATION
total_var$categorization = 0
total_var$categorization[which(total_var$variant=="B.1.525 (Eta)")] <- "Variant of Interest"
total_var$categorization[which(total_var$variant=="B.1.526 (Iota)")] <- "Variant of Interest"
total_var$categorization[which(total_var$variant=="B.1.617.1 (Kappa)")] <- "Variant of Interest"
total_var$categorization[which(total_var$variant=="P.2 (Zeta)")] <- "Variant of Interest"
total_var$categorization[which(total_var$variant=="B.1.427/B.1.429 (Epsilon)")] <- "Variant of Interest"
total_var$categorization[which(total_var$variant=="Other")] <- "Variant of Interest"

## VARIANTS OF CONCERN
total_var$categorization[which(total_var$variant=="B.1.1.7 (Alpha)")] <- "Variant of Concern"
total_var$categorization[which(total_var$variant=="B.1.351 (Beta)")] <- "Variant of Concern"
total_var$categorization[which(total_var$variant=="B.1.617.1 (Kappa)")] <- "Variant of Concern"
total_var$categorization[which(total_var$variant=="B.1.617.2 (Delta)")] <- "Variant of Concern"
total_var$categorization[which(total_var$variant=="AY.1")] <- "Variant of Concern"

total_var$categorization[which(total_var$variant=="P.1 (Gamma)")] <- "Variant of Concern"
total_var$categorization[which(total_var$variant=="B.1.621")] <- "Variant of Concern"

###############################################################################################################
## REARRANGE COLUMNS
## CHANGE COLUMN NAMES
colnames(total_var)[1]<-"Variant"
colnames(total_var)[2]<-"Number of Cases"
colnames(total_var)[3]<-"Variant Classification"
## WRITE CSV (ENG)
write.csv(total_var, "variant_total_eng.csv", row.names=F)
###############################################################################################################
## SUMMARIZE TOTAL VARIANTS
vd2<- vd


## CONVERT DATE FORMAT
vd2$date <- as.Date(format(as.Date(vd2$date, "%m/%d/%Y"), "%Y-%m-%d"))

daily_var <- data.frame(group_by(vd, date, variant) %>% 
                          summarize(count_sum = sum(count=="1")))

daily_var$date <- as.Date(format(as.Date(daily_var$date, "%m/%d/%Y"), "%Y-%m-%d"))

## SUBSET LAST 30-DAY PERIOD
daily_var$sort_ID <- as.integer(factor(with(daily_var, paste(date))))
## SORT BY TEMP ID
daily_var <- arrange(daily_var, -sort_ID); head(daily_var)## SUBSET OUT 28 DAYS

daily_var2 <- daily_var %>%
  complete(date = seq(date[1], Sys.Date(), by = "1 day"),
           fill = list(Val1 = 0, Val2 = 0))
##############################################################################################
## VARIANT TIME SERIES ANALYSIS GRAPHS
daily_var_ts <- daily_var2
daily_var_ts <- data.frame(daily_var_ts)
daily_var_ts[is.na(daily_var_ts)] <- 0
daily_var_ts$delta_variant = 0
daily_var_ts$delta_variant[which(daily_var_ts$variant=="B.1.617.2 (Delta)")]<-"1"
daily_var_ts$date_month <- daily_var_ts$date
#daily_var_ts$date_month <- as.Date(format(as.Date(daily_var_ts$date_month, "%Y-%m-%d"), "%Y-%m"))
daily_var_ts$date_month <- as.character(daily_var_ts$date_month)
daily_var_ts$date_month = substr(daily_var_ts$date_month,1,nchar(daily_var_ts$date_month)-3)
## VARIANT CATEGORY
daily_var_ts$variant_present = 0
daily_var_ts$variant_present[daily_var_ts$count_sum > 0] <- "1"
## store df for daily analysis
dd_var_ts <- daily_var_ts
## MONTHLY SUMMARY - TOTAL VARIANT
daily_var_ts2 <- subset(daily_var_ts, count_sum > 0)
daily_var_ts2$date_month <- as.factor(daily_var_ts2$date_month)

month_var_ts <- data.frame(group_by(daily_var_ts2, date_month, delta_variant) %>% 
                          summarize(count_sum = sum(count_sum)))

month_var_ts$variant_type = "Delta Variant"
month_var_ts$variant_type[which(month_var_ts$delta_variant=="0")] = "Non-delta Variant"

## write.csv
month_var_ts$delta_variant=NULL
month_var_tsm = month_var_ts %>%
  spread(variant_type, count_sum)

month_var_tsm[is.na(month_var_tsm)] <- 0

#############################################################################################
## DAILY VARIANT TIME SERIES ANALYSIS
daily_var_ts <- data.frame(group_by(dd_var_ts, date, delta_variant) %>% 
                             summarize(count_sum = sum(count_sum)))

daily_var_ts$variant_type = "Delta Variant"
daily_var_ts$variant_type[which(daily_var_ts$delta_variant=="0")] = "Non-delta Variant"

## SUBSET OUT 28 DAY
daily_var_ts2 <- tail(daily_var_ts, 28)
daily_var_ts2$delta_variant=NULL
daily_var_ts3 = daily_var_ts2 %>%
  spread(variant_type, count_sum)
daily_var_ts3[is.na(daily_var_ts3)] <- 0
## SPREAD HISTORICAL DATA
daily_var_ts$delta_variant=NULL
daily_var_tsh = daily_var_ts %>%
  spread(variant_type, count_sum)
daily_var_tsh[is.na(daily_var_tsh)] <- 0

## MERGE CASE DATA
cd_d1 <- cd1[,c("Date", "Cases")]
colnames(daily_var_tsh)[1]<-"Date"
daily_var_tsh$Date <- as.character(daily_var_tsh$Date)
comp_cv <- merge(x=cd_d1, y=daily_var_tsh,by=c("Date"))
comp_cv$prop_dv = comp_cv$`Delta Variant`/comp_cv$Cases*100
comp_cv$prop_ndv = comp_cv$`Non-delta Variant`/comp_cv$Cases*100
comp_cv2 <- comp_cv[,c("Date", "prop_dv", "prop_ndv")]
comp_cv2$total_var = comp_cv2$prop_dv+comp_cv2$prop_ndv
comp_cv3<- comp_cv2[,c("Date", "prop_dv", "prop_ndv")]
colnames(comp_cv3)[2]<-"Delta Variant (%)"
colnames(comp_cv3)[3]<-"Non-delta Variants (%)"

comp_cv3_sub28 <- tail(comp_cv3, -28)

## write.csv
write.csv(comp_cv3_sub28, "variant_daily_28d_time_series.csv", row.names=F)
write.csv(comp_cv3, "variant_daily_historical_time_series.csv", row.names=F)
#############################################################################################
## LONG TO WIDE
daily_var3 = daily_var2 %>%
  spread(variant, count_sum)

## FORMAT TO DF
daily_var3 <- as.data.frame(daily_var3)
daily_var3$county = NULL
daily_var3$sort_ID = NULL

## CLEAN OUT VARS
daily_var3 <- daily_var3[,c(1:8)]
## REPLACE NA'S
daily_var3[is.na(daily_var3)]<-0
## ARRANGE VARIABLES REVERSE ORDER
## SUBSET LAST 30-DAY PERIOD
daily_var3$sort_ID <- as.integer(factor(with(daily_var3, paste(date))))
## SORT BY TEMP ID
daily_var3 <- arrange(daily_var3, -sort_ID); head(daily_var3)## SUBSET OUT 28 DAYS

daily_var3_sub <- head(daily_var3, 28)

## STORE NAMES
names.var = names(daily_var3_sub[,(2:8)])
var_class = total_var$`Variant Classification`
# TEMP RENAME VARIABLES
colnames(daily_var3_sub)[2]<-"a"
colnames(daily_var3_sub)[3]<-"ep"
colnames(daily_var3_sub)[4]<-"io"
colnames(daily_var3_sub)[5]<-"d"
colnames(daily_var3_sub)[8]<-"g"

sub_var_summ <- data.frame(group_by(daily_var3_sub,) %>% 
                             summarize(a.sum = sum(a),
                                       ep.sum = sum(ep),
                                       io.sum = sum(io),
                                       d.sum = sum(d),
                                       b.1.621.sum=sum(B.1.621),
                                       other.sum = sum(Other),
                                       g.sum= sum(g)))


## MAKE ROUGH TABLE
var.count = c(sub_var_summ$a.sum,  sub_var_summ$ep.sum,  sub_var_summ$io.sum,  sub_var_summ$d.sum,  sub_var_summ$b.1.621.sum,  sub_var_summ$other.sum,  sub_var_summ$g.sum)

sub.var = data.frame(names.var,var.count,var_class)
## CLEAN UP VARIABLE NAMES
colnames(sub.var)[1]<-"Variant"
colnames(sub.var)[2]<-"Number of Cases"
colnames(sub.var)[3]<-"Variant Classification"
## WRITE.CSV (ENG)
write.csv(sub.var, "variant_sub28_eng.csv", row.names=F)
##############################################################################################
## VARIANT SUB 28 (ESP)
sub.var.esp = sub.var
sub.var.esp$cdv = sub.var.esp$`Variant Classification`
sub.var.esp$cdv[which(sub.var.esp$cdv=="Variant of Concern")] <- "Variante de Preocupacion"
sub.var.esp$cdv[which(sub.var.esp$cdv=="Variant of Interest")] <- "Variante de Interes"
sub.var.esp$`Variant Classification`= NULL
sub.var.esp$Variant[which(sub.var.esp$Variant=="Other")] <- "Otro" 
colnames(sub.var.esp)[1]<-"Variante"
colnames(sub.var.esp)[2]<-"Numero de Casos"
colnames(sub.var.esp)[3]<-"Clasificacion de Variantes"
## WRITE.CSV (ESP)
write.csv(sub.var.esp, "variant_sub28_esp.csv", row.names=F)
##############################################################################################
## HISTORICAL VARRIANT SUMMARY 
colnames(daily_var3)[2]<-"a"
colnames(daily_var3)[3]<-"ep"
colnames(daily_var3)[4]<-"io"
colnames(daily_var3)[5]<-"d"
colnames(daily_var3)[8]<-"g"


var_summ <- data.frame(group_by(daily_var3,) %>% 
                             summarize(a.sum = sum(a),
                                       ep.sum = sum(ep),
                                       io.sum = sum(io),
                                       d.sum = sum(d),
                                       other.sum = sum(Other),
                                       B.1.621.sum=sum(B.1.621),
                                       g.sum = sum(g),
                                       io.sum = sum(io),
                                       g.sum = sum(g)))
## MAKE ROUGH TABLE
var.count =     c(var_summ$a.sum,  var_summ$ep.sum,  var_summ$io.sum,  var_summ$d.sum,var_summ$B.1.621, var_summ$other.sum,  var_summ$g.sum)

var = data.frame(names.var,var.count,var_class)
## CLEAN UP VARIABLE NAMES
colnames(var)[1]<-"Variant"
colnames(var)[2]<-"Number of Cases"
colnames(var)[3]<-"Variant Classification"
## WRITE.CSV (ENG)
write.csv(var, "variant_historical_eng.csv", row.names=F)

##############################################################################################
## VARIANT SUB 28 (ESP)
var.esp = var
var.esp$cdv = var.esp$`Variant Classification`
var.esp$cdv[which(var.esp$cdv=="Variant of Concern")] <- "Variante de Preocupacion"
var.esp$cdv[which(var.esp$cdv=="Variant of Interest")] <- "Variante de Interes"
var.esp$`Variant Classification`= NULL
var.esp$Variant[which(var.esp$Variant=="Other")] <- "Otro" 
colnames(var.esp)[1]<-"Variante"
colnames(var.esp)[2]<-"Numero de Casos"
colnames(var.esp)[3]<-"Clasificacion de Variantes"
## WRITE.CSV (ESP)
write.csv(var.esp, "variant_historical_esp.csv", row.names=F)
##############################################################################################
## CASE SUMMARY DAY TABLE - 
## POSITIVITY
pos7d_avg = round(mean(head(pcr_pos_8wk$`Rolling 7-Day Percent of Tests Positive`, 7)),digits=1)
pos28d_avg = round(mean(head(pcr_pos_8wk$`Rolling 7-Day Percent of Tests Positive`, 28)),digits=1)

##TOTAL CASES
case7d_avg = round(mean(tail(compiled_df1$Cases, 7)),digits=1)
case28d_avg = round(mean(tail(compiled_df1$Cases, 28)),digits=1)
##HOSPITALIZATIONS
hosp7d_avg = round(mean(tail(compiled_df1$Hospitalizations, 7)),digits=1)
hosp28d_avg = round(mean(tail(compiled_df1$Hospitalizations, 28)),digits=1)
##DEATHS
death7d_avg = round(mean(tail(compiled_df1$Deaths, 7)),digits=1)
death28d_avg = round(mean(tail(compiled_df1$Deaths, 28)),digits=1)
##OBs
ob7d_avg = round(mean(tail(compiled_df1$Outbreaks, 7)),digits=1)
ob28d_avg = round(mean(tail(compiled_df1$Outbreaks, 28)),digits=1)


Metric = c("Percent Positivity","Total Cases", "Hospitalizations", "Deaths Among Cases","Outbreaks")
C2 = c(pos7d_avg,case7d_avg,hosp7d_avg,death7d_avg,ob7d_avg)
C3 = c(pos28d_avg,case28d_avg,hosp28d_avg,death28d_avg,ob28d_avg)

comp_table <- data.frame(Metric, C2, C3)
comp_table$C4 = "No Change"
comp_table$C4[which(comp_table$C2 > comp_table$C3)] <- "Increasing"
comp_table$C4[which(comp_table$C2 < comp_table$C3)] <- "Decreasing"

## RENAME COLUMNS
colnames(comp_table)[2]<-"Last 7 Days Daily average"
colnames(comp_table)[3]<-"Last 28 Days Daily Average"
colnames(comp_table)[4]<-"Status"
## WRITE CSV (ENG)
write.csv(comp_table, "comparison_table_eng.csv", row.names=F)
##############################################################################################
## CONVERT TO ESP
compar_esp <- comp_table

colnames(compar_esp)[1]<-"Metrico"
colnames(compar_esp)[2]<-"Promedio (7 dias anteriores)"
colnames(compar_esp)[3]<-"Promedio (28 dias anteriores)"
colnames(compar_esp)[4]<-"Tendencia"
## change metric names
compar_esp$Metrico <- c("Positividad de la prueba", "Casos confirmados", "Hospitalizations", "Muertes", "Brotes")

compar_esp$Tendencia[which(compar_esp$Tendencia=="Increasing")] <- "Creciente"
compar_esp$Tendencia[which(compar_esp$Tendencia=="Decreasing")] <- "Decreciente"
compar_esp$Tendencia[which(compar_esp$Tendencia=="No Change")] <- "Ninguno"
## WRITE CSV (ESP)
write.csv(compar_esp, "comparison_table_esp.csv", row.names=F)
####################################################################################
## DAILY TOTAL SC RESIDENT HOSPITALIZATIONS ACROSS COLORADO [LAST UPDATED: 2022-03-25] 
lh <- co_hosp_summ
lh <- as.data.frame(lh) 
lh[is.na(lh)] <- 0

PF = (100000/gv$TP)

## CLEAN UP DATAFRAME NAMES
colnames(lh)[1]<-"Date"
colnames(lh)[2]<-"count"
## SCALE POPULATION TO 100K
lh$count = lh$count * PF

## FORMAT DATE
#lh$Date <- as.Date(format(as.Date(lh$Date, "%m/%d/%Y"), "%Y-%m-%d"))
## CREATE SORT ID
lh$sort_ID <- as.integer(factor(with(lh, paste(Date))))
## ROLLING 7-DAY AVERAGE
lh <- lh %>%
  arrange(sort_ID) %>%
  mutate(rollapply_sum =zoo::rollapplyr(count, 7, sum, partial = TRUE))
lh$Goal = 10

## REMOVE JUNK
lh <- lh[,c("Date","rollapply_sum", "Goal")]

## SIGNIF 2
lh$rollapply_sum <- signif(lh$rollapply_sum, digits=2)
## ADD ADDITIONAL THRESHOLDS
lh$`Level Yellow: Medium` <- 15
lh$`Level Orange: High` <- 20

## CLEAN UP VAR NAMES
colnames(lh)[2]<-"New COVID-19 Summit County resident admissions per 100,000 population (7-day total)"
colnames(lh)[3]<-"Level Green: Low"

## REMOVE SORT-ID TRASH
lh$sort_ID = NULL
## WRITE.CSV HISTORICAL DATA (ENG)
lh_hist <- lh
lh_hist$`Level Orange: High` = max(lh_hist$`New COVID-19 Summit County resident admissions per 100,000 population (7-day total)`+5)
write.csv(lh_hist, "total_sc_covid_hosp_hist_eng.csv", row.names=F)
## SUBSET 8 WKS (ENG)
lh_8wk <- tail(lh, 28)
## WRITE CSV (CSV)
write.csv(lh_8wk, "total_sc_covid_hosp_8wk_eng.csv", row.names=F)
#################################################################################
## SC COVID 200+ INCIDENCE
lh_8wk_200 <- lh_8wk
colnames(lh_8wk_200)[3]<-"min"
lh_8wk_200$min = 0
lh_8wk_200$`Level Yellow: Medium` <- 10
write.csv(lh_8wk_200, "total_sc_covid_hosp_8wk_200_eng.csv",row.names=F)


## SC COVID HOSP (ESP)
lh_esp <- lh_hist
## TRANSLATE COLUMN NAMES (ESP)
colnames(lh_esp)[1]<-"Fecha"
colnames(lh_esp)[2]<-"hospitalizaciones de COVID-19 de residentes del Condado de Summit en Colorado"
colnames(lh_esp)[3]<-"Nivel Verde: Bajo"
colnames(lh_esp)[4]<-"Nivel Amarillo: Medio"
colnames(lh_esp)[5]<-"Nivel Anaranjado: Alto"

write.csv(lh_esp, "total_sc_covid_hosp_hist_esp.csv", row.names=F)

## SUBSET 8 WKS (ESP)
lh_esp_8wk <- tail(lh_esp, 28)
## WRITE CSV (CSV)
write.csv(lh_esp_8wk, "total_sc_covid_hosp_8wk_esp.csv", row.names=F)
## LH - INCIDENCE 200+ (ESP)
lh_esp_200 <- lh_esp_8wk
colnames(lh_esp_200)[3]<-"min"
lh_esp_200$min = 0
lh_esp_200$`Nivel Amarillo: Medio` <- 10
write.csv(lh_esp_200, "total_sc_covid_hosp_8wk_200_esp.csv",row.names=F)
############################################################################################################################################
## SASMC COVID-19 HOSPITALIZATION 
sasmc_cov <- sasmc[,c("Date", "cov_prop_occ")]
## ADD GOAL
sasmc_cov$Goal = "10"
## FORMAT DATE
sasmc_cov$Date <- as.Date(format(as.Date(sasmc_cov$Date, "%m/%d/%Y"), "%Y-%m-%d"))
## ARRANGE DATA 
sasmc_cov$sort_ID <- as.integer(factor(with(sasmc_cov, paste(Date))))
## ROLLING 7-DAY AVERAGE
sasmc_cov <- sasmc_cov %>%
  arrange(sort_ID) %>%
  mutate(rollapply_avg =zoo::rollapplyr(cov_prop_occ, 7, mean, partial = TRUE))
## SIGNIF OF ROLLING AVG
sasmc_cov$rollapply_avg <- signif(sasmc_cov$rollapply_avg, digits=2)

## SELECT VARIABLES OF INTEREST
sasmc_cov_final <- sasmc_cov[,c("Date","rollapply_avg","Goal")]
## ADD ADDITIONAL THRESHOLDS
sasmc_cov_final$`Level Yellow: Medium` <- 15
sasmc_cov_final$`Level Orange: High` <- 20
sasmc_cov_final$max=30
sasmc_cov_final$min=0
## CLEAN UP VARS
colnames(sasmc_cov_final)[2]<-"Percent of staffed inpatient beds occupied by COVID-19 SASMC patients (7-day average)"
colnames(sasmc_cov_final)[3]<-"Level Green: Low"
## WRITE CSV - HISTORICAL (ENG)
write.csv(sasmc_cov_final, "hosp_perc_covid_sasmc_hist_eng.csv", row.names=F)
## SUBSET LAST 8 WEEKS (ENG)
sasmc_cov_8wk <- tail(sasmc_cov_final, 28)
## WRITE CSV - HISTORICAL (ENG)
write.csv(sasmc_cov_8wk, "hosp_perc_covid_sasmc_8wks_eng.csv", row.names=F)
## TRANSLATE TO ESP
sasmc_cov_final_esp <- sasmc_cov_final
colnames(sasmc_cov_final_esp)[1]<-"Fecha"
colnames(sasmc_cov_final_esp)[2]<-"porcentaje del total de camas ocupadas de pacientes de COVID-19 en SASMC"
colnames(sasmc_cov_final_esp)[3]<-"Nivel Verde: Bajo"
colnames(sasmc_cov_final_esp)[4]<-"Nivel Amarillo: Medio"
colnames(sasmc_cov_final_esp)[5]<-"Nivel Anaranjado: Alto"

## 200+ INCIDENCE 
sasmc_cov_8wk_200 <- sasmc_cov_8wk
sasmc_cov_8wk_200$`Level Yellow: Medium` = 10
write.csv(sasmc_cov_8wk_200, "hosp_perc_covid_sasmc_8wks_200_eng.csv", row.names=F)


## WRITE CSV - HISTORICAL (ESP)
write.csv(sasmc_cov_final_esp, "hosp_perc_covid_sasmc_hist_esp.csv", row.names=F)
## SUBSET LAST 8 WEEKS (ESP)
sasmc_cov_8wks_esp <- tail(sasmc_cov_final_esp, 28)
## WRITE CSV - HISTORICAL (ESP)
write.csv(sasmc_cov_8wks_esp, "hosp_perc_covid_sasmc_8wks_esp.csv", row.names=F)
## INCIDENCE 200+
sasmc_cov_8wks_200_esp <- sasmc_cov_8wks_esp
sasmc_cov_8wks_200_esp$`Nivel Amarillo: Medio` = 10
write.csv(sasmc_cov_8wks_200_esp, "hosp_perc_covid_sasmc_8wks_200_esp.csv", row.names=F)

########################################################################################################################
## MILESTONES SUMMARY TABLE
## TABLE INPUT: INCIDENCE
head_incid <- tail(incid_7d, 1)
head_incid$Status = "Low"
head_incid$Status[which(head_incid$pos_case_7d >= 200)] <-  "High"
head_incid$Indicator = "New COVID-19 Cases Per 100,000 people in the past 7 days"
colnames(head_incid)[6]<-"Metric"
incid_tab <- head_incid[,c("Indicator", "Metric","Status")]
incid_tab$Metric <- signif(incid_tab$Metric, digits=3)

## TABLE INPUT: SC Residents
sc_hosp <- tail(lh, 1) 
sc_hosp$Indicator = "New COVID-19 Summit County resident admissions per 100,000 population (7-day total)"
colnames(sc_hosp)[2]<-"Metric"
sc_hosp$Status = "Low"
sc_hosp$Status[which(sc_hosp$Metric >= 10 & sc_hosp$Metric < 20)] <- "Medium"
sc_hosp$Status[which(sc_hosp$Metric >= 20)] <- "High"
sc_hosp_tab <- sc_hosp[,c("Indicator","Metric","Status")]
sc_hosp_tab$Metric <- signif(sc_hosp_tab$Metric, digits=2)

## TABLE INPUT: SASMC COVID %
head_sasmc_cov <- tail(sasmc_cov_8wk, 1)
head_sasmc_cov$Indicator = "Percent of staffed inpatient beds occupied by COVID-19 SASMC patients (7-day average)"
colnames(head_sasmc_cov)[2]<-"Metric"
head_sasmc_cov$Status = "Low"

head_sasmc_cov$Status[which(head_sasmc_cov$Metric >= 10 & head_sasmc_cov$Metric < 20)] <- "Medium"
head_sasmc_cov$Status[which(head_sasmc_cov$Metric >= 20)] <- "High"
head_sasmc_cov_tab <- head_sasmc_cov[,c("Indicator","Metric","Status")]
head_sasmc_cov_tab$Metric <- signif(head_sasmc_cov_tab$Metric, digits=2)
## COMPILED STATUS
comp_tab_status <- head_sasmc_cov_tab
comp_tab_status$Indicator = "COVID-19 community level"
comp_tab_status$Metric = ""
comp_tab_status$Status = "Level Green: Low"
comp_tab_status$Status[which(head_sasmc_cov$Status == "Medium" | sc_hosp_tab$Status == "Medium"| incid_tab$Status == "Medium")] <- "Level Yellow: Medium"
comp_tab_status$Status[which(head_sasmc_cov$Status == "High" | sc_hosp_tab$Status == "High"| incid_tab$Status == "High")] <- "Level Orange: High"


## COMPILE MILESTONE TABLE (ENG)
comp_ms_tab <- rbind(sc_hosp_tab,head_sasmc_cov_tab,incid_tab,comp_tab_status)
## WRITE.CSV (ENG)
write.csv(comp_ms_tab, "comp_ms_tab_eng.csv", row.names=F)
##############################################################################
## 200+ INCIDENCE SUMMARY TABLE
## TABLE INPUT: SASMC COVID %
sasmc_cov_200 <- tail(sasmc_cov_8wk, 1)
sasmc_cov_200$Indicator = "Percent of staffed inpatient beds occupied by COVID-19 SASMC patients (7-day average)"
colnames(sasmc_cov_200)[2]<-"Metric"
sasmc_cov_200$Status = "Medium"

sasmc_cov_200$Status[which(sasmc_cov_200$Metric >= 10)] <- "High"
sasmc_cov_200 <- head_sasmc_cov_200[,c("Indicator","Metric","Status")]
sasmc_cov_200$Metric <- round(sasmc_cov_200$Metric, digits=2)

sc_hosp_tab_200 <- sc_hosp_tab 
sc_hosp_tab_200$Status = "Medium"
sc_hosp_tab_200$Status[which(sc_hosp_tab_200$Metric >= 10)] <- "High"


## COMPILED STATUS 
comp_tab_status_200 <- head_sasmc_cov_tab
comp_tab_status_200$Indicator = "COVID-19 community level"
comp_tab_status_200$Metric = ""
comp_tab_status_200$Status = "Level Yellow: Medium"
comp_tab_status_200$Status[which(comp_tab_status_200$Status == "High" | sc_hosp_tab$Status == "High")] <- "Level Orange: High"

## COMPILE MILESTONE TABLE (ENG)
comp_ms_tab_200 <- rbind(sc_hosp_tab_200,sasmc_cov_200,incid_tab,comp_tab_status_200)
## WRITE.CSV (ENG)
write.csv(comp_ms_tab_200, "comp_ms_tab_200_eng.csv", row.names=F)
## STORE COMMUNITY STATUS TABLE (200+) (ENG)
comm_status_200 <- tail(comp_ms_tab_200, 1)
names(comm_status_200) <- NULL
write.csv(comm_status_200, "comm_status_200_eng.csv", row.names=F)
## TRANSLATE SUMMARY TABLE (200+) (ESP) 
comp_ms_tab_200_esp <- comp_ms_tab_200
## CHANGE COLUMN NAMES
colnames(comp_ms_tab_200_esp)[1]<-"Indicador"
colnames(comp_ms_tab_200_esp)[2]<-"Metrico"
colnames(comp_ms_tab_200_esp)[3]<-"Estado"
## INDICATOR TO ESP
comp_ms_tab_200_esp$Indicador[which(comp_ms_tab_200_esp$Indicador=="New COVID-19 Cases Per 100,000 people in the past 7 days")] <- "Casos Positivos Nuevos por 100,000 Personas en los Ultimos 7 dias"
comp_ms_tab_200_esp$Indicador[which(comp_ms_tab_200_esp$Indicador=="New COVID-19 Summit County resident admissions per 100,000 population (7-day total)")] <- "Hospitalizaciones de COVID-19 de residentes del Condado de Summit en Colorado"
comp_ms_tab_200_esp$Indicador[which(comp_ms_tab_200_esp$Indicador=="Percent of staffed inpatient beds occupied by COVID-19 SASMC patients (7-day average)")] <- "Porcentaje del total de camas ocupadas de pacientes de COVID-19 en SASMC"
comp_ms_tab_200_esp$Indicador[which(comp_ms_tab_200_esp$Indicador=="COVID-19 community level")] <- "Nivel de COVID-19"
comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="Low")]<- "Bajo"
comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="Medium")]<- "Medio"
comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="High")]<- "Alto"

comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="Level Green: Low")]<- "Nivel Verde: Bajo"
comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="Level Yellow: Medium")]<- "Nivel Amarillo: Medio"
comp_ms_tab_200_esp$Estado[which(comp_ms_tab_200_esp$Estado=="Level Orange: High")]<- "Nivel Anaranjado: Alto"
## WRITE.CSV (ESP)
write.csv(comp_ms_tab_200_esp, "comp_ms_tab_200_esp.csv", row.names=F)
## STORE COMMUNITY STATUS TABLE (ESP)
comm_status_200_esp <- tail(comp_ms_tab_200_esp, 1)
names(comm_status_200_esp) <- NULL
write.csv(comm_status_200_esp, "comm_status_200_esp.csv", row.names=F)
#############################################################################################################
## STORE COMMUNITY STATUS TABLE (ENG)
comm_status <- tail(comp_ms_tab, 1)
names(comm_status) <- NULL
write.csv(comm_status, "comm_status_eng.csv", row.names=F)

## TRANSLATE MILESTONE TABLE TO ESP
comp_ms_tab_esp <- comp_ms_tab
## CHANGE COLUMN NAMES
colnames(comp_ms_tab_esp)[1]<-"Indicador"
colnames(comp_ms_tab_esp)[2]<-"Metrico"
colnames(comp_ms_tab_esp)[3]<-"Estado"
## INDICATOR TO ESP
comp_ms_tab_esp$Indicador[which(comp_ms_tab_esp$Indicador=="New COVID-19 Cases Per 100,000 people in the past 7 days")] <- "Casos Positivos Nuevos por 100,000 Personas en los Ultimos 7 dias"
comp_ms_tab_esp$Indicador[which(comp_ms_tab_esp$Indicador=="New COVID-19 Summit County resident admissions per 100,000 population (7-day total)")] <- "Hospitalizaciones de COVID-19 de residentes del Condado de Summit en Colorado"
comp_ms_tab_esp$Indicador[which(comp_ms_tab_esp$Indicador=="Percent of staffed inpatient beds occupied by COVID-19 SASMC patients (7-day average)")] <- "Porcentaje del total de camas ocupadas de pacientes de COVID-19 en SASMC"
comp_ms_tab_esp$Indicador[which(comp_ms_tab_esp$Indicador=="COVID-19 community level")] <- "Nivel de COVID-19"
comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="Low")]<- "Bajo"
comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="Low")]<- "Medio"
comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="High")]<- "Alto"

comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="Level Green: Low")]<- "Nivel Verde: Bajo"
comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="Level Yellow: Medium")]<- "Nivel Amarillo: Bajo"
comp_ms_tab_esp$Estado[which(comp_ms_tab_esp$Estado=="Level Orange: High")]<- "Nivel Anaranjado: Alto"
## WRITE.CSV (ESP)
write.csv(comp_ms_tab_esp, "comp_ms_tab_esp.csv", row.names=F)
## STORE COMMUNITY STATUS TABLE (ESP)
comm_status_esp <- tail(comp_ms_tab_esp, 1)
names(comm_status_esp) <- NULL
write.csv(comm_status_esp, "comm_status_esp.csv", row.names=F)
################################################################################################
variance$count=1
variance$garbage=0
variance$garbage[which(variance$covid19_test_result=="SARS-COV-2 - SEQUENCING UNSUCCESSFUL")]<-"1"
variance$garbage[which(variance$covid19_test_result=="SPECIMEN UNSATISFACTORY FOR EVALUATION")]<-"1"
variance$garbage[which(variance$covid19_test_result=="SEQUENCE COVERAGE NOT MET")]<-"1"
variance$garbage[which(variance$covid19_test_result=="UNSUCCESSFUL SEQUENCING")]<-"1"
variance$garbage[which(variance$covid19_test_result=="SPECIMEN UNSATISFACTORY FOR EVALUATION")]<-"1"
variance$garbage[which(startsWith(variance$covid19_test_result, "HCOV-19/USA/OH-CDC"))]<-"1"

## CLEAN VARIANT RESULTS
variance$variant_lineage <- variance$covid19_test_result
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - BA.1.1"))]<-"BA.1.1"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "BA.1.1.18"))]<-"BA.1.1"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - BA.2 LINEAGE"))]<-"BA.2"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - BA.3 LINEAGE"))]<-"BA.3"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - BA.4 LINEAGE"))]<-"BA.4"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - BA.5 LINEAGE"))]<-"BA.5"
variance$variant_lineage[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - AY"))]<-"AY"

## CLEAN UP VARIANT TYPE NAME
variance$variant_name <- "Omicron"
variance$variant_name[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - AY"))]<-"Delta"
variance$variant_name[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - B.1.617.2"))]<-"Delta"
variance$variant_name[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - B.1.1.7"))]<-"Alpha"
variance$variant_name[which(startsWith(variance$covid19_test_result, "SARS-COV-2 - Q"))]<-"Alpha"

variance <- subset(variance, garbage=="0")
variance$Date <- variance$dateadded
variance$Date <- as.Date(format(as.Date(variance$Date, "%Y-%m-%d"), "%Y-%m-%d"))

complete_date_var <- tibble(
  Date = seq.Date(min(variance$Date),
                  max(variance$Date),
                  by = "day"))

variance2 <- merge(x=complete_date_var,y=variance,by=c("Date"))
variance3 <- subset(variance2, Date > Sys.Date()-28)

var_summ <- data.frame(group_by(variance3, variant_lineage,variant_name) %>% 
                         summarize(sum = sum(count=="1")))

## CLEAN UP COLUMNS
var_summ <- var_summ[,c("variant_name","variant_lineage","sum")]
## RENAME VARS
names(var_summ)<- c("Name","Lineage","Count")
write.csv(var_summ, "var_sum_eng.csv", row.names=F)
## TRANSLATE TO ESP
var_summ_esp <- var_summ
names(var_summ_esp) <- c("Nombre","Linaje","Cantidad")
write.csv(var_summ, "var_sum_esp.csv", row.names=F)
################################################################################################
## WEEKLY INFOGRAPHIC OUTPUT magikarp
wk_report_names <- c("Total Confirmed Cases","New Cases in Last 7 Days","Change in Weekly Incidence","Total Deaths","Hospitalized Residents per 100,000","Beds Occupied at SASMC","Total Vaccine Doses")
## TOTAL NEW CASES
wk_case_sum = tail(incid_7d, 7)
wk_case_sum = sum(wk_case_sum$Cases)
## MOST RECENT 14-DAY INCIDENCE
wk_14_incid <- tail(incid_7d, 14)
wk_min_date = min(wk_14_incid$Date)
wk_max_date = max(wk_14_incid$Date)
## RECENT 7-DAY PERIOD
wk_curr_7d <- tail(wk_14_incid, 7)
wk_prev_7d <- head(wk_14_incid, 7)
wk_prev_7d_case_sum = sum(wk_prev_7d$Cases)
wk_curr_7d_case_sum = sum(wk_curr_7d$Cases)
wk_prop_case_change = round((wk_curr_7d_case_sum-wk_prev_7d_case_sum)/wk_prev_7d_case_sum*100, digits=2)
## HOSP RESIDENT SUM 7-DAY
hosp_7d_sum = subset(hosp_comp, Date >= wk_min_date & Date <= wk_max_date)
hosp_7d_sum_tail7 = tail(hosp_7d_sum)
hosp_7d_sum_tail7=sum(hosp_7d_sum_tail7$hosp_count)

## MOST RECENT 7-DAY INCIDENCE
wk_report_values <- c(summary_eng[3,2], wk_case_sum, wk_prop_case_change, summary_eng[7,2], summary_eng[4,2], hosp_7d_sum_tail7,dim(vax)[1])
wk_df <- data.frame(wk_report_names,wk_report_values)
## WRITE.CSV - WEEKLY REPORT TABLE VALUES
write.csv(wk_df, "wkr_table.csv",row.names=F)
################################################################################################
library(SaviR)
df_who <- get_combined_table("WHO")

US_covid = data.frame(subset(df_who, country=="United States of America"))
US_covid2 = subset(US_covid, date >= wk_min_date & date <= wk_max_date)
US_covid3 <- US_covid2[,c("date", "week_case_incidence")]
colnames(US_covid3)[2]<-"7-day Incidence per 100,000"
US_covid3$group="USA"
cdphe_incid <- read.csv("CDPHE_COVID19_State-Level_Expanded_Case_Data.csv", header=T)

US_covid_pos = data.frame(subset(df_who, country=="United States of America"))


cdphe_incid$date <- as.Date(format(as.Date(cdphe_incid$date, "%m/%d/%Y"), "%Y-%m-%d"))

cdphe_incid2 <- subset(cdphe_incid, description=="7-Day Average of COVID-19 Cases in Colorado by Date Reported to the State")
cdphe_incid3 <- subset(cdphe_incid2, date >= wk_min_date & date <= wk_max_date)
cdphe_incid4 <- cdphe_incid3[,c("date","value")]
colnames(cdphe_incid4)[2]<-"7-day Incidence per 100,000"
cdphe_incid4$group = "CO"
co_pf = 100000/5810000
cdphe_incid4$`7-day Incidence per 100,000` = cdphe_incid4$`7-day Incidence per 100,000`*co_pf





sc_incid14 <- wk_14_incid[,c("Date","pos_case_7d")]
colnames(sc_incid14)[1]<-"date"
colnames(sc_incid14)[2]<-"7-day Incidence per 100,000"
sc_incid14$group= "Summit County"
## COMPILE INCIDENCE 14-DAY PERIOD
comp_14inc <- rbind(sc_incid14,cdphe_incid4,US_covid3)

require(ggplot2)
incid14d_plot <- ggplot(comp_14inc, aes(x=date, y=`7-day Incidence per 100,000`, group=group)) +
  geom_line(aes(color=group),size=2)+
  geom_point(aes(color=group),size=3)+
  scale_color_manual(values=c("#7698ac", "#002856", "#035d67"))+
  scale_x_date(date_breaks = "day" , date_labels = "%b-%d")+
  theme_classic()+ theme(legend.position="bottom",
                         legend.title = element_blank(),
                         axis.title.x = element_blank(),
                         axis.text.x = element_text(angle = 90))

ggsave(filename = "incidence_plot.jpg",path = "G:/My Drive/Summit County - COVID-19/Database/Datawrapper", width = 3, height = 3, units="in", device='jpg', dpi=300)

pos14_sub <- subset(pcr_pos_8wk, Date >= wk_min_date & Date <= wk_max_date)

## POSITIVITY 14-DAY PERIOD PLOT fuck
positivity14d_plot <- ggplot(pos14_sub, aes(x=Date, y=`Rolling 7-Day Percent of Tests Positive`)) +
  geom_line(size=2, color="#002856")+
  geom_point(size=3,color="#002856")+
  ylim(0,max(pos14_sub$`Rolling 7-Day Percent of Tests Positive`+5))+
  ylab("7-day Positivity (%)")+
  scale_x_date(date_breaks = "day" , date_labels = "%b-%d")+
  theme_classic()+ theme(legend.position="bottom",
                         legend.title = element_blank(),
                         axis.title.x = element_blank(),
                         axis.text.x = element_text(angle = 90))

ggsave(filename = "positivity_plot.jpg",path = "G:/My Drive/Summit County - COVID-19/Database/Datawrapper", width = 3, height = 3, units="in", device='jpg', dpi=300)
################################################################################################
demo_df <- subset(ceders, reporteddate >= wk_min_date & reporteddate <= wk_max_date)
## ASSIGN WEEK PERIOD
demo_df$wk_period = "Previous Week" 
demo_df$wk_period[which(demo_df$reporteddate >= wk_min_date + 7)] <- "Current Week"

wk_age <- data.frame(group_by(demo_df, age_group, wk_period) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))

wk_age2 <- subset (wk_age, wk_period=="1")
colnames(wk_age2)[3]<-"current"
wk_age_prev <- subset (wk_age, wk_period=="0")
wk_age2$prev = wk_age_prev$sum_cases
wk_age2$prop = wk_age2$prev-wk_age2$current/wk_age2$prev*100

wk_age$wk_period<-factor(wk_age$wk_period, levels=c('Previous Week','Current Week'), ordered = TRUE)

wk_age$sum_cases <- as.integer(wk_age$sum_cases)

age_plot <-ggplot(wk_age, aes(x=age_group, y=sum_cases, fill=wk_period)) +
  geom_bar(stat="identity",position=position_dodge(),colour="black")+
  scale_fill_manual(values=c("#a6bbc9", "#002856"))+
  ylab("Case Count")+
  ylim(0,max(wk_age$sum_cases))+
  scale_y_continuous(breaks=seq(round(max(wk_age$sum_cases),0)))+
  theme_classic()+ theme(legend.position="bottom",
                       legend.title = element_blank(),
                       axis.title.x = element_blank())

ggsave(filename = "age_bar_plot.jpg",path = "G:/My Drive/Summit County - COVID-19/Database/Datawrapper", width = 3.5, height = 2, units="in", device='jpg', dpi=300)
################################################################################################
## RACE/ETHNICITY WEEK PLOT
wk_race <- data.frame(group_by(demo_df, race_ethnic, wk_period) %>% 
                       summarize(sum_cases = sum(index_measure=="1")))
wk_race <- subset(wk_race, race_ethnic=="Hispanic/Latinx" | race_ethnic=="White")
wk_race2 <- subset (wk_race, wk_period=="Current Week")
wk_race2_prev <- subset (wk_race, wk_period=="Previous Week")
wk_race2$prev = wk_race2_prev$sum_cases
colnames(wk_race2)[3]<-"current"
wk_race2$prop = wk_race2$prev-wk_race2$current/wk_race2$prev*100

wk_age$wk_period<-factor(wk_age$wk_period, levels=c('Previous Week','Current Week'), ordered = TRUE)

wk_age$sum_cases <- as.integer(wk_age$sum_cases)

race_plot <-ggplot(wk_race, aes(x=race_ethnic, y=sum_cases, fill=wk_period)) +
  geom_bar(stat="identity",position=position_dodge(),colour="black")+
  scale_fill_manual(values=c("#a6bbc9", "#002856"))+
  ylab("Case Count")+
  #ylim(0,max(wk_age$sum_cases))+
  #scale_y_continuous(breaks=seq(round(max(wk_race$sum_cases),0)))+
  theme_classic()+ theme(legend.position="bottom",
                         legend.title = element_blank(),
                         axis.title.x = element_blank())

ggsave(filename = "race_plot.jpg",path = "G:/My Drive/Summit County - COVID-19/Database/Datawrapper", width = 3.5, height = 2, units="in", device='jpg', dpi=300)
################################################################################################
## SUMMARIZE VACCINE DATA LAST 7-DAYS
## FORMAT VACCINE DATE
vax$date <- as.Date(format(as.Date(vax$vaccination_date, "%m/%d/%Y"), "%Y-%m-%d"))
## SUBSET OUT VACCINE DATA
vax_7d <- subset(vax, date >= wk_min_date & date <= wk_max_date)
## SUMMARIZE VACCINE DOSES BY AGE GROUP
vax_7d$count=1
wk_vax <- data.frame(group_by(vax_7d, age_class, date) %>% 
                        summarize(sum_cases = sum(count=="1")))
#fuck
vax_plot <-ggplot(wk_vax, aes(x=date, y=sum_cases, fill=age_class)) +
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#7f7e73","#035d67","#d25f15","#002856","#f6a800","#4e7f71","#a9ad00","#205641","#7698ac"))+
  ylab("Case Count")+
  scale_x_date(date_breaks = "day" , date_labels = "%b-%d")+
  theme_classic()+ theme(legend.position="bottom",
                         legend.title = element_blank(),
                         axis.title.x = element_blank(),
                         axis.text.x = element_text(angle = 90))

ggsave(filename = "vax_age_plot.jpg",path = "G:/My Drive/Summit County - COVID-19/Database/Datawrapper", width = 4, height = 4, units="in", device='jpg', dpi=300)



################################################################################################
## REMOVE DOWNLOADED .TXT FILES FROM DIRECTORY
setwd("C:/Users/hayde/Downloads")

file.remove("cedrs_Summit.txt")
file.remove("elr_rollup_Summit.txt")
file.remove("elr_tests_Summit.txt")
file.remove("PatientImmunizations_Summit.txt")
file.remove("elr_variant_tests_Summit.txt")
file.remove("cophs_Summit.txt")


