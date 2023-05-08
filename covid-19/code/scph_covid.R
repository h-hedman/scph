#title: SCPH Automated COVID-19 Output Data for Dashboard
#author: Hayden Hedman
#date: "2023-05-07" 
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages  
pacman::p_load(dplyr, data.table, bit64, curl, tidyr, tidyverse, zoo, gsheet,googlesheets4)
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load Google Sheets Data Sources
gv <- data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1DB7xhMwKqzAi9xU_fX9eTDAU50utv0gtjw2dkq2AI5c/edit?usp=sharing'))
# ------------------------------------------------------------------------------------------------------------------------------------
## Set working directory to downloads and load in CDPHE files (internal private files) ------------------------------------------------------------------------------

ceders <- as.data.frame(read.delim("cedrs_Summit.txt", sep="|"))
elr_df <- as.data.frame(read.delim("elr_rollup_Summit.txt", sep="|"))
vax <- as.data.frame(read.delim("PatientImmunizations_Summit.txt", sep ="|", skipNul = T))
co_hosp <- read.delim("cophs_Summit.txt", sep="|")
# Set working directory to output folder ----------------------------------------------------------------------------------------------------------------------------

# Filter out duplicate entries --------------------------------------------------------------------------------------------------------------------------------------
ceders$unique_id <- with(ceders, paste0(ceders, first_name, last_name, date_of_birth,reporteddate))
elr_df<- subset(elr_df, !duplicated(elr_df))
# Transform 'reporteddate' to YYYY-MM-DD date class and structure ---------------------------------------------------------------------------------------------------
ceders$reporteddate <- as.Date(format(as.Date(ceders$reporteddate, "%Y-%m-%d"), "%Y-%m-%d"))
# Create cutoff dates -----------------------------------------------------------------------------------------------------------------------------------------------
curr_ceders = as.Date(max(ceders$reporteddate))
cutoff_index_date = as.Date("2020-03-12")
rec_28d_date = max(ceders$reporteddate) - 28
start_date = as.Date("2020-03-10")
end_date = Sys.Date()-1
complete_date_df <- tibble(
  Date = seq.Date(min(start_date),
                  max(end_date),
                  by = "day"))
# Date updated -------------------------------------------------------------------------------------------------------------------------------------------------------
DATE_UPDATE = Sys.Date() 
# SC resident hospitalization total county ---------------------------------------------------------------------------------------------------------------------------
count_co_hosp <- nrow(unique(co_hosp))
# Total SC resident death count among COVID-19 cases -----------------------------------------------------------------------------------------------------------------
ceders_death <- subset(ceders, outcome=="Patient died")
ceders_death$count=1
DA = length(ceders_death$count)
# Summarize bivalent booster among SC residents ----------------------------------------------------------------------------------------------------------------------
vax$dose_count =1
# Filter vaccine duplicates with created string categorization variable ----------------------------------------------------------------------------------------------
vax$DUP_ROW_FILTER_STRING <- as.integer(factor(with(vax, paste(patient_first_name, patient_last_name, patient_dob,vaccination_date, clinic_id, clinic_desc, 
                                                               dose_count, vaccination_code, clinic_county, gender_code, age_at_1stvaccination, 
                                                               race_ethnicity, patient_street_number, patient_street_prefix,patient_street_name,patient_street_name,
                                                               patient_street_suffix,patient_address,patient_city))))
vax <- subset(vax, !duplicated(DUP_ROW_FILTER_STRING))
# Filter out invalidated vaccine dose --------------------------------------------------------------------------------------------------------------------------------
vax <- subset(vax, Invalidated_Dose = "N")
# Filter vaccine data by SC ------------------------------------------------------------------------------------------------------------------------------------------
vax <- subset(vax, patient_county=="SUMMIT" | patient_county=="Summit")
# Create bivalent booster vaccine categorizations --------------------------------------------------------------------------------------------------------------------
vax$bivalent_booster = 0
vax$bivalent_booster[which(vax$vaccination_code == "COV MOD OMICRON BOOSTER")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR OMICRON 12+")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR OMICRON 12+")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR OMICRON 5-11")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR BIVALENT 6m-4y")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV MOD BIVALENT 6m-5y")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV MOD BIVALENT 6y+")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR BIVALENT 12y+")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COV PFR BIVALENT 5y-11y")] <- 1
vax$bivalent_booster[which(vax$vaccination_code == "COVID MOD Bivalent 18+")] <- 1
# Summarize bivalent count and proportion -----------------------------------------------------------------------------------------------------------------------------
biv_summ <- as.data.frame(group_by(vax,) %>%
                            summarize(sum_bivalent_booster = sum(bivalent_booster=="1"), 
                                      prop_bivalent_booster = round((sum_bivalent_booster/gv$TP)*100,digits=2)))

biv_comp <- with(biv_summ, paste0(biv_summ$sum_bivalent_booster, " ", "(", biv_summ$prop_bivalent_booster, "%", ")"))
# Summarize cumulative CEDRS case data --------------------------------------------------------------------------------------------------------------------------------
total_summ <- data.frame(group_by(ceders) %>% 
                           summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                     prob_sum = sum(casestatus=="probable"),
                                     total_cases = sum(pos_confirm_sum, prob_sum)))
# Create clean table names (ENG/ESP) ----------------------------------------------------------------------------------------------------------------------------------
type_eng = c("Cases", "Total Deaths", "Hospitalizations", "Bivalent Boosters (6 month+)")
type_esp = c("Casos", "Muertes Totales", "Hospitalizaciones", "Refuerzo de bivalente (edad: 6 mes+)")

value = c(total_summ$total_cases, DA, count_co_hosp, biv_comp)
count_table_eng <- data.frame(type_eng, value)
count_table_eng$date_uploaded=DATE_UPDATE

colnames(count_table_eng)[1]<-"Metric"
colnames(count_table_eng)[2]<-"Value"

count_table_esp <- data.frame(type_esp, value)
count_table_esp$date_uploaded=DATE_UPDATE

colnames(count_table_esp)[1]<-"Metrico"
colnames(count_table_esp)[2]<-"Valor"

# Write summary table output files (ENG/ESP) ---------------------------------------------------------------------------------------------------------------------------
write.csv(count_table_eng, row.names = F, "count_table_eng.csv")
write.csv(count_table_esp, row.names = F, "count_table_esp.csv")

# COVID-19 test positivity ---------------------------------------------------------------------------------------------------------------------------------------------
elr_df <- subset(elr_df, test_type=="PCR"); dim(df)
# Summarize sum of  tests by 'dateadded' -------------------------------------------------------------------------------------------------------------------------------
total_tests <- data.frame(group_by(elr_df, dateadded) %>%
                            summarize(total_tested = sum(n_tests))); dim(total_tests)

# Create temporary ID to filter out dates up to '2020-03-13' (first reported COVID-19 case in SC) ----------------------------------------------------------------------
total_tests$sort_ID <- as.integer(factor(with(total_tests, paste(dateadded))))
total_tests <- subset(total_tests, dateadded  >cutoff_index_date)
total_tests <- arrange(total_tests, -sort_ID)
# Pull out positive test data but filter by duplicated 'dateadded'(sum of positive tests from CDPHE is 2x = counted twice lab and non-lab)
pos_tests <- elr_df[!duplicated(elr_df[3]),]
pos_tests <- pos_tests[,c("total_confirmed_cases", "county", "dateadded")]
pos_tests$sort_ID <- as.integer(factor(with(pos_tests, paste(dateadded))))
pos_tests <- arrange(pos_tests, sort_ID)
# Confirm dates 2020-03-12' and prior are removed ----------------------------------------------------------------------------------------------------------------------
pos_tests <- subset(pos_tests, dateadded > cutoff_index_date)
# 
pos_tests <- arrange(pos_tests, -sort_ID)
# Remove 'dateadded' for QC purposes -----------------------------------------------------------------------------------------------------------------------------------
pos_tests$dateadded = NULL
# Add positive tests and total tests -----------------------------------------------------------------------------------------------------------------------------------
elr_df2 <- cbind(total_tests, pos_tests)
# Standardize for potential data input erorrs (e.g. N positive tests < 0) ----------------------------------------------------------------------------------------------
setDT(elr_df2)[total_tested < total_confirmed_cases, total_tested := total_confirmed_cases]
# Difference of Npos tests - Pos tests = sum of negative tests ---------------------------------------------------------------------------------------------------------
elr_df2$total_neg_tests = elr_df2$total_tested-elr_df2$total_confirmed_cases
# Calculate proportion of positive tests -------------------------------------------------------------------------------------------------------------------------------
elr_df2$positive_prop = (elr_df2$total_confirmed_cases/elr_df2$total_tested)*100
## Convert all 'NaN' (e.g., 0/0) to 0 ----------------------------------------------------------------------------------------------------------------------------------
elr_df2$positive_prop[is.na(elr_df2$positive_prop)] <- 0
# Remove placeholder county variable -----------------------------------------------------------------------------------------------------------------------------------
elr_df2$county = NULL

# Calculate 3-day moving mean of positive proportion -------------------------------------------------------------------------------------------------------------------
elr_df2<- subset(elr_df2, sort_ID > 11)
elr_df2$positive_prop <- as.double(elr_df2$positive_prop)
elr_df2$avg_7day <- stats::filter(elr_df2$positive_prop, rep(1/7,7))

# Include shift function -----------------------------------------------------------------------------------------------------------------------------------------------
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
# Shift entries up to align dates --------------------------------------------------------------------------------------------------------------------------------------
elr_df2$avg_7day <- shift(elr_df2$avg_7day, 3)
# Replace all NA's with 0 ----------------------------------------------------------------------------------------------------------------------------------------------
elr_df2$avg_7day[is.na(elr_df2$avg_7day)] <- 0

# Prepare previous 28-day data -----------------------------------------------------------------------------------------------------------------------------------------
pos28day <- head(elr_df2, 28)
pos28day_avg = as.numeric(format(mean(pos28day$avg_7day),digits=2,nsmall=1))
pos7day <- head(elr_df2, 7)
pos7day_avg = as.numeric(format(mean(pos7day$avg_7day),digits=2,nsmall=1))
# Summarize 28-day and 7-day periods -----------------------------------------------------------------------------------------------------------------------------------
ceders_daily <- data.frame(group_by(ceders, reporteddate) %>% 
                           summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                     prob_sum = sum(casestatus=="probable"),
                                     total_cases = sum(pos_confirm_sum, prob_sum)))

# Format date ----------------------------------------------------------------------------------------------------------------------------------------------------------
ceders_daily$Date <- as.Date(format(as.Date(ceders_daily$reporteddate, "%Y-%m-%d"), "%Y-%m-%d")) 

# Compile time series data ---------------------------------------------------------------------------------------------------------------------------------------------
ceders_daily <-  ceders_daily %>%
  complete(Date = seq(cutoff_index_date, end_date, by ="day")) 
  ceders_daily <- subset(ceders_daily, !is.na(ceders_daily$Date))
  ceders_daily <- data.frame(ceders_daily) 
  ceders_daily$reporteddate = NULL
  ceders_daily[is.na(ceders_daily)] <-0
  
# 28 days data --------------------------------------------------------------------------------------------------------------------------------------------------------- 
cases_28_day <- tail(ceders_daily, 28) 
cases_28_day_avg = as.numeric(format(mean(cases_28_day$total_cases),digits=2,nsmall=1))

# 7 days data ---------------------------------------------------------------------------------------------------------------------------------------------------------- 
cases_7_day <- tail(ceders_daily, 7) 
cases_7_day_avg = as.numeric(format(mean(cases_7_day$total_cases),digits=2,nsmall=1))

# Summarize 7- and 28-day period hospitalizations ----------------------------------------------------------------------------------------------------------------------
colnames(co_hosp)[5]<-"admission_date"
co_hosp$Date <- format(as.Date(co_hosp$admission_date, "%Y-%m-%d"), "%Y-%m-%d")
co_hosp$freq=1
co_hosp_summ <- data.frame(group_by(co_hosp, Date) %>% 
                             summarize(hosp_count = sum(freq=="1")))
co_hosp_summ$Date <- as.Date(format(as.Date(co_hosp_summ$Date, "%Y-%m-%d"), "%Y-%m-%d"))

# Complete time series -------------------------------------------------------------------------------------------------------------------------------------------------
co_hosp_summ <-  co_hosp_summ %>%
  complete(Date = seq(cutoff_index_date, end_date, by ="day")) 
hosp_comp <- subset(co_hosp_summ, !is.na(co_hosp_summ$Date))
hosp_comp[is.na(hosp_comp)] <-0
hosp_comp <- data.frame(hosp_comp) 
hosp_comp <- arrange(hosp_comp, Date)
# 28-day period hospitalziations --------------------------------------------------------------------------------------------------------------------------------------- 
hosp_28_day <- tail(hosp_comp, 28)
hosp_28_day_avg = as.numeric(format(mean(hosp_28_day$hosp_count),digits=2,nsmall=1))
# 7-day period hospitalizations ----------------------------------------------------------------------------------------------------------------------------------------
hosp_7_day <- tail(hosp_comp, 7)
hosp_7_day_avg = as.numeric(format(mean(hosp_7_day$hosp_count),digits=2,nsmall=1))
# Compile 7- vs 28-day periods
type_eng = c("Percent Positivity", "Total Cases", "Hospitalizations")
type_esp = c("Casos", "Muertes Totales", "Hospitalizaciones")
avg_7d = c(pos7day_avg, cases_7_day_avg, hosp_7_day_avg)
avg_28d = c(pos28day_avg, cases_28_day_avg, hosp_28_day_avg)
# Prepare 28- and 7-day table 'status' and table organization ----------------------------------------------------------------------------------------------------------
summary_eng <- data.frame(type_eng, avg_7d, avg_28d)
summary_eng$avg_7d <- as.numeric(summary_eng$avg_7d)
summary_eng$avg_28d <- as.numeric(summary_eng$avg_28d)
summary_eng$perc_change = format((summary_eng$avg_7d - summary_eng$avg_28d)/summary_eng$avg_28*100,digits=2,nsmall=1)
summary_eng$change = "+"
summary_eng$change[which(summary_eng$avg_7d < summary_eng$avg_28d)] <- "-"
summary_eng$change[which(summary_eng$avg_7d == summary_eng$avg_28d)] <- ""
summary_eng$Status = "No Change"
summary_eng$Status[which(summary_eng$avg_7d < summary_eng$avg_28d)] <- "Decreasing"
summary_eng$Status[which(summary_eng$avg_7d > summary_eng$avg_28d)] <- "Increasing"
summary_eng$perc_change[which(summary_eng$avg_7 == summary_eng$avg_28d)] <- "No Change"
summary_eng$perc_change[which(summary_eng$Status == "No Change")] <- "0"
summary_eng$perc_change_final = with(summary_eng, paste0(change, perc_change))
summary_eng$perc_change_final = gsub(" ","",summary_eng$perc_change_final)
# Pull out variables of interest ----------------------------------------------------------------------------------------------------------------------------------------
summary_eng<- summary_eng[,c("type_eng","avg_7d","avg_28d","perc_change_final", "Status")]
# Clean up column names -------------------------------------------------------------------------------------------------------------------------------------------------
summary_table_header <- c("Metric","Last 7 Days Daily Average", "Last 28 Days Daily Average", "Percent Change", "Status")
names(summary_eng)<- summary_table_header
# Add time stamp for github viewers -------------------------------------------------------------------------------------------------------------------------------------
summary_eng$date_uploaded=DATE_UPDATE
# Write data to csv (ENG) -----------------------------------------------------------------------------------------------------------------------------------------------
write.csv(summary_eng, row.names = F, "summary_28d_and_7d_table_eng.csv")
# Write data to csv (ESP) -----------------------------------------------------------------------------------------------------------------------------------------------
summary_esp <- summary_eng
summary_esp$Status[which(summary_esp$Status =="Increasing")] <- "Creciente"
summary_esp$Status[which(summary_esp$Status =="Decreasing")] <- "Decreciente"
summary_esp$Status[which(summary_esp$Status =="No Change")] <- "No Cambio"

metrics_esp = c("Porcentaje de Positividad", "Promedio 7 dias anteriores", "Promedio 28 dias anteriores", "Cambio Porcentual", "Tendencia")
names(summary_esp) <- metrics_esp
write.csv(summary_esp, row.names = F, "summary_28d_and_7d_table_esp.csv")

# Prepare 7-day incidence data ------------------------------------------------------------------------------------------------------------------------------------------
colnames(ceders_daily)[1]<-"Date"
colnames(ceders_daily)[4]<-"Cases"

incid_7d <- ceders_daily[,c("Date","Cases")]

# Create proxy temporary ID and sort by  --------------------------------------------------------------------------------------------------------------------------------
incid_7d$sort_ID <- as.integer(factor(with(incid_7d, paste(Date))))
incid_7d <- arrange(incid_7d, -sort_ID); head(incid_7d)
# Assign variable for 7-day intervals -----------------------------------------------------------------------------------------------------------------------------------
incid_7d$ID <-rep(1:100, each=7, length.out=nrow(incid_7d))
# Calculate 7-day mooving sum -------------------------------------------------------------------------------------------------------------------------------------------
incid_7d <- incid_7d %>%
  arrange(sort_ID, Date) %>%
  mutate(rollapply_sum =zoo::rollapplyr(Cases, 7, sum, partial = TRUE)) # requires package zoom
# Calculate incidence scaled to SC census population (gv$TP) ------------------------------------------------------------------------------------------------------------
incid_7d$pos_case_7d <- incid_7d$rollapply_sum*(100000/gv$TP) 
# Subset 8 weeks of data for display purposes ---------------------------------------------------------------------------------------------------------------------------
incid_7d_sub <- arrange(incid_7d, -sort_ID)
incid_7d_sub <- head(incid_7d_sub, 28)
# Include threshold cutoff values ---------------------------------------------------------------------------------------------------------------------------------------
incid_7d_sub$target1 = 200
incid_7d_sub$target2 = 400
# Pull out variables of interest ----------------------------------------------------------------------------------------------------------------------------------------
incid_7d_sub <- incid_7d_sub[,c("Date","pos_case_7d","target1","target2")]
# Prepare incidence data for print --------------------------------------------------------------------------------------------------------------------------------------
colnames(incid_7d_sub)[2] <- "Rolling 7-Day Positive Cases per 100,000"
colnames(incid_7d_sub)[3] <- "Level Green: Low"
colnames(incid_7d_sub)[4] <- "Level Orange: High"
# Write incidence to csv (ENG) ------------------------------------------------------------------------------------------------------------------------------------------
incid_7d_sub$date_uploaded=DATE_UPDATE
write.csv(incid_7d_sub, row.names=FALSE,"pon_incidence_8wk_eng.csv")
# Write incidence to csv (ESP) ------------------------------------------------------------------------------------------------------------------------------------------
incid_7d_sub_esp <- incid_7d_sub
colnames(incid_7d_sub_esp)[1] <- "Fecha"
colnames(incid_7d_sub_esp)[2] <- "positivos nuevos por 100,000 personas en los 7 dias anteriores"
colnames(incid_7d_sub_esp)[3]<-"Nivel Verde: Bajo"
colnames(incid_7d_sub_esp)[4]<-"Nivel Anaranjado: Alto"
write.csv(incid_7d_sub_esp, row.names=FALSE,"pon_incidence_8wk_esp.csv")
# Remove downloaded files from directory --------------------------------------------------------------------------------------------------------------------------------

file.remove("cedrs_Summit.txt")
file.remove("elr_rollup_Summit.txt")
file.remove("PatientImmunizations_Summit.txt")
file.remove("cophs_Summit.txt")
# End of script ---------------------------------------------------------------------------------------------------------------------------------------------------------