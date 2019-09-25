##########################################################################################################
###################### Siyenza R Script for Dashboard Population ########################################
###################### Developed by: Gina Safarty, Yaa Obeng ###########################################
                      print(Sys.Date()) # Last Updated

# Load Packages Needed
library(tidyverse)
library(readxl)
library(lubridate)
library(splitstackshape)

`%ni%` <- Negate(`%in%`)


#Reference Dates
tx_curr_startOfSiyenza  <-  as.POSIXct("2019-03-01")
startOfSiyenza          <-  as.POSIXct("2019-03-15")
endOfSiyenza            <-  as.POSIXct("2019-10-04") # Change the date here to 2020 date
currentWeekStart        <-  as.POSIXct("2019-09-07") # Change date here WEEKLY
currentWeekEnd          <-  as.POSIXct("2019-09-13") # Change date here WEEKLY


##### Import CDC datasets #####
cdc_result <- read_excel("RAW/CDC_Siyenza_20190917.xlsx", sheet = "Siyenza")%>%
   filter(Week_End >= date(tx_curr_startOfSiyenza))

##### Import USAID datasets and filter out dummy rows for Western Cape Province Sites #####
usaid_result <- read_excel("RAW/USAID_Siyenza_20190917.xlsx", sheet = "USAID RAW DATA") %>% 
  filter(Week_End >= date(tx_curr_startOfSiyenza)) %>% 
  filter(!Siyenza_StartDate==date("2019-08-01") | !Week_End<date("2019-08-02"))

##### Merge interagency datasets #####
df_merged <- bind_rows(cdc_result, usaid_result) %>% 
  rename(TPT_Initiated = "TPT initiated") %>% 
  select(-CURR_RTC) %>%
  filter(!Week_End<date("2019-06-07") | !PrimePartner=="Anova Health Institute" | !SNU1=="wc Western Cape Province") %>% 
  filter(!Week_End>date("2019-05-31") | !PrimePartner=="Kheth'Impilo" | !SNU1=="wc Western Cape Province") %>% 
  arrange(Facility, Week_End)



##### Remove values for rows with data before agreed upon reporting weeks #######
interim<-df_merged %>%
                  gather(indicator, value, HTS_TST_POS:TARG_WKLY_NETNEW)%>%
  mutate(value = case_when(
    Week_End == date(startOfSiyenza) & indicator %in% c("TX_CURR_28")  ~ 0,
    Week_End <= date(startOfSiyenza) & indicator %ni% c("TX_CURR_28")  ~ 0, 
    indicator %in% c("TX_CURR_28")& Week_End <= date("2019-08-01") & 
      date(Siyenza_StartDate) == date("2019-08-01") ~ 0,
    indicator %ni% c("TX_CURR_28")& Week_End <= date("2019-08-10") & 
      date(Siyenza_StartDate) == date("2019-08-01") ~ 0,
    TRUE ~ value)) %>% 
  filter(indicator %in% c("TX_CURR_28","EARLYMISSED", "LATEMISSED", "uLTFU", "TX_NEW")) %>% 
  arrange(Facility, Week_End)

###### Drop irrelevant columns to account for transitional complexities.############# 
###### 'calc_indicators' is used for all subsequent data transformations #############
calc_indicators<-interim %>% 
                group_by(Facility) %>%
              select(-c(1:6,10:12))

#### Filter merged dataset for indicators that are NOT modified ####
original_indicators<-df_merged %>%
  gather(indicator, value, HTS_TST_POS:TARG_WKLY_NETNEW)%>%
  filter(indicator %ni% c("TX_CURR_28","EARLYMISSED", "LATEMISSED", "uLTFU", "TX_NEW")) %>% 
  spread(indicator, value) %>% 
  arrange(Facility, Week_End)


######## Create Biweekly Net NEW and Weekly Average #############
df_tx_net_new<-calc_indicators %>% 
  filter(indicator=="TX_CURR_28")%>%
  filter(Week_End != date("2019-04-05") 
         & Week_End != date("2019-05-03")) %>% 
  group_by(Facility, indicator) %>%
  arrange(Facility,Week_End)%>%
  filter(value != 0)%>%
  mutate(diff_value = case_when(Week_End == date("2019-08-02") & Siyenza_StartDate == date("2019-08-01") ~ 0,
                                                                 TRUE ~ value - lag(value)))%>%
  mutate(net_new_wkly_avg= case_when(Week_End == date("2019-03-29")~ as.numeric(diff_value/4),
                                TRUE~ as.numeric(diff_value/2)))%>% 
  ungroup() %>% 
  rename(bi_weekly_net_new=diff_value,
         TX_CURR_28=value) %>%
  select(-c("indicator", "TX_CURR_28"))

######## Create Curr Reporting dates and Bi-Weekly Grouping for ease of visualizing ########
care_date<-calc_indicators %>% 
          filter(indicator %in% c("TX_CURR_28", "TX_NEW"))%>%
          spread(indicator, value)%>%
          mutate(TX_CURR_28=case_when(Week_End==date("2019-04-05") | Week_End==date("2019-05-03")~0, TRUE~TX_CURR_28)) %>%
          mutate(curr_reporting_date=case_when(TX_CURR_28>0~ Week_End)) %>%
          arrange(Facility, Week_End)%>%
          mutate(Bi_Week_End=case_when(date(Week_End)<date(curr_reporting_date) ~ curr_reporting_date,
                                       date(Week_End)< date(currentWeekEnd) & is.na(curr_reporting_date) ~ lead(curr_reporting_date),
                                       TRUE~ curr_reporting_date))


###### Create baseline and difference in EARLY missed appts by week; baseline different for old and new sites ########
embase_oldsites<-calc_indicators %>%
  filter(indicator=="EARLYMISSED",
         Siyenza_StartDate==date("2019-03-01"),
         Week_End>=date("2019-03-22")) %>%
  mutate(EARLYMISSED_BASE=case_when(
    Week_End==date("2019-03-22") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(EARLYMISSED_BASE= max(EARLYMISSED_BASE)) %>%
  ungroup() %>%
  mutate(EARLYMISSED_DIFF= (value-EARLYMISSED_BASE)) %>%
  select(-indicator) %>%
  rename(EARLYMISSED=value)

embase_newsites<-calc_indicators %>%
  filter(indicator=="EARLYMISSED",
         Siyenza_StartDate==date("2019-08-01"),
         Week_End>=date("2019-08-16")) %>%
  mutate(EARLYMISSED_BASE=case_when(
    Week_End==date("2019-08-16") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(EARLYMISSED_BASE=max(EARLYMISSED_BASE)) %>%
  ungroup() %>%
  mutate(EARLYMISSED_DIFF= (value-EARLYMISSED_BASE)) %>%
  select(-indicator) %>%
  rename(EARLYMISSED=value)

### bind old and new early missed ###
earlymissed<-bind_rows(embase_newsites,embase_oldsites)


###### create baseline and difference in LATE missed appts by week; baseline different for old and new sites ########
lmbase_oldsites<-calc_indicators %>%
  filter(indicator=="LATEMISSED",
         Siyenza_StartDate==date("2019-03-01"),
         Week_End>=date("2019-03-22")) %>%
  mutate(LATEMISSED_BASE=case_when(
    Week_End==date("2019-03-22") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(LATEMISSED_BASE= max(LATEMISSED_BASE)) %>%
  ungroup() %>%
  mutate(LATEMISSED_DIFF= (value-LATEMISSED_BASE)) %>%
  select(-indicator) %>%
  rename(LATEMISSED=value)

lmbase_newsites<-calc_indicators %>%
  filter(indicator=="LATEMISSED",
         Siyenza_StartDate==date("2019-08-01"),
         Week_End>=date("2019-08-16")) %>%
  mutate(LATEMISSED_BASE=case_when(
    Week_End==date("2019-08-16") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(LATEMISSED_BASE=max(LATEMISSED_BASE)) %>%
  ungroup() %>%
  mutate(LATEMISSED_DIFF= (value-LATEMISSED_BASE)) %>%
  select(-indicator) %>%
  rename(LATEMISSED=value)

### bind old and new late missed ####
latemissed<-bind_rows(lmbase_newsites,lmbase_oldsites) 

##### create baseline and difference in uLTFU appts by week; baseline different for old and new sites ########
ultfubase_oldsites<-calc_indicators %>%
  filter(indicator=="uLTFU",
         Siyenza_StartDate==date("2019-03-01"),
         Week_End>=date("2019-03-22")) %>%
  mutate(uLTFU_BASE=case_when(
    Week_End==date("2019-03-22") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(uLTFU_BASE= max(uLTFU_BASE)) %>%
  ungroup() %>%
  mutate(uLTFU_DIFF= (value-uLTFU_BASE)) %>%
  select(-indicator) %>%
  rename(uLTFU=value)

ultfubase_newsites<-calc_indicators %>%
  filter(indicator=="uLTFU",
         Siyenza_StartDate==date("2019-08-01"),
         Week_End>=date("2019-08-16")) %>%
  mutate(uLTFU_BASE=case_when(
    Week_End==date("2019-08-16") ~ value,
    TRUE ~ 0)
  ) %>%
  group_by(Facility) %>%
  mutate(uLTFU_BASE=max(uLTFU_BASE)) %>%
  ungroup() %>%
  mutate(uLTFU_DIFF= (value-uLTFU_BASE)) %>%
  select(-indicator) %>%
  rename(uLTFU=value)

###### bind old and new uLTFU missed ##########
ultfumissed<-bind_rows(ultfubase_newsites,ultfubase_oldsites) 


############## COMBINE ALL DATASETS ############################ 
df_final<-left_join(original_indicators,care_date)%>%
  left_join(df_tx_net_new)%>%
  left_join(earlymissed)%>%
  left_join(latemissed)%>%
  left_join(ultfumissed)
  
  
##############  WRITE FINAL DATASET TO OUTPUTS FOLDER #################

write.table(df_final, paste0("Outputs/interagencyDash_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)


#Remove the objects in your environment
 rm(list=ls())
