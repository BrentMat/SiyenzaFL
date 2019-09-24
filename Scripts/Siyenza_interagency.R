#### Siyenza Interagency Dashboard output###############
#### Author: Abraham Agedew #############################################
#### Date: April, 2019 ##############################################


# .libPaths("C:/R/RLib")

#load installed packages into R#
library(tidyverse)
library(readxl)
library(lubridate)
library(splitstackshape)

`%ni%` <- Negate(`%in%`) 

# rm(list=ls())

# Function to generate the interagency dashboard
GenerateInteragencyOutput <-  function(tx_curr_startOfSiyenza,
                                       startOfSiyenza, 
                                       endOfSiyenza, 
                                       currentWeekStart,
                                       currentWeekEnd,
                                       df)
{
  tx_curr_startOfSiyenza   <-  as.POSIXct(tx_curr_startOfSiyenza)
  startOfSiyenza           <-  as.POSIXct(startOfSiyenza)
  endOfSiyenza             <-  as.POSIXct(endOfSiyenza)
  currentWeekStart         <-  as.POSIXct(currentWeekStart)
  currentWeekEnd           <-  as.POSIXct(currentWeekEnd)
   
  # ##### Uncomment if running the script within the function is desired. 
  # tx_curr_startOfSiyenza  <-  as.POSIXct("2019-03-01")
  # startOfSiyenza          <-  as.POSIXct("2019-03-15")
  # endOfSiyenza            <-  as.POSIXct("2019-10-04")
  # currentWeekStart        <-  as.POSIXct("2019-09-07")
  # currentWeekEnd          <-  as.POSIXct("2019-09-13")
  # 
  # 
  ##### Calcuate weeks remaining as well as the total number of weeks
  weeks_remaining <- isoweek(endOfSiyenza) - isoweek(currentWeekEnd)
  number_of_weeks <- isoweek(currentWeekEnd) - isoweek(tx_curr_startOfSiyenza)
  
  ##### TX_CURR Dates as agreed upon by interagency stakeholders. ####
  ### TX_CURR values are collected for only the days listed below ###
  tx_curr_dates <-  c("2019-03-01","2019-03-29","2019-04-12", "2019-05-10", 
                      "2019-05-24","2019-06-07", "2019-06-21", "2019-07-05", "2019-07-19", "2019-08-02", "2019-08-16", "2019-08-30", 
                      "2019-09-13", "2019-09-27")
  
  ##### Import CDC datasets #####
  cdc_result <- read_excel("RAW/CDC_Siyenza_20190917.xlsx", sheet = "Siyenza") %>%
    # filter(Week_End >= startOfSiyenza & Week_End <= date(currentWeekEnd) +1)
    filter(Week_End >= date(tx_curr_startOfSiyenza) & Week_End <= date(currentWeekEnd))
  ##### Import USAID datasets #####
  usaid_result <- read_excel("RAW/USAID_Siyenza_20190917.xlsx", sheet = "USAID RAW DATA") %>% 
    filter(Week_End >= date(tx_curr_startOfSiyenza) & Week_End <= date(currentWeekEnd))
  ##### Merge interagency datasets #####
  df_merged <- bind_rows(cdc_result, usaid_result) %>% 
    rename(TPT_Initiated = "TPT initiated") %>% 
    select(-CURR_RTC)
  
   
  ## Remove records/rows that are not within the start/end dates
  df_all <-  df_merged %>%
    gather(indicator, value,HTS_TST_POS:TARG_WKLY_NETNEW) %>%
    # filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(value = case_when(
      # Week_End < date(currentWeekEnd) - 7 & indicator %in% c("TX_CURR_28")  ~ 0,
      Week_End == date(startOfSiyenza) & indicator %in% c("TX_CURR_28")  ~ 0,
      Week_End <= date(startOfSiyenza) & indicator %ni% c("TX_CURR_28")  ~ 0, 
      indicator %in% c("TX_CURR_28")& Week_End <= date("2019-08-01") & date(Siyenza_StartDate) == date("2019-08-01") ~ 0,
      indicator %ni% c("TX_CURR_28")& Week_End <= date("2019-08-10") & date(Siyenza_StartDate) == date("2019-08-01") ~ 0,
      TRUE ~ value)) %>% 
    spread(indicator, value)
  
  ##### Replicate rows to include future weeks
  df_replicate <- df_all %>%
    filter(Week_End == date(currentWeekEnd)) %>%
    gather(indicator, value, cLTFU:uLTFU) %>%
    mutate(value = 0) %>%
    spread(indicator, value)
  
  all <-  df_all

  ##### Loop through the weeks remaining for replication
  for(i in 1:weeks_remaining)
  {
    df_all_replicate <- df_replicate %>%
      mutate(Week_Start = as.POSIXct(date(currentWeekStart) + (7*i)),
             Week_End   = as.POSIXct(date(currentWeekEnd)+ (7*i)))
    all <- bind_rows(all, df_all_replicate)
    # print(i)

  }
  
  ##### Calculate cumulative records
  df_cum <-  df_all %>%
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("HTS_TST_POS","TX_NEW", "TX_NEW_SAMEDAY", "TPT_Initiated")) %>% 
    group_by(Facility, indicator) %>% 
    arrange(Week_End) %>% 
    mutate(cum_value = cumsum(value)) %>% 
    ungroup() %>% 
    mutate(indicator = paste0(indicator, "_CUM"))%>% 
    filter(Week_End == date(currentWeekEnd)) %>%
    select(-value) %>% 
    spread(indicator, cum_value)
  
  ##### Merge cumulative rows with the original datasets
  
  df_final <-  bind_rows(all, df_cum) %>% 
    gather(indicator, value, cLTFU:TX_NEW_SAMEDAY_CUM, na.rm = TRUE) %>% 
    spread(indicator, value) %>% 
    replace(is.na(.), "") 
  
  ##### Create TX_CURR baseline indicator to calculate CURR TODATE
  df_tx_curr_baseline <-  df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(indicator = case_when(Week_End == date(tx_curr_startOfSiyenza) & Siyenza_StartDate == date(tx_curr_startOfSiyenza) ~ "TX_CURR_28_BASE", 
                                 Week_End == date("2019-08-02") & Siyenza_StartDate == date("2019-08-01") ~ "TX_CURR_28_BASE",
                                 TRUE ~ "")) %>% 
    filter(indicator %in% c("TX_CURR_28_BASE"))
  
  ##### Calculate TX_CURR ToDate
  df_tx_curr_todate <- df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(indicator = "TX_CURR_28_TODATE") 
  
  #### Calculate max of TX_CURR Week_End
  max_tx_curr_date <-  max(df_tx_curr_todate$Week_End)

  #### Find MAX TX_CURR
  df_max_txcurr_todate <-   df_tx_curr_todate %>% 
    filter(Week_End == max_tx_curr_date)
  
  #### Merge TX_CURR TODATE with Baseline
  df_tx_curr_merged <-  bind_rows(df_tx_curr_baseline, df_max_txcurr_todate) 
 
  
  #### Calculate TX_NET_NEW and AVG
  df_net_new <- df_tx_curr_merged %>% 
    select(-Week_Start, -Week_End, -PrimePartner, -MechanismID) %>%
    spread(indicator, value) %>% 
    mutate(TX_NET_NEW_28_TODATE = (`TX_CURR_28_TODATE` - `TX_CURR_28_BASE`),
           Week_Start = date(as.POSIXct(max_tx_curr_date))-6,
           Week_End = date(as.POSIXct(max_tx_curr_date)), 
           # TX_NET_NEW_AVG = case_when(Week_End == date("2019-03-29") ~ TX_NET_NEW_28_TODATE/4,
           #                            TRUE ~ TX_NET_NEW_28_TODATE/2)) %>%
           TX_NET_NEW_AVG= TX_NET_NEW_28_TODATE/number_of_weeks) %>% 
    gather(indicator, value, TX_CURR_28_BASE,TX_CURR_28_TODATE,TX_NET_NEW_28_TODATE, TX_NET_NEW_AVG) %>% 
    filter(indicator %in% c('TX_NET_NEW_28_TODATE', 'TX_NET_NEW_AVG')) %>% 
    mutate(Week_Start =  as.POSIXct(Week_Start),
           Week_End  = as.POSIXct(Week_End))
  
  #### Calculate bi-weekly TX_NET NEW
  df_tx_net_new_cum <- df_tx_curr_todate %>% 
    filter(Week_End != date("2019-03-15")) %>% 
    group_by(Facility, indicator) %>% 
    arrange(Week_End) %>% 
    mutate(diff_value = case_when(Week_End == date("2019-08-02") & Siyenza_StartDate == date("2019-08-01") ~ 0,
                                  TRUE ~ value - lag(value)))%>% 
    ungroup() %>% 
    mutate(indicator = paste0("TX_NET_NEW_BI_WEEKLY"),
           value = diff_value)%>% 
    select(-diff_value)
 
  
  #### Handle timezone changes
  attr(df_net_new$Week_End, "tzone") <- "UTC"
  attr(df_net_new$Week_Start, "tzone") <- "UTC"
  
  
  #### select partner information for merging back to df_net_new, 
  ### necessary if partner change at a site
  df_partner <-  df_tx_curr_merged %>% 
    select(PrimePartner, MechanismID, Facility, Week_End)
  #### Join df_net_new with partner data
  df_net_new_partner <-  df_net_new %>% 
    left_join(df_partner, by = c("Facility", "Week_End")) %>% 
    distinct()
  
 
  
  #### Merge NET NEW df with TX_CURR
  df_curr <-  bind_rows(df_tx_curr_merged,df_net_new_partner, df_tx_net_new_cum)
  
  #### Convert to long
  df_final <-  df_final %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE ) %>% 
    mutate(value = as.numeric(value))
  
  
  #### Bind orginal df with derived curr dataframe
  df <-  bind_rows(df_final, df_curr) %>% 
    spread(indicator, value)%>% 
    replace(is.na(.), "") %>% 
    arrange(Facility,Week_End) %>% 
    mutate(siyenza_site_status = case_when(Siyenza_StartDate == date("2019-08-01") ~ "New",
                                           TRUE ~ "Existing"))
  
  #### Write to txt output
  write.table(df, paste0("Outputs/interagencyDash_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
  
  return(df)
  
}

#### Function for data quality checks
#### DO NOT RUN - 
#### RUN as needed/requested.
GenerateDataQualityReport <- function(df,currentWeekEnd,df_quality)
{
  
  currentWeekEnd           <-  as.POSIXct(currentWeekEnd)
  
  df_quality <-  df %>% 
    mutate(TX_NEW_CURRENT = case_when(Week_End == date(currentWeekEnd) ~ TX_NEW, TRUE ~ ""),
           TX_NET_NEW_Montly_Target = case_when(Week_End == date(currentWeekEnd) ~ (8*as.numeric(TARG_WKLY_NETNEW)), TRUE ~ 0 )) %>% 
    select(FundingAgency, PrimePartner,Facility,
           TX_CURR_28_BASE, TX_CURR_28_TODATE, TX_NET_NEW_28_TODATE,
           TX_NEW_CURRENT,TX_NET_NEW_Montly_Target) %>% 
    gather(indicator, value,TX_CURR_28_BASE:TX_NET_NEW_Montly_Target, na.rm=TRUE) %>% 
    mutate(value = as.numeric(value)) %>% 
    drop_na(value) %>% 
    filter(value != 0) %>% 
    spread(indicator, value) %>% 
    mutate(change_TX_CURR = (TX_NET_NEW_28_TODATE/TX_CURR_28_BASE),
           NEW_vs_NET_NEW = (TX_NEW_CURRENT/TX_NET_NEW_28_TODATE),
           issue = case_when(change_TX_CURR >= 0.1 & NEW_vs_NET_NEW <= 0.25 ~
                               ">10% INCREASE IN TX_CURR, LOW TX_NEW vs NET_NEW %",
                             NEW_vs_NET_NEW <= 0.25 ~ "LOW %  TX_NEW vs NET_NEW",
                             TX_NET_NEW_28_TODATE < 0 ~"NEGATIVE NET NEW",
                             TRUE ~ "")) %>% 
    filter(issue != "")
  
  write.table(df_quality, paste0("Outputs/qualitycheck_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
  
  
  return(df_quality)
  
}

#### Function to merge Frenzy/Blitz dataset to Siyenza
#### DO NOT RUN - 
#### RUN as needed/requested.
MergeFrenzyBlitz_with_Siyenza <- function(tx_curr_startOfSiyenza,
                                          startOfSiyenza, 
                                          endOfSiyenza, 
                                          currentWeekStart,
                                          currentWeekEnd,
                                          df_all_merged)
{
  
  tx_curr_startOfSiyenza   <-  as.POSIXct(tx_curr_startOfSiyenza)
  startOfSiyenza           <-  as.POSIXct(startOfSiyenza)
  endOfSiyenza             <-  as.POSIXct(endOfSiyenza)
  currentWeekStart         <-  as.POSIXct(currentWeekStart)
  currentWeekEnd           <-  as.POSIXct(currentWeekEnd)
  
  
  # tx_curr_startOfSiyenza  <-  as.POSIXct("2019-03-01")
  # startOfSiyenza          <-  as.POSIXct("2019-03-15")
  # endOfSiyenza            <-  as.POSIXct("2019-05-10")
  # currentWeekStart        <-  as.POSIXct("2019-03-30")
  # currentWeekEnd          <-  as.POSIXct("2019-04-05")
  
  
  cdc_result <- read_excel("RAW/CDC_Siyenza_20190423_FFandSIYENZA.xlsx", sheet = "Siyenza") %>%
    # filter(Week_End >= startOfSiyenza & Week_End <= date(currentWeekEnd) +1)
    filter(Week_End <= date(currentWeekEnd))
  
  usaid_result <- read_excel("RAW/USAID_Siyenza_20190424.xlsx", sheet = "USAID RawData") %>% 
    filter(Week_End <= date(currentWeekEnd))
  
  
  
  df_merged <- bind_rows(cdc_result, usaid_result) %>% 
    rename(TPT_Initiated = "TPT initiated") %>% 
    select(-CURR_RTC)
  
  
  df_all_merged <-  df_merged %>% 
    gather(indicator, value,HTS_TST_POS:TARG_WKLY_NETNEW, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_NEW", "HTS_TST_POS", "TX_CURR_28" )) %>% 
    mutate(value = case_when(
      # Week_End < date(currentWeekEnd) - 7 & indicator %in% c("TX_CURR_28")  ~ 0,
      Week_End == date(startOfSiyenza) & indicator %in% c("TX_CURR_28")  ~ 0,
      TRUE ~ value)) %>% 
    spread(indicator, value) %>% 
    replace(is.na(.), "") 
  
  
  write.table(df_all_merged, paste0("Outputs/FF_FB_Siyenza_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
  
  
  return(df_all_merged)
  
  
}

####EXECUTE FUNCTIONS#######


#### TODO: update dates as needed to reflect current week.

df <- GenerateInteragencyOutput(tx_curr_startOfSiyenza  = "2019-03-01",
                                startOfSiyenza = "2019-03-15",
                                endOfSiyenza = "2019-08-31",
                                currentWeekStart = "2019-08-10",
                                currentWeekEnd = "2019-08-16")

# 
# df_quality <-  GenerateDataQualityReport(df, currentWeekEnd = "2019-04-12")
# 
# 
# df_all_merged <-  MergeFrenzyBlitz_with_Siyenza(tx_curr_startOfSiyenza  = "2019-03-01",
#                                                 startOfSiyenza = "2019-03-15",
#                                                 endOfSiyenza = "2019-05-10",
#                                                 currentWeekStart = "2019-04-13",
#                                                 currentWeekEnd = "2019-04-19")
