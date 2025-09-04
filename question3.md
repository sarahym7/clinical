question_3
================
Sarahy Martinez

``` r
# read in the dataset 

ccu_data =
   read_csv(
  "C:/Users/sarah/OneDrive/Documents/Data Science/clinical/202210 - EDW - CCU_assessment.csv",
  na = c("NA", ".", "")
) %>% 
  janitor::clean_names() %>% 
  select(mrn_hidden, enc_no_hidden,adt_evnt_nm,from_dept, adt_dttm_hidden,to_dept)
```

    ## New names:
    ## Rows: 3524 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): ADT_EVNT_NM, FROM_DEPT, ADT_DTTM_hidden, TO_DEPT dbl (2): MRN_hidden,
    ## ENC_NO_hidden lgl (1): ...7
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...7`

Recoding data from the notes regarding:

``` r
updated_data = ccu_data %>% 
    mutate(adt_dttm = mdy_hm(adt_dttm_hidden)) %>% #mutate adt so that date time are more legible
    group_by(mrn_hidden, enc_no_hidden) %>%  #grouping by id since its the common identifier and instructions say to group by both variables
   arrange(adt_dttm) %>%  # arranging the admitted dates and times for order 
  summarise(
    first_icu_admission = min(adt_dttm[grepl("ICU", to_dept)], na.rm = TRUE), #min for the first encounter in the icu admin date
    first_icu_discharge = min(adt_dttm[grepl("ICU", from_dept)], na.rm = TRUE), # min for the first icu discharge 
    .groups = "drop" 
  ) %>%
  mutate(
    first_icu_discharge = if_else(is.infinite(first_icu_discharge),
                                  ymd_hms("2022-10-03 00:00:00"), # for anyone admitted ansd is still there will use this discharge date
                                  first_icu_discharge),
    first_icu_los = as.numeric(difftime(first_icu_discharge,   # use sql diff time to calculate the length of stay between admin and discharge and want in hrs
                                        first_icu_admission,
                                        units = "hours")))

# under first_icu_admission some values will appear inf that s
```
