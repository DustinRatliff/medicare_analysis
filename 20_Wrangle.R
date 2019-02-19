medicare_physician_compare_data <- medicare_physician_compare_data %>%
        mutate(full_address = 
                       paste(`Line 1 Street Address`,
                             City,
                             State,
                             substr(`Zip Code`,
                                       1,5),
                                       
                             sep = ", "))

medicare_ohio <- medicare_physician_compare_data %>%
        filter(State == "OH")

warren_cities <- c("FRANKLIN",
                   "HARVEYSBURG",
                   "KINGS MILLS",
                   "LEBANON",
                   "MAINEVILLE",
                   "MASON",
                   "MORROW",
                   "OREGONIA",
                   "PLEASANT PLAIN",
                   "SOUTH LEBANON",
                   "SPRINGBORO",
                   "WAYNESVILLE")

medicare_warren <- medicare_ohio %>% filter(City %in% warren_cities)

# medicare_warren_geocode <- mutate_geocode(medicare_warren, full_address)
# write_csv(medicare_warren_geocode, "data/medicare_warren_geocode.csv")
# medicare_warren_geocode <- read_csv("data/medicare_warren_geocode.csv")



medicare_warren_geocode <- read_csv("data/medicare_warren_geocode.csv") %>% 
        distinct(NPI, .keep_all = TRUE)


market_saturation_state <- market_saturation %>%
        filter(reference_period == "2015-01-01 to 2015-12-31" |
                       reference_period == "2016-01-01 to 2016-12-31" |
                       reference_period == "2017-01-01 to 2017-12-31") %>%
        mutate(year = substr(reference_period, 0, 4)) %>%
        filter(state == "OH") %>%
              filter(type_of_service == "Preventive Health Services") %>%
        select(year, 
               number_of_fee_for_service_beneficiaries,
               number_of_providers,
               number_of_users) %>%
        group_by(year) %>%
        summarise_all(sum) %>%
        ungroup %>%
        mutate(county = "State of Ohio")

market_saturation_county <- market_saturation %>%
        filter(reference_period == "2015-01-01 to 2015-12-31" |
                       reference_period == "2016-01-01 to 2016-12-31" |
                       reference_period == "2017-01-01 to 2017-12-31") %>%
        mutate(year = substr(reference_period, 0, 4)) %>%
        filter(state == "OH") %>%
        filter(county == "Warren" |
                       county == "Clermont" |
                       county == "Delaware" |
                       county == "Medina") %>%
        filter(type_of_service == "Preventive Health Services") %>%
        select(year, 
               county, 
               number_of_fee_for_service_beneficiaries,
               number_of_providers,
               number_of_users) %>%
       bind_rows(market_saturation_state) %>%
        rename(Providers  = number_of_providers) %>%
        rename(Beneficiaries = number_of_fee_for_service_beneficiaries) %>%
        rename(Users = number_of_users) %>%
        rename(County = county) %>%
        rename(Year = year) %>%
        mutate(`Beneficiaries per Provider` = 
                       round(Beneficiaries/Providers,
                             digits = 2)) %>%
        mutate(`Users per Provider` = 
                       round(Users/Providers,
                             digits  = 2)) %>%
        mutate(`Users per Beneficiaries` = 
                       round(Users/Beneficiaries,
                             digits = 2)) %>%
        arrange(County, Year) %>%
        tableGrob(rows = NULL)
        
ctsa_survey_table_first_half <- ctsa_survey %>%
        filter(response_category <6 ) %>%
        select(question_number, category) %>%
        group_by(question_number, category) %>%
        count %>%
        spread(key = category, value = n) %>%
        rename("Question Number" = question_number) %>%
        adorn_totals %>%
        tableGrob(rows = NULL)

ctsa_survey_table_second_half <- ctsa_survey %>%
        filter(response_category > 5 ) %>%
        select(question_number, category) %>%
        group_by(question_number, category) %>%
        count %>%
        spread(key = category, value = n) %>%
        rename("Question Number" = question_number) %>%
        adorn_totals %>%
        tableGrob(rows = NULL)
        

ctsa_survey_table <- grid.arrange(ctsa_survey_table_first_half,
             ctsa_survey_table_second_half)
