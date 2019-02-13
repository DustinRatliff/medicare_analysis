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

medicare_warren <- geocode(medicare_warren, output = "latlon")
        