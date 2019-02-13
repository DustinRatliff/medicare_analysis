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
        