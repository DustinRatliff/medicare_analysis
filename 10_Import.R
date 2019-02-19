medicare_physician_compare_data <- 
        read_csv("data/Physician_Compare_National_Downloadable_File.csv")

market_saturation <- 
        read_csv("data/Market_Saturation_And_Utilization_Dataset_2019-01-25.csv") %>%
        clean_names

ctsa_survey <-
        read_csv("data/community_survey_responses.csv")