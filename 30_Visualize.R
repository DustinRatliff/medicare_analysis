medicare_warren %>% 
        filter(Credential %in% c("DO","MD","NP","PA")) %>%
        group_by(City, Credential) %>% 
        summarise(n = n()) %>% 
        spread(key = City, value = n)

ggplot2(data = medicare_ohio, aes(x = City, y=))