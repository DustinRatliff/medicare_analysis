medicare_warren_geocode_pcp <- medicare_warren_geocode %>%
        filter(
                `Primary specialty` %in%
                        c(
                                "FAMILY MEDICINE",
                                "GENERAL PRACTICE",
                                "INTERNAL MEDICINE",
                                "NURSE PRACTITIONER",
                                "PHYSICIAN ASSISTANT",
                                "GERIATRIC MEDICINE"
                        ) |
                        `Secondary specialty 1` %in%
                        c(
                                "FAMILY MEDICINE",
                                "GENERAL PRACTICE",
                                "INTERNAL MEDICINE",
                                "NURSE PRACTITIONER",
                                "PHYSICIAN ASSISTANT",
                                "GERIATRIC MEDICINE"
                        ) |
                        `Secondary specialty 1` %in%
                        c(
                                "FAMILY MEDICINE",
                                "GENERAL PRACTICE",
                                "INTERNAL MEDICINE",
                                "NURSE PRACTITIONER",
                                "PHYSICIAN ASSISTANT",
                                "GERIATRIC MEDICINE"
                        )
        )

        
primary_specialty_chart <- medicare_warren_geocode_pcp %>%
        filter(
                `Primary specialty` %in%
                        c(
                                "FAMILY MEDICINE",
                                "GENERAL PRACTICE",
                                "NURSE PRACTITIONER",
                                "PHYSICIAN ASSISTANT",
                                "GERIATRIC MEDICINE"
                        )) %>%
        group_by(City, `Primary specialty`) %>% 
        summarise(n = n()) %>%
        
        spread(key = City, value = n) %>%
        replace(is.na(.),0) %>%
        ungroup %>%
        adorn_totals %>%
        tableGrob(rows = NULL)

grid.arrange(primary_specialty_chart)


counties <- map_data("county") %>% 
        filter(region == "ohio" & 
                       subregion == "warren")

warren_map <- get_map(location = "warren county, ohio", 
                      zoom = 10,
                      source= "google",
                      maptype = "roadmap")

provider_map <- ggmap(warren_map) + 
        geom_polygon(data = counties,
                     aes(x=long, y=lat, group=group),
                     fill = NA,
                     color = "steelblue",
                     size  = 1) +
        scale_x_continuous(limits = c(-84.375,-83.95), expand = c(0,0)) + 
        scale_y_continuous(limits = c(39.26, 39.6)) +
        geom_point(data = medicare_warren_geocode_pcp,
                   aes(x = lon, y = lat),
                   size=1,
                   color = "firebrick") +
        labs(title = "Distribution of Medicare Primary Care Providers",
             subtitle = "Warren County, Ohio") +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())

ggsave(
        plot = provider_map,
        file = "maps/provider_map.png",
        width = 8,
        height = 8,
        type = "cairo-png"
)

vars <-  {
        c(
                "B25034_002",
                "B25034_001",
                "B25034_003",
                "B25034_004",
                "B01002_001",
                "B19083_001",
                "B17001_001",
                "B17001_002",
                "B02001_001",
                "B02001_002",
                "B01001_003",
                "B01001_020",
                "B01001_021",
                "B01001_022",
                "B01001_023",
                "B01001_024",
                "B01001_025",
                "B01001_027",
                "B01001_044",
                "B01001_045",
                "B01001_046",
                "B01001_047",
                "B01001_048",
                "B01001_049",
                "B08141_001",
                "B08141_002",
                "B19013_001",
                "B02001_001",
                "B02001_002",
                "B02001_003",
                "B02001_004",
                "B02001_005",
                "B02001_006",
                "B02001_007",
                "B02001_008",
                "B02001_009",
                "B02001_010",
                "B03002_001",
                "B03002_002",
                "B03002_003",
                "B03002_004",
                "B03002_005",
                "B03002_006",
                "B03002_007",
                "B03002_008",
                "B03002_009",
                "B03002_010",
                "B03002_011",
                "B03002_012",
                "B03002_013",
                "B03002_014",
                "B03002_015",
                "B03002_016",
                "B03002_017",
                "B03002_018",
                "B03002_019",
                "B03002_020",
                "B03002_021",
                "B15002_001",
                "B15002_002",
                "B15002_003",
                "B15002_004",
                "B15002_005",
                "B15002_006",
                "B15002_007",
                "B15002_008",
                "B15002_009",
                "B15002_010",
                "B15002_011",
                "B15002_012",
                "B15002_013",
                "B15002_014",
                "B15002_015",
                "B15002_016",
                "B15002_017",
                "B15002_018",
                "B15002_019",
                "B15002_020",
                "B15002_021",
                "B15002_022",
                "B15002_023",
                "B15002_024",
                "B15002_025",
                "B15002_024",
                "B15002_025",
                "B15002_026",
                "B15002_027",
                "B15002_028",
                "B15002_029",
                "B15002_030",
                "B15002_031",
                "B15002_032",
                "B15002_033",
                "B15002_034",
                "B15002_035",
                "B15003_001",
                "B15003_002",
                "B15003_003",
                "B15003_004",
                "B15003_005",
                "B15003_006",
                "B15003_007",
                "B15003_008",
                "B15003_009",
                "B15003_010",
                "B15003_011",
                "B15003_012",
                "B15003_013",
                "B15003_014",
                "B15003_015",
                "B15003_016",
                "B15003_017",
                "B15003_018",
                "B15003_019",
                "B15003_020",
                "B15003_021",
                "B15003_022",
                "B15003_023",
                "B15003_024",
                "B15003_025",
                "B25105_001",
                "B01003_001",
                "B27001_001",
                "B27001_002",
                "B27001_003",
                "B27001_004",
                "B27001_005",
                "B27001_006",
                "B27001_007",
                "B27001_008",
                "B27001_009",
                "B27001_010",
                "B27001_011",
                "B27001_012",
                "B27001_013",
                "B27001_014",
                "B27001_015",
                "B27001_016",
                "B27001_017",
                "B27001_018",
                "B27001_019",
                "B27001_020",
                "B27001_021",
                "B27001_022",
                "B27001_023",
                "B27001_024",
                "B27001_025",
                "B27001_026",
                "B27001_027",
                "B27001_028",
                "B27001_029",
                "B27001_030",
                "B27001_031",
                "B27001_032",
                "B27001_033",
                "B27001_034",
                "B27001_035",
                "B27001_036",
                "B27001_037",
                "B27001_038",
                "B27001_039",
                "B27001_040",
                "B27001_041",
                "B27001_042",
                "B27001_043",
                "B27001_044",
                "B27001_045",
                "B27001_046",
                "B27001_047",
                "B27001_048",
                "B27001_049",
                "B27001_050",
                "B27001_051",
                "B27001_052",
                "B27001_053",
                "B27001_054",
                "B27001_055",
                "B27001_056",
                "B27001_057",
                "B25106_001",
                "B25106_002"
        )
}

map_theme_percentage <- function(Percentage, ...) {
        ggplot(data = ..., aes(fill = Percentage, color = Percentage)) +
                geom_sf() +
                coord_sf(crs = 26915) +
                scale_fill_viridis(name = "Percentage") +
                scale_color_viridis(name = "Percentage") +
                theme(panel.background = element_blank()) +
                theme(
                        axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()
                ) +
                theme(
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()
                )
}

warren_acs <- get_acs(
        state = "OH",
        county = "Warren",
        year = 2016,
        geography = "tract",
        variables = vars,
        output = "wide",
        geometry = TRUE
)

warren_acs <- warren_acs %>%
        mutate(Older64Male = B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E,
                     Older64Female = B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E,
                     Older64 = Older64Male + Older64Female)

over65_map <- warren_acs %>%
        mutate(Percentage = Older64 / B02001_001E * 100) %>%
        select(Percentage) %>%

        map_theme_percentage(Percentage = Percentage) +
        labs(title = "Percentage of Population 65+ by Census Tract",
             subtitle = "Warren County, OH")
ggsave(
        plot = over65_map,
        file = "maps/over65_map.png",
        width = 8,
        height = 8,
        type = "cairo-png"
)
