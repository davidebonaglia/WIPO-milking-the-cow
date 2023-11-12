# Unraveling the Rise of Unicorns: The Billion-Dollar Innovators

#Link to the article: `https://www.wipo.int/global_innovation_index/en/gii-insights-blog/2023/unraveling-rise-unicorns.html`

library(dplyr)
library(boxr)

#source(here::here("src/sql_tools.R"))

## CBInsights Data

#Source: CBInsights, Tracker – The Complete List of Unicorn Companies (`www.cbinsights.com/research-unicorn-companies`), accessed 7th of April 2023

# Import CBInsights raw data
UnicornVal <- readxl::read_excel("/Users/davidebonaglia/Documents/WIPO-milking-the-cow/Unicorns/Data/CB-Insights_Global-Unicorn-Club_2023.xlsx", skip = 2)

## Clean UnicornVal
uni_cln <- UnicornVal %>% 
  select(Company,
         Valuation = `Valuation ($B)`,
         Date = `Date Joined`,
         Country,
         City,
         Industry) %>% 
  
  ## Add ISO3 codes
  mutate(ISO3 = countrycode::countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>% 
  mutate(ISO2 = countrycode::countrycode(Country, origin = 'country.name', destination = 'iso2c')) %>% 
  
  ## Correct some industries names
  ## Industry
  mutate(Industry = case_when(Industry == "Artificial intelligence" ~ "Artificial Intelligence",
                              Industry == "E-commerce & direct-to-consumer" ~ "E-Commerce and Retail",
                              Industry == "Consumer & retail" ~ "E-Commerce and Retail",
                              Industry == "Internet software & services" ~ "Software and ICT Services",
                              Industry == "Hardware" ~ "ICT Hardware and Electrical Equipment",
                              Industry == "Health" ~ "Healthcare Industry",
                              Industry == "Fintech" ~ "Financial Technology",
                              Industry == "Edtech" ~ "Educational Technology", 
                              Industry == "Auto & transportation" ~ "Automobiles and Transports",
                              Industry == "Data management & analytics" ~ "Big Data",
                              Industry == "Supply chain, logistics, & delivery" ~ "Logistics and Delivery",
                              Industry == "Mobile & telecommunications" ~ "Mobile and Telecommunications",
                              Industry == "Travel" ~ "Travel and Leisure",
                              TRUE ~ Industry)) %>% 
  
  ## Clean Industry variable 
  mutate(Industry = case_when(Industry == "Artificial Intelligence" | Industry == "Big Data" ~ "AI and Data Driven",
                              Industry == "Financial Technology" | Industry == "Software and ICT Services" | 
                                Industry == "Mobile and Telecommunications" | Industry == "Cybersecurity" ~ "Technology and Software",
                              Industry == "E-Commerce and Retail" ~ "Digital Sevices and Commerce",
                              Industry == "Healthcare Industry" | Industry == "Educational Technology" ~ "Healthcare and Education",
                              Industry == "Logistics and Delivery" | 
                                Industry == "Automobiles and Transports" ~ "Logistics and Transportation",
                              TRUE ~ Industry))

## GII Data

# Import GII 2023 data on Unicorns (Output Indicator 6.2.2)
# Source: WIPO website, GII 2023 page - related documents: `https://www.wipo.int/publications/en/details.jsp?id=4679`
# 
# **Definition of Ind 6.2.2**: Unicorn valuation, % GDP
# Combined valuation of a country’s unicorns (% of GDP) | 2023
# Total valuation of all unicorns in a country as a percentage of GDP. A unicorn company is a private company with a valuation over USD 1 billion. Unicorn companies worldwide number 1,207 as of April 7, 2023.
# 
# Source: CBInsights, Tracker – The Complete List of Unicorn Companies (`www.cbinsights.com/research-unicorn-companies`); and International Monetary Fund World Economic Outlook Database, October 2022 (`www.imf.org/en/Publications/WEO/weo-database/2022/October`). Data year: 2023

# Import GII 2023 Data on unicorns
GIIData <- readxl::read_excel("/Users/davidebonaglia/Documents/WIPO-milking-the-cow/Unicorns/Data/GII 2023.xlsx", 
                              sheet = "Data") %>% 
  
  ## Filter for Unicorn Indicator
  filter(NUM == "OUT.6.2.2") %>% 
  arrange(desc(VALUE_SCREEN))

GIIData_cln <- GIIData %>% 
  select(ISO3, VALUE_SCREEN, RANK)

data <- uni_cln %>% 
  
  ## Join with GIIData_cln
  inner_join(GIIData_cln, by = "ISO3") %>% 
  
  ## Create period variable
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Period = case_when(Year <= 2015 ~ "2007 - 2015",
                            Year > 2015 & Year <= 2020 ~ "2015 - 2020",
                            Year > 2020 ~ "2020 - 2023")) 


## Fig 1 - time series
# Monthly number of new unicorn companies, 2011-2022

time_series <- data %>% 
  mutate(Month = lubridate::month(Date, label=TRUE),
         Month2 = lubridate::month(Date)) %>% 
  mutate(Period2 = paste0(Month, " - ", Year)) %>% 
  
  group_by(Period2, Month, Year, Month2) %>% 
  summarise(Unicorns = n_distinct(Company),
            Total_Valuation = sum(Valuation)) %>% 
  ungroup() %>% 
  
  arrange(Year, Month2) %>% 
  select(-c("Month", "Year", "Month2"))


## Fig 2 - The explosive growth in the unicorns landscape

# From 55 in 2007-2015, to 360 in 2015-2020, surging to 781 in the last three years
times_series_industries <- data %>% 
  
  group_by(Year, Industry) %>% 
  summarise(Unicorns = n_distinct(Company),
  ) %>% 
  ungroup() %>% 
  
  filter(Year >= 2011 & Year <= 2022) %>% 
  
  tidyr::pivot_wider(names_from = Industry, values_from = Unicorns, id_cols = Year) %>%
  mutate(across(-Year, ~tidyr::replace_na(., 0)))
