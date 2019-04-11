#Prepare Price deflators

library(tidyverse)
library(readxl)

dir.create("../tmp", showWarnings = F)


# Country codes ----------------------------------------------------------------

# with names adapted to match UN formulations
country_codes <- read.csv2("../inputs/equivalence_tables/country_codes.csv", 
                           stringsAsFactors = FALSE) %>% 
  mutate(UN_Name = as.factor(Name)) %>% 
  # Insert names used by the UN. New name (UN Name) = old_name
  mutate(UN_Name = fct_recode(UN_Name, "China, People's Republic of" = "China"),
         UN_Name = fct_recode(UN_Name, "Czech Republic" = "Czechia"),
         UN_Name = fct_recode(UN_Name, "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom"),
         UN_Name = fct_recode(UN_Name, "Republic of Korea" = "Korea, Republic of" ),
         UN_Name = fct_recode(UN_Name, "United States" = "United States of America" )
  ) %>% 
  mutate(UN_Name = as.character(UN_Name))


# Get WIOD countries ------------------------------------------------------

load(file = paste0("../inputs/WIOD_Nov16/R_Files/WIOT", 2010, "_October16_ROW.RData"))
WIOD_countries <- unique(wiot$Country)
remove(wiot)

# Compute Rest of World price coefficiens ----------------------------------------------

#For the rest of the world, all price.indexs were computed as averages 
# of the respective price.indexs for Brazil, China, India, Indonesia, Mexico and Russia. 
# weighted by current value shares

va_weights <- read_delim(file = "../inputs/price_deflators/unsd_snaAma_ValueAdded_constantUSD.xls", 
                         delim = "\t",
                         comment = "#",
                         col_types = cols(Year = "i", .default = c("c"))) %>% 
  gather(key = item, value = value, -c(1:3)) %>% 
  mutate(value = str_replace_all(value, ",", "."),
         value = as.numeric(value)) %>% 
  rename(Country = `Country or area`) %>% 
  filter(item == "Total Value Added") %>% 
  filter(str_detect(Country, "Brazil|China, People's Republic|India|Indonesia|Mexico|Russia")) %>% 
  mutate(Country = str_replace_all(Country, "China, People's Republic of", "China")) %>% 
  group_by(Year) %>% 
  mutate(share = value / sum(value)) %>% 
  select(Country, Year, share) %>% 
  #Now we add country codes
  left_join(., country_codes %>% rename(Country = Name), by = "Country") %>% 
  rename(Country.code = Code)

# Gross output price index ----------------------------------------

#Load WIOD Socio-Economic accounts
WIOD_SEA <- 
  readxl::read_excel(path = "../inputs/WIOD_SEA_Nov16/WIOD_SEA_Nov16.xlsx",
                     sheet = 2, 
                     na ="NA") %>% 
  gather(key = Year, value = value, -c(1:4)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(Country = country, IndustryCode = code, IndustryDescription = description) %>% 
  select(Country, variable, IndustryCode, everything())



# Update WIOD with latest Eurostat values for deflator of value added ---------------------------------
# 
# Eurostat_VA <- read_csv("../inputs/Eurostat/nama_10_a64/Value_added_deflator/nama_10_a64_1_Data.csv", col_types = "iccccc") %>% 
#   mutate(Value = replace(Value, Value == ":", NA),
#          Value = str_replace_all(Value, ",", ""),
#          Value = as.numeric(Value)) 

# Load exchange rates to convert SEA from local currency to dollars
exchange_rate <- 
  #Exchange rate in USD per unit of local currency
  read_xlsx("../inputs/WIOD_Nov16/Exchange_rates.xlsx", 
            sheet = 2,
            skip = 2) %>% 
  gather(key = Year, value = value, -c(Country, Acronym)) %>% 
  mutate(Year = gsub(x = Year, pattern = "_", replacement = ""),
         Year = as.integer(Year)) %>% 
  select(-Country) %>% 
  rename(Country = Acronym, exchange_rate = value) %>% 
  # Put the right country code for Romania
  mutate(Country = gsub(x = Country, pattern = "ROM", replacement = "ROU")) %>% 
  group_by(Country) %>%
  mutate(exchange_deflator = exchange_rate / exchange_rate[Year == 2010])

saveRDS(object = exchange_rate, file = "../outputs/exchange_rates.rds")


#Correspondance between industry code and sector number
codeToNumber <- readRDS("../inputs/CodeToNumber.rds")

GO_PI <- 
  WIOD_SEA %>% 
  filter(variable == "GO_PI") %>% 
  select(-variable) %>% 
  rename(price.index = value) %>% 
  left_join(., 
            codeToNumber %>% filter(RNr %in% c(1:56)), 
            by = "IndustryCode") %>% 
  select("Country", "IndustryCode", "IndustryDescription", "RNr", everything()) %>% 
  left_join(., 
            exchange_rate %>% 
              select(Country, Year, exchange_deflator), 
            by = c("Country", "Year")) %>%
  mutate(price.index = price.index * exchange_deflator) %>%
  select(-exchange_deflator)

#Get va-weighted price.index for the rest of the world (ROW)
row_GO_PI <- 
  GO_PI %>% 
  filter(Country %in% unique(va_weights$Country.code)) %>% 
  left_join(., va_weights %>% 
              select(Country.code, Year, share) %>% 
              rename(Country = Country.code),
            by = c("Country", "Year")) %>% 
  group_by(Year, IndustryCode, IndustryDescription, RNr) %>% 
  summarise(price.index = weighted.mean(x = price.index, w = share)) %>% 
  mutate(Country = "ROW") %>% 
  select(one_of(names(GO_PI)))

GO_PI <- bind_rows(GO_PI, row_GO_PI)
remove(row_GO_PI)

saveRDS(GO_PI, file = "../tmp/GO_PI.rds")

# Value price index -----------------------------------------------
# (also used for taxes, subsidies, transport and international margin)

VA_PI <- 
  WIOD_SEA %>% 
  filter(variable == "GO_PI") %>%   
  select(-variable) %>% 
  rename(price.index = value) %>% 
  left_join(., codeToNumber, by = "IndustryCode") %>% 
  select("Country", "IndustryCode", "IndustryDescription", "RNr", everything()) %>% 
  left_join(., 
            exchange_rate %>% 
              select(Country, Year, exchange_deflator), 
            by = c("Country", "Year")) %>%
  mutate(price.index = price.index * exchange_deflator) %>%
  select(-exchange_deflator)

#Get va-weighted price.index for the rest of the world (ROW)
row_VA_PI <- 
  VA_PI %>% 
  filter(Country %in% unique(va_weights$Country.code)) %>% 
  left_join(., va_weights %>% 
              select(Country.code, Year, share) %>% 
              rename(Country = Country.code),
            by = c("Country", "Year")) %>% 
  group_by(Year, IndustryCode, IndustryDescription, RNr) %>% 
  summarise(price.index = weighted.mean(x = price.index, w = share)) %>% 
  mutate(Country = "ROW") %>% 
  select(one_of(names(VA_PI)))

VA_PI <- bind_rows(VA_PI, row_VA_PI)
remove(row_VA_PI)

saveRDS(VA_PI, file = "../tmp/VA_PI.rds")

# Intermediate consumption price index ------------------------------------

II_PI <- 
  WIOD_SEA %>% 
  filter(variable == "GO_PI") %>%   
  select(-variable) %>% 
  rename(price.index = value) %>% 
  left_join(., codeToNumber, by = "IndustryCode") %>% 
  select("Country", "IndustryCode", "IndustryDescription", "RNr", everything()) %>% 
  left_join(., 
            exchange_rate %>% 
              select(Country, Year, exchange_deflator), 
            by = c("Country", "Year")) %>%
  mutate(price.index = price.index * exchange_deflator) %>%
  select(-exchange_deflator)

#Get II-weighted price.index for the rest of the world (ROW)
row_II_PI <- 
  II_PI %>% 
  filter(Country %in% unique(va_weights$Country.code)) %>% 
  left_join(., va_weights %>% 
              select(Country.code, Year, share) %>% 
              rename(Country = Country.code),
            by = c("Country", "Year")) %>% 
  group_by(Year, IndustryCode, IndustryDescription, RNr) %>% 
  summarise(price.index = weighted.mean(x = price.index, w = share)) %>% 
  mutate(Country = "ROW") %>% 
  select(one_of(names(II_PI)))

II_PI <- bind_rows(II_PI, row_II_PI)
remove(row_II_PI)

saveRDS(II_PI, file = "../tmp/II_PI.rds")

remove(WIOD_SEA)

# Compute final demand deflators ---------------------------------------------------------------

# Source: https://unstats.un.org/unsd/snaama/selbasicFast.asp
# From this source, we estimate the deflators for final demand of:
#  - household consumption, 
#  - governement consumption
#  - investment
# (and only for these three items: NPISH and inventories are determined differently later on)
# We take current price (cup) and constant prices (cop) to find the deflator



#Load UN data for current prices
fd_cup <- readxl::read_excel(path = "../inputs/price_deflators/unsd_snaAma_GDPbyExpenditures_currentUSD2010.xlsx",
                   sheet = 1, 
                   na ="NA") %>% 
  gather(key = item, value = cup, -c(1:3)) %>% 
  mutate(Year = as.integer(Year))

#Load UN data for constant prices
fd_cop <- readxl::read_excel(path = "../inputs/price_deflators/unsd_snaAma_GDPbyExpenditures_constantUSD2010.xlsx",
                             sheet = 1, 
                             na ="NA") %>%
  gather(key = item, value = cop, -c(1:3)) %>% 
  mutate(Year = as.integer(Year))

passage_UN_WIOD_finalDemand <- read_delim(file = "../inputs/price_deflators/nomenclature_finalDemand_UN-WIOD.txt", 
                                          col_types = "cc", 
                                          delim = "\t") %>% 
  na.omit()



# Compute price index for final demand
fd_PI <- 
  full_join(fd_cup, 
            fd_cop,
            by = c("Country or area", "Currency", "Year", "item")) %>% 
  filter(Year >= 2000) %>% 
  mutate(price.index = 100 * cup / cop) %>% 
  #On passe à la nomenclature du WIOD
  rename(UN = item) %>% 
  full_join(., passage_UN_WIOD_finalDemand, by = "UN") %>% 
  na.omit() %>% 
  select(-c(UN, cup, cop, Currency)) %>% 
  #spread(key = WIOD, value = deflator) %>% 
  #On ajoute les codes pays en trois lettres
  rename(Country = `Country or area`) %>% 
  #Pour cela, on fait bien correspondre les noms 
  mutate(Country = str_replace_all(Country, "China, People's Republic of", "China"),
         #        Country = str_replace_all(Country, "China, Hong Kong SAR", "Hong Kong"),
         Country = str_replace_all(Country, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"),
         Country = str_replace_all(Country, "Republic of Korea", "Korea, Republic of"),
         Country = str_replace_all(Country, "Czech Republic", "Czechia"),
         Country = str_replace_all(Country, "United States", "United States of America")) %>% 
  left_join(., 
            country_codes %>% rename(Country = Name, Country.code = Code), 
            by = "Country") %>% 
  filter(WIOD %in% c("CONS_h", "CONS_g", "GFC")) %>%  #Afin de ne garder que les trois valeurs qui nous intéressent
  mutate(use_sector = case_when(
    WIOD == "CONS_h" ~ 57,
    WIOD == "CONS_g" ~ 59,
    WIOD == "GFC" ~ 60)) %>% 
  select(-WIOD) %>% 
  select(-price.index, everything()) %>% 
  filter(Country.code %in% WIOD_countries)



# Add missing countries ---------------------------------------------------

# There is no data in the UN for Taiwan; and we need a proxy for the ROW

#For Taiwan, we use Korea as a proxy, replacing the 3 missing values
fd_PI <- fd_PI %>% 
  filter(Country == "Korea, Republic of") %>% 
  mutate(Country = "Taiwan", 
         Country.code = "TWN") %>% 
  bind_rows(., fd_PI) %>% 
  arrange(Country)

#Get va-weighted price.index for the rest of the world (ROW)
row_fd_PI <- left_join(va_weights, 
                       fd_PI, 
                       by = c("Country", "Year", "Country.code", "UN_Name")) %>% 
  group_by(Year, use_sector) %>% 
  summarise(price.index = weighted.mean(x = price.index, w = share)) %>% 
  mutate(Country = "Rest of the world", Country.code = "ROW", UN_Name = "Rest of the world") 

fd_PI <- bind_rows(fd_PI, row_fd_PI) 

remove(fd_cop, fd_cup, row_fd_PI)

saveRDS(fd_PI, file = "../tmp/fd_PI.rds")


# GDP price index ---------------------------------------------------------

GDP_PI <- 
  read_delim(file = "../inputs/price_deflators/unsd_snaAma_GDP_ImplicitPriceDeflator_USD.txt",
                     delim = "\t",
                     comment = "#",
                     col_types = c("cciddd")) %>% 
  filter(Year >= 2000) %>% 
  rename(price.index = `Implicit Price Deflator`) %>% 
  select(`Country or area`, Year, price.index) %>% 
  left_join(., 
            country_codes %>% 
              select(-Name) %>% 
              rename(`Country or area` = UN_Name),
            by = "Country or area") %>% 
  filter(Code %in% WIOD_countries) %>% 
  select(Code, Year, price.index) %>% 
  rename(Country.code = Code)

# There is no data in the UN for Taiwan; and we need a proxy for the ROW
# For Taiwan, we use Korea as a proxy, replacing the 3 missing values
GDP_PI <- 
  GDP_PI %>% 
  filter(Country.code == "KOR") %>% 
  mutate(Country.code = "TWN") %>% 
  bind_rows(., GDP_PI) %>% 
  arrange(Country.code) 
  
# Get va-weighted price.index for the rest of the world (ROW)
row_GDP_PI <- left_join(va_weights %>% select(Country.code, Year, share), 
                        GDP_PI, 
                        by = c("Year", "Country.code")) %>% 
  group_by(Year) %>% 
  summarise(price.index = weighted.mean(x = price.index, w = share)) %>% 
  mutate(Country.code = "ROW") 

GDP_PI <- bind_rows(GDP_PI, row_GDP_PI) 


# Remove temporary date
remove(va_weights)
