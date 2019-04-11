library(tidyverse)

# Comparing value at current and constant prices  -----------------
# They should be equal for the year 2010 (reference year)

load(file = paste0("../inputs/WIOD_Nov16/R_Files/WIOT", 2010, "_October16_ROW.RData"))

wiot_current_price <- 
  wiot %>% 
  filter(!(IndustryDescription %in% c("Total intermediate consumption", "Output at basic prices"))) %>% 
  select(-TOT) %>% 
  gather(key = use_sector, value = value, -c(1:5)) %>% 
  separate(use_sector, into = c("use_country", "use_sector"), 3, convert = TRUE) 


VA_current_price <- 
  wiot_current_price %>% 
  filter(RNr %in% 66:71 & use_sector %in% 1:56) %>% 
  .$value %>% sum()

VA_deflated <- 
  readRDS(file = "../outputs/wiot_2010_deflated.rds") %>% 
  filter(RNr %in% 66:71 & use_sector %in% 1:56) %>%
  .$value %>% sum()

VA_current_price - VA_deflated

fd_current_price <- 
  wiot_current_price %>% 
  filter(RNr %in% 1:56 & use_sector %in% c(57,59,60)) %>% 
  .$value %>% sum()
fd_deflated <- 
  readRDS(file = "../outputs/wiot_2010_deflated.rds") %>% 
  filter(RNr %in% formatC(1:56, width=2, flag="0") & 
           use_sector %in% formatC(c(57,59,60), width=2, flag="0")) %>% 
  .$value %>% sum()
fd_current_price - fd_deflated #Not necessarily equal due to the GRAS balance


# Testing initial data ----------------------------------------------------

# what is the initial delta for year 2010?
load(file = paste0("../inputs/WIOD_Nov16/R_Files/WIOT", 2010, "_October16_ROW.RData"))

wiot_current_price <- 
  wiot %>% 
  filter(!(IndustryDescription %in% c("Total intermediate consumption", "Output at basic prices"))) %>% 
  select(-TOT) %>% 
  gather(key = use_sector, value = value, -c(1:5)) %>% 
  separate(use_sector, into = c("use_country", "use_sector"), 3, convert = TRUE) 

LHS <- # Value added, inclusive of taxes in final demand
  wiot_current_price %>% 
  filter(RNr %in% 66:71 & use_sector %in% 1:56) %>% 
  .$value %>% sum()

RHS <- 
  wiot_current_price %>% 
  filter(use_sector %in% 57:61 & RNr %in% 1:56) %>% 
  .$value %>% sum()

LHS - RHS # delta


# Comparing value added and final demand ----------------------------------


diff_VA_FD <- function(year, country, load_wiot = TRUE) {
  
  if (load_wiot) {
    wiot <- readRDS(paste0("../outputs/wiot_", year, "_deflated.rds"))  
  }
  
  VA <- 
    wiot %>% 
    filter(RNr %in% 66:71 & use_country == country & use_sector %in% 1:56)
  
  exports <- 
    wiot %>% 
    filter(Country == country & RNr %in% 1:56 & use_country != country) %>% 
    group_by(Country, RNr, IndustryCode) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(use_country = country, use_sector = 62)
  
  FD <- #Final demand
    wiot %>% 
    filter(Country == country  & RNr %in% 1:56 &
             use_country == country & use_sector %in% 57:61) %>% 
    bind_rows(., exports)
  
  imports <- 
    wiot %>% 
    filter(Country != country & RNr %in% 1:56 &
             use_country == country & use_sector %in% 1:56) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value)) %>% 
    mutate(Country = country, RNr = 72, IndustryCode = "Imports")
  
  
  diff <- 100 * ( sum(FD$value) - (sum(VA$value) + sum(imports$value)) ) / sum(FD$value)
  
  print(paste0("The difference for ", country, year, " is ", round(diff, 2)," %"))
  
  out <- tibble(year = year, country = country, diff = diff)
}

test <- diff_VA_FD(2008, "FRA")

#Compute difference for all countries
countries <- 
  readRDS(paste0("../outputs/wiot_", 2010, "_deflated.rds")) %>%
  filter(Country != "TOT") %>% 
  .$Country %>%
  unique()

diff <- list()
for (year in 2008:2014) {
  wiot <- readRDS(paste0("../outputs/wiot_", year, "_deflated.rds"))  
  diff[[year-2007]] <- countries %>% 
    map(function(x) {diff_VA_FD(x, year = year, load_wiot = FALSE)}) %>% 
    bind_rows()
}
diff <- bind_rows(diff)

max(abs(diff$diff))

saveRDS(diff, "../outputs/diff_VA_FD.rds")

diff <- readRDS("../outputs/diff_VA_FD.rds")



# Compute GDPs -------------------------------------------------------------

getGDP <- function(year, type = "deflated") {
  #' type: deflated or current

  if (type == "deflated") 
  { 
    dir <- "../outputs"
  } else if (type == "current") {
    dir <- "../tmp/WIOD_R" 
  } else {message("Select type: Do you want deflated data or current prices?")}
  
  GDP_dollars <- 
    readRDS(paste0(dir, "/wiot_", year, "_", type, ".rds")) %>% 
    filter(RNr %in% c(66,70)) %>% 
    group_by(use_country) %>% 
    summarise(value = sum(value)) %>% 
    mutate(year = year) %>% 
    select(year, use_country, value) %>% 
    rename(country = use_country)
  
  #Convert GDP in euros
  
  exchange_rate <- readRDS("../outputs/exchange_rates.rds") %>% 
    rename(country = Country, year = Year)
  
  GDP_euros <- 
    GDP_dollars %>% 
    left_join(., exchange_rate, by = c("year", "country")) %>% 
    mutate(value = value / exchange_rate) %>% 
    select(year, country, value)
  
  GDP <- 
    bind_rows(GDP_dollars %>% 
                mutate(unit = ifelse(type == "deflated", 
                                     "2010 USD",
                                     "current USD")), 
              GDP_euros %>% 
                mutate(unit = ifelse(type == "deflated", 
                                     "2010 euros",
                                     "current euros"))) %>% 
    mutate(value = value / 10^6) %>% 
    select(country, year, unit, everything())
  
}

GDP_WIOD <- crossing(type = c("current", "deflated"),
                year = 2008:2014) %>% 
  pmap(getGDP) %>% 
  bind_rows() %>% 
  mutate(source = "WIOD") %>% 
  select(country, year, unit, source, value, everything())

saveRDS(GDP_WIOD, "../outputs/GDP_WIOD.rds")


# Compare GDP at current and deflated prices -------------------------------------------------

GDP_WIOD %>% 
  filter(unit %in% c("2010 USD", "current USD")) %>% 
  filter(country == "FRA") %>% 
  ggplot(aes(x = year, y = value, colour = unit)) +
  geom_line()
         

# Print TES as csv ---------------------------------------------------------------

selected_year <- 2010

wiot <- 
  readRDS(paste0("../tmp/WIOD_R/wiot_", selected_year, "_current.rds"))

TES <- 
  wiot %>%
  # aggregate all other countries
  mutate(Country = case_when(
    Country == "FRA" ~ "FRA",
    Country == "TOT" ~ "TOT",
    TRUE             ~ "ROW"),
    use_country = ifelse(use_country == "FRA", "FRA", "ROW")) %>%
  group_by(IndustryCode, Country, RNr, Year, use_country, use_sector) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  #aggregate imports
  mutate(supply = case_when(
    Country == "FRA" ~ IndustryCode,
    Country == "ROW" ~ "Imports",
    TRUE             ~ IndustryCode
  )) %>% 
  mutate(supply = as.factor(supply)) %>% 
  mutate(supply = factor(supply, levels = levels(supply)[c(1:26,28:37,40:55,58:60,62,61,27,57,56,63,39,38)])) %>% 
  #agregate exports
  mutate(use = ifelse(use_country == "FRA", 
                      paste(use_country, use_sector, sep = "_"),
                      use_country)) %>% 
  group_by(supply, use) %>% 
  summarise(value = sum(value)) %>% 
  mutate(use = as.factor(use),
         use = factor(use, levels = c(paste0("FRA_", 1:61), "ROW"))) %>% 
  mutate(value = ifelse(supply %in% levels(supply)[57:63] & use == "ROW", 0, value)) %>% 
  mutate(year = selected_year) %>% 
  select(year, everything())

#TES in dollars

TES_dollars <- 
  TES %>% 
  spread(key = use, value = value) 
write.csv2(TES_dollars, file = "../tmp/TES_France_dollars.csv", row.names = F)

#TES in euros

exchange_rate <- readRDS("../outputs/exchange_rates.rds") %>% 
  rename(country = Country, year = Year)

TES_euros <- 
  TES %>%
  mutate(rate = exchange_rate %>% 
              filter(year == 2010 & country == "FRA") %>% 
              .$exchange_rate) %>% 
  mutate(value = value / rate) %>% 
  select(-rate) %>% 
  spread(key = use, value = value) 
write.csv2(TES_euros, file = "../tmp/TES_France_euros.csv", row.names = F)


# Get final consumption ---------------------------------------------------

getFC <- function(year, type = "deflated") {
  #' type: deflated or current
  
  if (type == "deflated") 
  { 
    dir <- "../outputs"
  } else if (type == "current") {
    dir <- "../tmp/WIOD_R" 
  } else {message("Select type: Do you want deflated data or current prices?")}
  
  FC <- 
    readRDS(paste0(dir, "/wiot_", year, "_", type, ".rds")) %>% 
    filter(RNr %in% 66:70 & use_sector %in% 57:61) %>% 
    group_by(use_country) %>% 
    summarise(value = sum(value)) %>% 
    select(year, use_country, value) %>% 
    rename(country = use_country)
}

FC_current <- 
  2008:2014 %>% 
  map(getGDP, type = "current") %>% 
  bind_rows()
saveRDS(FC_current, "../outputs/FC_current.rds")



# World Bank --------------------------------------------------------------

GDP_WB_current <- 
  read_csv("../inputs/world_bank/GDP_current/API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv", skip = 4) %>% 
  gather(key = year, value = value, -c(1:4)) %>% 
  filter(year %in% 2008:2014) %>% 
  mutate(year = as.integer(year),
         value = as.numeric(value)) %>% 
  rename(country = `Country Code`) %>% 
  mutate(unit = "current USD", source = "World Bank") %>% 
  select(year, country, unit, source, value) 
  
GDP_WB_constant <- 
  read_csv("../inputs/world_bank/GDP_constant/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv", skip = 4) %>% 
  gather(key = year, value = value, -c(1:4)) %>% 
  filter(year %in% 2008:2014) %>% 
  mutate(year = as.integer(year),
         value = as.numeric(value)) %>% 
  rename(country = `Country Code`) %>% 
  mutate(unit = "2010 USD", source = "World Bank") %>% 
  select(year, country, unit, source, value)

GDP_WB <- 
  bind_rows(GDP_WB_current, GDP_WB_constant) %>% 
  mutate(value = value / 10^12)

saveRDS(GDP_WB, "../tmp/GDP_WB.rds")

remove(GDP_WB_constant, GDP_WB_current)


#Comparing data
bind_rows(readRDS("../tmp/GDP_WB.rds"),
          GDP_WIOD) %>% 
  filter(unit == "2010 USD") %>% 
  filter(country == "ESP") %>% 
  ggplot(aes(x = year, y = value, colour = source)) + 
  geom_line()

# Eurostat ----------------------------------------------------------------

country_codes <- read.csv2("../inputs/equivalence_tables/country_codes.csv", 
                           stringsAsFactors = FALSE) %>% 
  rename(GEO = Name, Country_code = Code)

exchange_rate <- readRDS("../outputs/exchange_rates.rds")

eurostat <- 
  read_csv("../inputs/Eurostat/nama_10_gdp/nama_10_gdp_1_Data.csv", 
           locale = locale(encoding = "ISO-8859-1"),
           col_types = "iccccc") %>% 
  select(-`Flag and Footnotes`) %>% 
  mutate(Value = gsub(x = Value, pattern = ",", replacement = ""),
         Value = gsub(x = Value, pattern = ":", replacement = NA),
         Value = as.numeric(Value)) %>% 
  #Add country codes
  mutate(GEO = gsub(x = GEO, pattern = "Germany \\(until 1990 former territory of the FRG\\)", replacement = "Germany"),
         GEO = gsub(x = GEO, pattern = "Macedonia, the former Yugoslav Republic of", replacement = "Former Yugoslav Republic of Macedonia, the")) %>% 
  left_join(., country_codes %>% 
              mutate(GEO = gsub(x = GEO, pattern = "Czechia", replacement = "Czech Republic")), 
            by = "GEO") %>% 
  filter(UNIT == "Chain linked volumes (2010), million euro") %>% 
  select(TIME, Country_code, GEO, everything()) %>% 
  #Add the exchange rate of 2010, to change from 2010 constant euros to 2010 constant dollards
  left_join(., 
            exchange_rate %>% 
              filter(year == 2010) %>% 
              select(-c(year, exchange_deflator)) %>% 
              rename(Country_code = country), 
            by = c("Country_code")) %>% 
  mutate(Value = Value * exchange_rate) %>% 
  select(-exchange_rate) %>% 
  rename(year = TIME, value = Value) %>% 
  mutate(Source = "Eurostat")

gdp_eurostat <- 
  eurostat %>% 
  filter(NA_ITEM == "Gross domestic product at market prices" & 
           UNIT == "Current prices, million euro") %>% 
  mutate(value = value / 10^6) %>% 
  mutate(unit = "current euros", source = "Eurostat") %>% 
  select(country, year, unit, source, value)
saveRDS(gdp_eurostat, "../tmp/eurostat_constant2010USD.rds")
  

bind_rows(gdp_eurostat, 
          GDP_WIOD %>% filter(unit == "current euros")) %>% 
  filter(country == "FRA") %>% 
  ggplot(aes(x = year, y = value, colour = source)) + geom_line()

# Test for 2010 -----------------------------------------------------------

current_tes <- readRDS("../tmp/WIOD_R/wiot_2010_current.rds") %>% 
  filter(use_country == "FRA")

deflated_tes <- readRDS("../outputs/wiot_2010_deflated.rds") %>% 
  filter(use_country == "FRA")
  
current_tes %>% filter(Country == "TOT") %>% .$value %>% sum()

deflated_tes %>% filter(Country == "TOT") %>% .$value %>% sum()



