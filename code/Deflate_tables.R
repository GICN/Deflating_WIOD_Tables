#This file deflates TES from current to constant prices.

library(tidyverse)

dir.create("../outputs", showWarnings = F)
dir.create("../tmp", showWarnings = F)
dir.create("../tmp/WIOD_R", showWarnings = F)


deflate_TES <- function(year) {

  print(paste0("Deflate Input-Output tables for year ", year))
    
  # Load TES --------------------------------------------------------------

  load(file = paste0("../inputs/WIOD_Nov16/R_Files/WIOT", year, "_October16_ROW.RData"))
  
  #Put wiot in long format and remove subtotals
  io_table <- 
    wiot %>% 
    filter(!(IndustryDescription %in% c("Total intermediate consumption", "Output at basic prices"))) %>% 
    select(-TOT) %>% 
    gather(key = use_sector, value = value, -c(1:5)) %>% 
    separate(use_sector, into = c("use_country", "use_sector"), 3, convert = TRUE) %>% 
    select(-IndustryDescription)
  
  saveRDS(io_table, file = paste0("../tmp/WIOD_R/wiot_", year, "_current.rds"))
  
  
  # I - DEFLATING TES -------------------------------------------------------------
  
  source("price_deflator.R")
  
  # Deflate output of sectors 1 to 56 ----------------------------------------------------------
  
  GO <- 
    #Compute total output at current prices
    io_table %>% 
    filter(use_sector %in% c(1:56)) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value)) %>% 
    #Deflate output to get value at constant prices
    left_join(., 
              GO_PI %>% 
                filter(Year == year) %>% 
                rename(use_sector = RNr, use_country = Country) %>% 
                select(use_country, use_sector, price.index),
              by = c("use_country", "use_sector")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  
  # Deflate value added, taxes, subsidies and transports margins -----------------------------------------------------
  
  VA <- 
    io_table %>% 
    filter(RNr %in% 66:71 & use_sector %in% c(1:56)) %>% 
    left_join(., 
              VA_PI %>% 
                filter(Year == year) %>% 
                rename(use_sector = RNr, use_country = Country) %>% 
                select(use_country, use_sector, price.index),
              by = c("use_country", "use_sector")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  # Deflate sum of final demand for Households, GOV and GFCF ---------------------------------------------
  # Compute and deflate the column sum
  
  HOH_sum <-  
    #Get sum of final demand
    io_table %>% 
    filter(use_sector %in% c(57, 59, 60)) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    # Deflate
    left_join(.,
              fd_PI %>% 
                filter(Year == year) %>% 
                rename(use_country = Country.code) %>% 
                select(use_country, use_sector, price.index),
              by = c("use_country", "use_sector")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  
  #Test: is there any country without deflator (correct countries appearing below)
  HOH_sum %>% 
    filter(is.na(value)) %>% .$use_country %>% unique()
  
  
  # Deflate taxes and subsidies in final demands ----------------------------
  # they are deflated using the total output price of the using country
  Taxes_fd <- 
    io_table %>% 
    filter(RNr %in% 66:71 & use_sector %in% c(57:61)) %>% 
    left_join(., 
              GDP_PI %>% 
                filter(Year == year) %>% 
                rename(use_country = Country.code) %>% 
                select(use_country, price.index),
              by = c("use_country")) %>% 
    mutate(value = value * 100 / price.index) %>%  
    select(-price.index)
  
  
  # Deflate final demand of NPISH and inventories -----------------------------------------------------------
  
  # Deflate cell using output prices of the producing country
  fd_NPISH <- 
    io_table %>% 
    filter(RNr %in% 1:56 & use_sector %in% c(58, 61)) %>% 
    # p11n = p11i = p12n =p12i = p1 
    left_join(., 
              GO_PI %>% 
                filter(Year == year) %>% 
                select(Country, RNr, Year, price.index),
              by = c("Country", "RNr", "Year")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  # Get sum
  NPISH_sum <-
    bind_rows(fd_NPISH, 
              Taxes_fd %>% filter(RNr %in% 66:71 & use_sector %in% c(58, 61))) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value))
  
  
  # Deflate intermediate consumptions ---------------------------
  # (Although it does not appear in the note on the construction of pyp WIOT, it was indicated in an email)
  # we deflate intermediate consumption - both the domestic one and the one of other countries which are actuals exports
  # using industry gross output deflator of the producing country
  
  IC <- 
    io_table %>% 
    filter(RNr %in% 1:56 & use_sector %in% 1:56) %>% 
    left_join(.,
              II_PI %>% 
                filter(Year == year) %>% 
                select(Country, RNr, Year, price.index),
              by = c("Country", "RNr", "Year")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  # Deflate final demand for HOH, GOV and GFCF ----------------------
  # We deflate them row-wise using the output deflator of the producing country
  # This does not appear in the note of on the  construction of wiot, but was indicated in an email
  
  fd_HOH <- 
    io_table %>% 
    filter(RNr %in% 1:56 & use_sector %in% c(57, 59, 60)) %>% 
    left_join(.,
              GO_PI %>% 
                filter(Year == year) %>% 
                select(Country, RNr, Year, price.index),
              by = c("Country", "RNr", "Year")) %>% 
    mutate(value = value * 100 / price.index) %>% 
    select(-price.index)
  
  # II - BALANCING THE MATRIX ----------------------------------------------------
  
  # Balancing errors for inventories ---------------------------------------------------
  
  # cf. WIOD "sources_methode_pyp" by Los et al. (2014), on top of page 5
  
  # Check difference between the sum of value added and final demand
  # Total value added should equal final demand
  # but this might not be true because the deflator we used are not perfect
  # so we need to correct it
  
  # Get VA
  total_VA_deflated <- #Value added, inclusive of taxes, plus imports
    VA %>% 
    .$value %>% 
    sum()
  
  #Get final demand (without taxes)
  total_fd <- 
    sum(HOH_sum$value) +   #column total for HOH, GOV and INV
    sum(NPISH_sum$value) - #column total for NPISH and inventories
    sum(Taxes_fd$value)    #sum of taxes in final demand
  
  delta <- #delta is the current difference between value added and demand, to be redistributed
    (total_VA_deflated - total_fd)
  message(x = paste0("Initial deflation leads to an error of ", 
                     round(100 * delta / total_VA_deflated, 2), 
                     "% between global value added and final demand. We distribute this error to inventories."))
  
  global_inventory <- 
    fd_NPISH %>% 
    filter(use_sector == 61) %>% 
    .$value %>% sum()
  
  inventory_change_rate <- delta / global_inventory
  message(paste0("Inventories are changed by ", round(100*inventory_change_rate), " %."))
  
  dir.create("../tmp/inventory_adjustment/", showWarnings = FALSE)
  tibble(year = year, 
         diff_VA_FD = delta,
         inventory_change_rate = inventory_change_rate) %>% 
    saveRDS(file = paste0("../tmp/inventory_adjustment/inventory_adjustment_", year, ".rds"))
  
  inventory_adjustment <- 
    fd_NPISH %>% 
    filter(use_sector == "61") %>% 
    mutate(value = value * (1+inventory_change_rate))
  
  NPISH_adj <- 
    fd_NPISH %>% 
    filter(use_sector != 61) %>% 
    bind_rows(inventory_adjustment)
  
  NPISH_sum_adj <-
    bind_rows(NPISH_adj, 
              Taxes_fd %>% filter(RNr %in% 66:71 & use_sector %in% c(58, 61))) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value))
  
  total_fd_adj <- 
    sum(HOH_sum$value) +   #column total for HOH, GOV and INV
    sum(NPISH_sum_adj$value) - #column total for NPISH and inventories
    sum(Taxes_fd$value)    #sum of taxes in final demand
  
  remove(fd_NPISH, NPISH_sum, total_fd) #in order to avoid used these not-adjusted values
  
  delta_adj <- #delta is the current difference between value added and demand, to be redistributed
    (total_VA_deflated - total_fd_adj)
  message(x = paste0("The difference after adjustment is ", 
                     round(100 * delta_adj / total_VA_deflated, 2), 
                     "%."))
  
  
  # Applying the GRAS procedure --------------------------------------------------------------

  print("Balance matrix with the GRAS algorithm")
  
  # Getting the column sum of intermediate consumptions
  # These are the total which will be used by the GRAS algorithm
  # u is the row sum
  # v the column sum
  # Z0 is the matrix to balance
  # Be careful to arrange all these vector in the same order...

  total_outputs <- #output for all sectors
    bind_rows(GO,        #output for sectors 1 to 56
              HOH_sum,   #output for final demand columns of HOH, GOV and FGCF
              NPISH_sum_adj) #output for final demand columns of NPISH and Inventories

  total_taxes_and_VA <-
    bind_rows(VA, Taxes_fd) %>%
    group_by(use_country, use_sector) %>%
    summarise(value = sum(value))

  u <- # u represents the exact row sums in GRAS
    GO %>%
    arrange(use_country, use_sector)

  v <- # v represent the exact column sums in GRAS
    full_join(total_outputs %>% rename(output = value),
              total_taxes_and_VA %>% rename(value_added = value),
              by = c("use_country", "use_sector")) %>%
    mutate(product_use = output - value_added) %>%
    select(use_country, use_sector, product_use) %>%
    arrange(use_country, use_sector)

  Z0 <- #Z0 is the matrix to be balanced. Here intermediate consumption and final demand
    bind_rows(IC,          #Deflated intermdiate consumptions
              fd_HOH,      #Deflated final demand of HOH, GOV and GFCF
              NPISH_adj) %>%    #Deflated final demand of NPISH and INV
    mutate(use_sector = ifelse(nchar(use_sector) == 1,
                               paste0("0", use_sector),
                               use_sector),
           RNr  = ifelse(nchar(RNr) == 1,
                         paste0("0", RNr),
                         RNr)) %>%
    arrange(Country, RNr, use_country, use_sector) %>%
    unite(use_country, use_sector, col = use, sep = "_") %>%
    unite(Country, RNr, col = supply, sep = "_") %>%
    spread(key = use, value = value) %>%
    arrange(supply) %>%
    select(-c(IndustryCode, Year))

  rnames <- Z0$supply

  Z0 <-
    Z0 %>%
    select(-supply) %>%
    as.matrix()

  row.names(Z0) <- rnames
  
  source("GRAS.R")

  gras_output <-
    GRAS(Z0,
         u$value,
         v$product_use,
         tolerance = 0.03)


  # Write results ----------------------------------------------------------

  codeToNumber <- readRDS("../inputs/CodeToNumber.rds") %>%
    mutate(RNr  = ifelse(nchar(RNr) == 1,
                         paste0("0", RNr),
                         as.character(RNr)))

  # Add value added to GRAS outputs
  deflated_tes <-
    # Put GRAS output (the matrix of intermediate use) in long format
    gras_output %>%
    as_tibble() %>%
    mutate(supply = row.names(gras_output)) %>%
    select(supply, everything()) %>%
    gather(key = use, value = value, - supply) %>%
    separate(col = supply, into = c("Country", "RNr"), sep = "_") %>%
    separate(col = use, into = c("use_country", "use_sector"), sep = "_") %>%
    left_join(., codeToNumber, by = "RNr") %>%
    select(Country, RNr, IndustryCode, everything()) %>%
    mutate(RNr = gsub(x = RNr, pattern = "^0", replacement = "") %>% as.integer(),
           use_sector = gsub(x = use_sector, pattern = "^0", replacement = "") %>% as.integer()) %>%
    #Add value added
    bind_rows(., VA %>%
                  select(-c(Year))) %>%
    bind_rows(., Taxes_fd %>% select(-c(Year)))

  print("Save output as RDS")
  saveRDS(object = deflated_tes, file = paste0("../outputs/wiot_", year, "_deflated.rds"))

}



# Deflate all tables ------------------------------------------------------

# deflate_TES(2010)

# 2014:2014 %>%
#   walk(deflate_TES, phase = "testing")

c(2008:2014) %>%
  walk(deflate_TES)

#Combine inventory adjustments into a single file
list.files(path = "../tmp/inventory_adjustment", full.names = TRUE) %>% 
  map(readRDS) %>% 
  bind_rows() %>% 
  saveRDS("../outputs/inventory_adjustment_all.rds")



# Create excel and csv files ------------------------------------------------------


get_IO_table <- function(country, year) {
  library(xlsx)
  
  wiot <- readRDS(paste0("../outputs/wiot_", year, "_deflated.rds"))
  
  TEI <- 
    wiot %>% 
    filter(Country == country & RNr %in% 1:56 & 
             use_country == country & use_sector %in% 1:56) 
  
  FD <- 
    wiot %>% 
    filter(Country == country & RNr %in% 1:56 & 
             use_country == country & use_sector %in% 57:61) 
  
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
  
  imports <- 
    wiot %>% 
    filter(Country != country & RNr %in% 1:56 &
             use_country == country & use_sector %in% 1:56) %>% 
    group_by(use_country, use_sector) %>% 
    summarise(value = sum(value)) %>% 
    mutate(Country = country, RNr = 72, IndustryCode = "Imports")
  
  IO_table <- 
    bind_rows(TEI, FD, VA, exports, imports) %>% 
    select(-use_country) %>% 
    mutate(year = year)
    
  use_codes <- tibble(use_sector = 1:62,
                       use_code = (c(IO_table$IndustryCode[1:56], "CONS_h", "CONS_np", "CONS_g", "GFCF", "INVEN", "EXP"))) %>% 
    mutate(use_code = factor(use_code, levels = use_code))
  
  IO_table <- 
    IO_table %>% 
    mutate(IndustryCode = factor(IndustryCode, levels = c(TEI$IndustryCode[1:56], VA$IndustryCode[1:6], "Imports"))) %>% 
    left_join(., use_codes, by = "use_sector") %>% 
    select(-RNr, use_sector) %>% 
    select(year, IndustryCode, use_code, value) %>% 
    arrange(year, use_code, IndustryCode) %>% 
    spread(key = use_code, value = value) 
    
}


dir.create("../outputs/tsv_files", showWarnings = FALSE)
dir.create("../outputs/csv_files", showWarnings = FALSE)
dir.create("../outputs/excel_files", showWarnings = FALSE)

WIOD_countries <- readRDS(paste0("../outputs/wiot_2010_deflated.rds")) %>% 
  filter(Country != "TOT") %>% 
  .$Country %>% 
  unique()

for (country_code in WIOD_countries) {
  IO_table <- 
    2008:2014 %>% 
    map(get_IO_table, country = country_code) %>% 
    bind_rows()
  write.table(IO_table, paste0("../outputs/tsv_files/", country_code, "_niot_2010USD.tsv"), sep="\t", dec = ",", row.names = FALSE)
  write_csv(IO_table, paste0("../outputs/csv_files/", country_code, "_niot_2010USD.csv"))
  write.xlsx2(as.data.frame(IO_table), paste0("../outputs/excel_files/", country_code, "_niot_2010USD.xlsx"), row.names = FALSE)
}



            