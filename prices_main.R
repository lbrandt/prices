# -------------------------------------------------------------------------
#
# LB 2023-03
# -------------------------------------------------------------------------

require(tidyverse)
require(tictoc)


# Set working directory to script file location

# Retrieve script location when code is run within RStudio
location.thisfile = dirname(rstudioapi::getActiveDocumentContext()$path)
# Retrieve script location if file is called via source()
#location.thisfile = dirname(sys.frame(1)$ofile)

# Setting working directory to file location
setwd(location.thisfile)




# -------------------------------------------------------------------------
# Data I/O
load("../prices-files/prices_workspace.RData")


# -------------------------------------------------------------------------
# Aggregate
# average price by item, time
prices_aggregate_by_item = prices %>%
  group_by(item_id, QDATE) %>%
  summarise(price_ave = mean(price), 
            nobs = n()) 

# Plot bread
prices_aggregate_by_item %>%
  filter(item_id == 210102) %>%
  ggplot() +
  geom_line(aes(x = QDATE, y = price_ave))


# price changes by region, shop, item, time
filter(prices, QDATE == "2023-01-01", item_id == 210102, shop_code == 803)

# Restrict sample
prices_from2019 = filter(prices, QDATE >= "2019-01-01") %>%
  select(-c(item_id_raw, indicator_box))
#rm(prices)

filter(prices_from2019, QDATE == "2023-01-01", item_id == 210102, shop_code == 803)


prices_from2019_change = prices_from2019 %>%
  arrange(item_id, region, shop_code, QDATE) %>%
  group_by(item_id, region, shop_code) %>%
  mutate(date_last_quote   = lag(QDATE, 1),
         months_since_last = round((QDATE - date_last_quote)/30),0) %>%
  mutate(price_pct      = round(100*((price/lag(price, 1))^(1/months_since_last)-1), 2),
         indicator_up   = ifelse(price_pct > 0, 1, 0),
         indicator_down = ifelse(price_pct < 0, 1, 0),
         indicator_noch = ifelse(price_pct == 0, 1, 0))


filter(prices_from2019_change, QDATE == "2022-12-01", item_id == 210102)


# collapse region
prices_from2019_median = prices_from2019 %>%
  group_by(QDATE, item_id, shop_code) %>%
  summarise(price_med = median(price),
            nobs = n())

filter(prices_from2019_median, QDATE == "2022-12-01", item_id == 210102)

prices_aggregate_by_SIT = prices_from2019_median %>%
  arrange(item_id, shop_code) %>%
  group_by(item_id, shop_code) %>%
  mutate(price_change   = round(price_med/lag(price_med, 1)-1, 2),
         indicator_up   = ifelse(price_change>0, 1, 0),
         indicator_down = ifelse(price_change<0, 1, 0),
         indicator_noch = ifelse(price_change==0, 1, 0))



filter(prices_aggregate_by_SIT, QDATE == "2022-12-01", item_id == 210102) %>%
  ggplot() +
  geom_histogram(aes(price_change))
  



prices_aggregate_by_RSIT = prices_from2019 %>%
  group_by(region, shop_code, item_id, QDATE) %>%
  summarise(price_change = price-lag(price, 1),
            nobs = n())




