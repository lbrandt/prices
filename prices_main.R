# -------------------------------------------------------------------------
#
# LB 2023-03
# -------------------------------------------------------------------------

require(tidyverse)
require(lubridate)
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
  group_by(item_id, region, shop_code) %>%
  arrange(QDATE) %>%
  mutate(date_last_quote   = lag(QDATE, 1),
         months_since_last = round(as.numeric(QDATE - date_last_quote)/30, 0)) %>%
  mutate(price_last     = lag(price, 1),
         price_pct      = round(100*((price/price_last)^(1/months_since_last)-1), 2),
         indicator_up   = ifelse(price_pct > 0, 1, 0),
         indicator_down = ifelse(price_pct < 0, 1, 0),
         indicator_noch = ifelse(price_pct == 0, 1, 0)) %>%
  filter(months_since_last != 0) 


vvv = filter(prices_from2019_change, QDATE >= "2022-01-01", item_id == 210102)
#clipr::write_clip(vvv)


# Extreme quantiles of unconditional price changes for trimming
prices_stats = ungroup(prices_from2019_change) %>%
  filter(months_since_last == 1) %>%
  summarise(pct_mean  = mean(price_pct),
            pct_max   = max(price_pct),
            trim_up   = quantile(price_pct, 0.9995),
            pct_p95   = quantile(price_pct, 0.95),
            pct_p80   = quantile(price_pct, 0.80), 
            pct_p50   = quantile(price_pct, 0.50),
            pct_p20   = quantile(price_pct, 0.20),
            pct_p05   = quantile(price_pct, 0.05),
            trim_down = quantile(price_pct, 1-0.9995),
            pct_min   = min(price_pct),
            nobs = n())


# Aggregate changes by month
prices_aggregate_by_month = prices_from2019_change %>%
  filter(months_since_last == 1) %>%
  # Trim most extreme 0.1% of price changes
  filter(price_pct < prices_stats$trim_up, price_pct > prices_stats$trim_down) %>%
  group_by(QDATE) %>%
  summarise(share_up   = mean(indicator_up, na.rm = TRUE),
            share_down = mean(indicator_down, na.rm = TRUE),
            share_noch = mean(indicator_noch, na.rm = TRUE),
            # Conditional distribution of price changes
            pct_up_mean = mean(price_pct[indicator_up==1]),
            pct_up_p95  = quantile(price_pct[indicator_up==1], 0.95),
            pct_up_p80  = quantile(price_pct[indicator_up==1], 0.80), 
            pct_up_p50  = quantile(price_pct[indicator_up==1], 0.50),
            pct_up_p20  = quantile(price_pct[indicator_up==1], 0.20),
            pct_up_p05  = quantile(price_pct[indicator_up==1], 0.05),
            pct_down_mean = mean(price_pct[indicator_down==1]),
            pct_down_p95  = quantile(price_pct[indicator_down==1], 0.95),
            pct_down_p80  = quantile(price_pct[indicator_down==1], 0.80), 
            pct_down_p50  = quantile(price_pct[indicator_down==1], 0.50),
            pct_down_p20  = quantile(price_pct[indicator_down==1], 0.20),
            pct_down_p05  = quantile(price_pct[indicator_down==1], 0.05),
            pct_all_mean = mean(price_pct),
            pct_all_p95  = quantile(price_pct, 0.95),
            pct_all_p80  = quantile(price_pct, 0.80), 
            pct_all_p50  = quantile(price_pct, 0.50),
            pct_all_p20  = quantile(price_pct, 0.20),
            pct_all_p05  = quantile(price_pct, 0.05),
            nobs = n())

#clipr::write_clip(prices_aggregate_by_month)

### Charts
# Plot shares
ggplot(prices_aggregate_by_month) +
  geom_line(aes(x = QDATE, y = share_up, color = "Share of prices rising within the month")) +
  geom_line(aes(x = QDATE, y = share_down, color = "Share of prices falling within the month")) +
  geom_line(aes(x = QDATE, y = share_noch, color = "Share of prices unchanged"))

# Plot unconditional quantiles over time
ggplot(prices_aggregate_by_month) +
  geom_line(aes(x = QDATE, y = pct_all_mean, color = "Average price change")) +
  geom_line(aes(x = QDATE, y = pct_all_p95, color = "95th percentile")) +
  geom_line(aes(x = QDATE, y = pct_all_p50, color = "Median")) +
  geom_line(aes(x = QDATE, y = pct_all_p05, color = "5th percentile"))

# Plot quantiles of price increases
ggplot(prices_aggregate_by_month) +
  geom_line(aes(x = QDATE, y = pct_up_mean, color = "Average price increase")) +
  geom_line(aes(x = QDATE, y = pct_up_p80, color = "80th percentile")) +
  geom_line(aes(x = QDATE, y = pct_up_p50, color = "Median")) +
  geom_line(aes(x = QDATE, y = pct_up_p20, color = "20th percentile"))

# Plot quantiles of price decreases
ggplot(prices_aggregate_by_month) +
  geom_line(aes(x = QDATE, y = pct_down_mean, color = "Average price decrease")) +
  geom_line(aes(x = QDATE, y = pct_down_p80, color = "80th percentile")) +
  geom_line(aes(x = QDATE, y = pct_down_p50, color = "Median")) +
  geom_line(aes(x = QDATE, y = pct_down_p20, color = "20th percentile"))


# Plot kernel densities
uuu = prices_from2019_change %>%
  filter(months_since_last == 1) %>%
  # Trim most extreme 0.1% of price changes
  filter(price_pct < prices_stats$trim_up, price_pct > prices_stats$trim_down) %>%
  filter(QDATE == "2022-10-01") 

summary(uuu)

uuu %>% filter(indicator_up == 1) %>%
  ggplot() +
  geom_density(aes(x = price_pct)) +
  xlim(0, 120)
  
uuu %>% filter(indicator_down == 1) %>%
  ggplot() +
  geom_density(aes(x = price_pct)) +
  xlim(-100, 0)

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




