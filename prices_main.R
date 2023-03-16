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


prices_aggregate_by_RSIT = prices %>%
  group_by(region, shop_code, item_id, QDATE) %>%
  summarise(price_change = log(price)-log(lag(price, 1)),
            nobs = n())

