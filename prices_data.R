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
item = read.csv("db_item.csv")


selectItem = 210102

# Read full prices database
tic()
prices = read.csv("../prices-files/db_prices.csv")
toc()


# Check data
head(prices)
tail(prices)


prices = prices %>% 
  filter(!is.na(quote_date)) %>%
  mutate(YEAR  = as.numeric(substr(quote_date, 1, 4)),
         MONTH = as.numeric(substr(quote_date, 5, 6)),
         QDATE = ymd(quote_date, truncated = 1)) %>%
  filter(YEAR >= 1997) %>%
  arrange(item_id, region, QDATE)

summary(prices)
object.size(prices)


# Save all
save.image("../prices-files/prices_workspace.RData")
# Reproduce bread chart
bread = prices %>%
  filter(item_id == selectItem)

# Plot
ggplot() +
  geom_point(aes(x = QDATE, y = price), bread)


# Save from 2019 onwards only - for BoE
# Restrict sample
prices_from2019 = filter(prices, QDATE >= "2019-01-01") %>%
  select(-c(item_id_raw, indicator_box))
#rm(prices)

filter(prices_from2019, QDATE == "2023-01-01", item_id == 210102, shop_code == 803)

# Save restricted sample
save(prices_from2019, file = "save_prices_from2019.RData")





