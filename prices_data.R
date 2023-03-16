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


save.image("../prices-files/prices_workspace.RData")
# Reproduce bread chart
bread = prices %>%
  filter(item_id == selectItem)

# Plot
ggplot() +
  geom_point(aes(x = QDATE, y = price), bread)



