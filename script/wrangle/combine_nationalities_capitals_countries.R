library(dplyr)

# Load the data
setwd("/Users/tombombo/Documents/EH1 TGEH/essay_1_code")
data <- read_csv("input/country_nationalities.csv", encoding = "UTF-8")
data$alpha_3_code[which(data['alpha_3_code']=='ROU')] <- 'ROM'
data <- data %>% select(alpha_3_code, nationality)
world_countries <- read.csv("input/WorldCountriesList.csv")
world_countries <- world_countries %>% left_join(data, by = c("Abreviation" = "alpha_3_code")) %>% 
  select(-c(X, X.1))
write_csv(world_countries, "output/world_countries.csv")

