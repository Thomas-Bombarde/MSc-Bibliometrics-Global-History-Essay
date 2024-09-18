# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr) 

setwd("/Users/tombombo/Documents/EH1 TGEH/essay_1_code")
### DEPRECATED BY MODEL SPACY IN PYTHON
# Load the CSV file
filepath <- "output/articles_data.csv" # take recently scraped data
articles <- read.csv(filepath, stringsAsFactors = FALSE)

# get summary statistics
names(articles)
journals <- articles %>%
  group_by(Journal) %>%
  summarise(n_articles = n(),
    '80s' = sum(Year >= 1980 & Year < 1990),
    '90s' = sum(Year >= 1990 & Year < 2000),
    '00s' = sum(Year >= 2000 & Year < 2010),
    '10s' = sum(Year >= 2010 & Year < 2020)
  )

head(journals)

#######################################
# SUBSET JOURNALS
#######################################
                          
galofre_vida <- c("\\bCliometrica\\b",
                  "\\bEconomic History Review\\b",
                  "\\bEuropean Review of Economic History\\b",
                  "\\bExplorations in Economic History\\b",
                  "\\bHistorical Methods\\b",
                  "\\bJournal of Economic History\\b", 
                  "\\bJournal of Interdisciplinary History\\b",
                  "\\bSocial Science History\\b")
cioni <- c("Cliometrica", 
           "Economic History Review",
           "European Review of Economic History",
           "Explorations in Economic History",
           "Journal of Economic History")

te5 <- c("American Economic Review",
         "Econometrica", # none
         "Journal of Political Economy",
         "Quarterly Journal of Economics",
         "Review of Economic Studies")

te20 <- c("\\bAmerican Economic Review\\b",
          "\\bThe Review of Financial Studies\\b",
          "\\bJournal of Political Economy\\b",
          "\\bThe Quarterly Journal of Economics\\b",
          "\\bThe Journal of Finance\\b",
          "The Review of Economic Studies\\b",
          "\\bEconometrica\\b",
          "\\bJournal of Economic Perspectives\\b",
          "\\bJournal of Public Economics\\b",
          "\\bReview of Economics and Statistics\\b",
          "\\bJournal of Development Economics\\b",
          "\\bThe Econonmic Journal\\b",
          "\\bJournal of Monetary Economics\\b",
          "\\bEconomic Modelling\\b",
          "\\bJournal of the Euroepan Economic Association\\b",
          "\\bJournal of International Economics\\b",
          "\\bAmerican Economic Association Papers and Proceedings\\b",
          "\\bEconomics Letters\\b",
          "\\bEuropean Economic Review\\b",
          "\\bJournal of Economic Literature\\b") # google scholar's count: https://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bus_economics

te20_eh <- c(te20, galofre_vida)

# filter for the name of of Journal in article whose string subsets any of the te20 strings
article20_eh <- articles %>%
  filter(str_detect(Journal, paste(te20_eh, collapse = "|")))
# fix cliometrica and drop nordic jpe
article20_eh <- article20_eh %>% 
  filter(Journal != "Nordic Journal of Political Economy") %>% 
  filter(Year != "NA") %>% 
  # join observations for all observations contained "Cliometrica" in the journal name, noting that Year should take the non-NA value
  mutate(Journal = ifelse(str_detect(Journal, "Cliometrica"), "Cliometrica", Journal)) 

  

# summarise per journal
journal20_eh <- article20_eh %>%
  group_by(Journal) %>%
  summarise(n_articles = n(),
            '<80s' = sum(Year < 1980),
            '80s' = sum(Year >= 1980 & Year < 1990),
            '90s' = sum(Year >= 1990 & Year < 2000),
            '00s' = sum(Year >= 2000 & Year < 2010),
            '10s' = sum(Year >= 2010 & Year < 2020),
            '20s' = sum(Year >= 2020 & Year < 2030)
  )


#######################################
# GET COUNTRY OF ARTICLE 
#######################################

countries <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", 
               "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", 
               "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", 
               "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina",
               "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", 
               "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", 
               "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", 
               "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", 
               "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", 
               "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", 
               "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece",
               "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
               "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Italy",
               "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea, North",
               "Korea, South", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon",
               "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
               "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands",
               "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia",
               "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal",
               "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia",
               "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay",
               "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda",
               "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa",
               "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles",
               "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
               "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden",
               "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste",
               "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu",
               "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States",
               "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen",
               "Zambia", "Zimbabwe")

article20_eh <- article20_eh %>%
  mutate(Country = ifelse(str_detect(Title, 
                                     paste(countries, collapse = "|")),
                          "International", 
                          "Domestic"))

     
#######################################
#######################################
# SAVE
#######################################
#######################################

write_csv(journal20_eh, "output/journal20_eh.csv")
write_csv(article20_eh, "output/article20_eh.csv")
