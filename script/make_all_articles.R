# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)

setwd("/Users/tombombo/Documents/EH1 TGEH/essay_1_code")

#######################################
#######################################
# LOAD
#######################################
#######################################


articles <- read_excel("input/savedrecs.xls")
articles_econ_eh <- read_csv("output/articles_data_only_econpapers.csv")

for (i in (1:49)){
  articles <- rbind(articles, read_excel(paste("input/savedrecs (", i, ").xls", sep = "")))
}

#######################################
#######################################
# WRANGLE
#######################################
#######################################

eh_journals <- c("Cliometrica",
                      "Economic History Review",
                      "European Review of Economic History",
                      "Explorations in Economic History",
                      "Historical Methods",
                      "Journal of Economic History", 
                      "Journal of Interdisciplinary History",
                      "Social Science History") %>% # make upper case
  toupper()

articles <- articles %>% 
  mutate("EH" = case_when(
    `Source Title` %in% eh_journals ~ 1,
    TRUE ~ 0
      ))

articles_econ_eh <- articles_econ_eh %>% 
  mutate(Journal = case_when(`Journal` == "The Review of Financial Studies" ~ "Review of Financial Studies",
                             `Journal` == "The Review of Economics and Statistics" ~ "Review of Economics and Statistics",
                             `Journal` == "American Economic Review: Insights" ~ "American Economic Review-Insights") %>% 
           toupper)

# I find articles mentioned in the dataframe that have the same title, author, 
# and year as an observation in the WoS Economics journals OR the same title

articles <- articles %>% mutate("ECON" = ifelse(`Source Title` %in% toupper(c("American Economic Review", 
                                                                "Journal of Political Economy",
                                                                "Quarterly Journal of Economics", 
                                                                "Econometrica",
                                                                "Review of Economic Studies")), 1, 0))
articles %>%  # first filter out the articles that are already in the EH journals
  filter(EH == 0) %>%
  filter(
    #ECON == 1 | 
           `Article Title` %in% articles_econ_eh$`Title` | # keep if the title matches one exact row in articles_econ_eh
           # keep if the year, the author, and the journal match one exact row in articles_econ_eh
           mapply(function(year,
                           author, 
                           journal) {
             any(articles_econ_eh$`Year` == year & 
                  sapply(strsplit(author, " |,|;"), function(a) any(grepl(a, articles_econ_eh$`Author`))) & 
                   articles_econ_eh$`Journal` == journal)
           }, 
           `Publication Year`, 
           `Authors`, 
           `Source Title`)) -> new 

articlean <- articles %>% 
  filter(EH == 1) %>% 
  rbind(new)
#######################################
#######################################
# SAVE
#######################################
#######################################

write_csv(articlean, "output/all_articles.csv")
read_csv("output/all_articles.csv")
rm(list = ls())
