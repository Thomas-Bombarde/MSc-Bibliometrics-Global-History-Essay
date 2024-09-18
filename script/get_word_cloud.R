# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(httr)
library(jsonlite)
# robust standard errors
library(sandwich)
library(stargazer)
library(lmtest)

setwd("/Users/tombombo/Documents/EH1 TGEH/essay_1_code")
"This script produces saves data on EH vs non-EH articles and
the plots of articles in EH and Econ journals over time"
#######################################
#######################################
# LOAD
#######################################
#######################################

# Load the CSV file
filepath <- "output/all_articles.csv"
articles <- read.csv(filepath, stringsAsFactors = FALSE)
articles <- articles %>% 
  filter(Publication.Year != 2024) %>% 
  filter(!(Source.Title %in% c("ECONOMIC AND DEMOGRAPHIC HISTORY OF SAO PAULO, 1850-1950",
                                     "HISTORICAL METHODS",
                                     "SOCIAL CHANGE, INDUSTRIALIZATION, AND THE SERVICE ECONOMY IN SAO PAULO, 1950-2020"))) 
######################################
#######################################
# WRANGLE
######################################
######################################


# identify EH journals
articles <- articles %>%
  group_by(Publication.Year, Source.Title) %>%
  mutate(n_articles = n(),
        n_citations = sum(Times.Cited..WoS.Core),
        citationsperarticle = n_citations/n_articles) %>% 
  mutate("EH" = as.factor(case_when(
    Source.Title %in% toupper(c("Cliometrica",
                        "Economic History Review",
                        "European Review of Economic History",
                        "Explorations in Economic History",
                        "Historical Methods",
                        "Journal of Economic History", 
                        "Journal of Interdisciplinary History",
                        "Social Science History")) ~ 1,
    TRUE ~ 0
  ))) %>% ungroup()

articles <- articles %>%
  mutate(Source.Title = case_when(
    Source.Title == toupper("Cliometrica") ~ "CLIO",
    Source.Title == toupper("Economic History Review") ~ "ECON.HIST.REV.",
    Source.Title == toupper("European Review of Economic History") ~ "EU.REV.ECON.HIST.",
    Source.Title == toupper("Explorations in Economic History") ~ "EXP.ECON.HIST.",
    Source.Title == toupper("Historical Methods") ~ "HIST.METHODS",
    Source.Title == toupper("Journal of Economic History") ~ "J.ECON.HIST.",
    Source.Title == toupper("Journal of Interdisciplinary History") ~ "J.INTER.HIST.",
    Source.Title == toupper("Social Science History") ~ "SOC.SCI.HIST.",
    Source.Title == toupper("American Economic Review") ~ "AMER.ECON.REV",
    Source.Title == toupper("American Economic Review-Insights") ~ "AMER.ECON.REV-INSIGHTS",
    Source.Title == toupper("Economic Modelling") ~ "ECON.MODELLING",
    Source.Title == toupper("Economic Theory") ~ "ECON.THEORY",
    Source.Title == toupper("Journal of Economic Perspectives") ~ "J.ECON.PERSP.",
    Source.Title == toupper("Journal of Economic Literature") ~ "J.ECON.LIT.",
    Source.Title == toupper("Journal of Public Economics") ~ "J.PUBLIC.ECON",
    Source.Title == toupper("Review of Financial Studies") ~ "REV.FIN.STUDIES",
    Source.Title == toupper("Review of Economics and Statistics") ~ "REV.ECON.STAT.",
    Source.Title == toupper("Journal of Monetary Economics") ~ "J.MONETARY.ECON",
    Source.Title == toupper("Economic Letters") ~ "ECON.LETTERS",
    Source.Title %in% c("European Journal of Political Economy", 
                        toupper("European Journal of Political Economy")) ~ "EU.J.POL.ECON.",
    Source.Title == toupper("Journal of Development Economics") ~ "J.DEV.ECON.",
    Source.Title == toupper("Quarterly Journal of Economics") ~ "QUART.J.ECON.",
    Source.Title == toupper("Review of Economic Studies") ~ "REV.ECON.STUDIES",
    Source.Title == "EUROPEAN ECONOMIC REVIEW" ~ "EU.ECON.REV.",
    Source.Title == "Economic Letters" ~ "ECON.LETTERS",
    TRUE ~ Source.Title
  ))


# get source ID
articles <- articles %>% group_by(EH, Source.Title) %>% 
  mutate("Source.ID" = as.factor(Source.Title)) %>% ungroup()

# save
write.csv(articles, "output/articles_cleaned.csv", row.names = FALSE)

######################################
#######################################
# PLOTS
######################################
######################################

# make colour palettes for EH and non-EH journals
n_EH <- articles %>%
  filter(EH == 1) %>%
  ungroup() %>% 
  select("Source.Title") %>% 
  unique() %>% 
  nrow()
n_non_EH <- articles %>%
  filter(EH == 0) %>%
  ungroup() %>% 
  select("Source.Title") %>% 
  unique() %>% 
  nrow()
# Define color palettes for each group
colors_EH <- scales::hue_pal()(n_EH)  # 10 colors for group A
colors_nEH <- scales::viridis_pal()(n_non_EH)  # 10 colors for group B
colors <- c(colors_EH, colors_nEH)
articles_quint$palette <- colors[articles_quint$Source.ID]


# Plot the number of articles per year
ggplot(articles) + 
  geom_point(aes(x = Publication.Year, 
                 y = n_articles, 
                 group = Source.Title,
                 color = Source.Title,
                 alpha = EH), size = 1) +
  geom_line(aes(x = Publication.Year, 
                y = n_articles,
                group = Source.Title,
                color = Source.Title,
                linetype = EH,
                alpha = EH)) +
  theme_minimal() +
  # Change line type styles
  scale_linetype_manual(values = c("dotted", "solid"), 
                        labels = c("Economics, N*", "Economic History")) +
  # Change alpha values
  scale_alpha_manual(values = c(0.7, 1), 
                     labels = c("Economics", "Econoimc History")) +
  # Change breaks in year
  scale_x_continuous(breaks = seq(1999, 2023, by = 2)) +
  # Change axis labels
  labs(x = "Year", y = "Number of Articles") +
  # Combine the linetype and the color legend into one
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 12, family = "serif"), 
        legend) +
  guides(color = guide_legend("Journal", ncol = 3), 
         alpha = "none",
         linetype = guide_legend("Journal Type", position = "top"))


# sum and plot total counts in EH=1 and EH=0 over the years
plot_n_articles <- articles %>%
  group_by(Publication.Year, EH) %>%
  summarise(n_articles = sum(n_articles)) %>%
  ggplot(aes(x = Publication.Year, 
             y = n_articles, 
             col = as.factor(EH),
             shape = as.factor(EH)),
         ) +
  geom_line(size=1) +
  geom_point(size=3) +
  # choose shapes
  scale_shape_manual(values = c(16, 17)) +
  scale_colour_discrete(labels = c("Economics, N*", "Economic History")) +
  # Change breaks in year
  scale_x_continuous(breaks = seq(1999, 2023, by = 2)) +
  # Change axis labels
  labs(x = "Year", y = "Number of Articles") +
  theme_minimal() +
  # Combine the linetype and the color legend into one
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 20, family = "serif"), 
        legend) + 
  guides(color = guide_legend("Journal Type",
                              ncol = 2,
                              override.aes = list("shape" = c(16,17))),
         # no shape legend
         shape = "none")

plot_n_citations <- articles %>% filter(Publication.Year < 2019) %>%
  group_by(Publication.Year, EH) %>%
  summarise(n_citations = sum(n_citations)) %>%
  ggplot(aes(x = Publication.Year, 
             y = n_citations, 
             col = as.factor(EH),
             shape = as.factor(EH)),
         ) +
  geom_line(size=1) +
  geom_point(size=3) +
  # choose shapes
  scale_shape_manual(values = c(16, 17)) +
  scale_colour_discrete(labels = c("Economics, N*", "Economic History")) +
  # Change breaks in year
  scale_x_continuous(breaks = seq(1999, 2023, by = 2)) +
  # Change axis labels
  labs(x = "Year", y = "Number of Citations") +
  theme_minimal() +
  # Combine the linetype and the color legend into one
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 20, family = "serif"), 
        legend) + 
  guides(color = guide_legend("Journal Type",
                              ncol = 2,
                              override.aes = list("shape" = c(16,17))),
         # no shape legend
         shape = "none")

plot_n_citations_per_article <- articles %>% filter(Publication.Year < 2019) %>%
  group_by(Publication.Year, EH) %>%
  summarise(n_citations = sum(n_citations),
            n_articles = sum(n_articles)) %>%
  mutate(citationsperarticle = n_citations/n_articles) %>%
  ggplot(aes(x = Publication.Year, 
             y = citationsperarticle, 
             col = as.factor(EH),
             shape = as.factor(EH)),
         ) +
  geom_line(size=1) +
  geom_point(size=3) +
  # choose shapes
  scale_shape_manual(values = c(16, 17)) +
  scale_colour_discrete(labels = c("Economics, N*", "Economic History")) +
  # Change breaks in year
  scale_x_continuous(breaks = seq(1999, 2023, by = 2)) +
  # Change axis labels
  labs(x = "Year", y = "Citations per Article") +
  theme_minimal() +
  # Combine the linetype and the color legend into one
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 20, family = "serif"), 
        legend) + 
  guides(color = guide_legend("Journal Type",
                              ncol = 2,
                              override.aes = list("shape" = c(16,17)),
                              title.position = "top"),
         # no shape legend
         shape = "none")

# save all plots

ggsave("output/plot_n_articles.png", plot = plot_n_articles, device = "png")
ggsave("output/plot_n_citations.png", plot = plot_n_citations, device = "png")
ggsave("output/plot_n_citations_per_article.png", plot = plot_n_citations_per_article, device = "png")

articles <- articles %>%  
  mutate("EH" = as.factor(EH),
         "Year.Since.Publication" = as.factor(2023-Publication.Year))

# A REGRESSION
# get mean number of citations since year of publication per Source.Title 
model <- lm(log(citationsperarticle) ~ EH*Year.Since.Publication + Source.Title, data = articles)
coeftest(model, vcov = vcovHC(model, type = "HC1"))
# print results
table <- stargazer(model, type = "latex", 
          single.row = TRUE, 
          header = FALSE, 
          digits = 3,
          # robust standard errors
          se = list(vcovHC(model, type = "HC1")),
          covariate.labels = c("Economic history",
                               paste0("Economic history, ", (rep(1:24)), "yrs since publication")),
          omit = c("Source.Title", "^Year.Since.Publication")
          )
# save table
write.table(table, "output/regression_table.txt", row.names = FALSE, col.names = FALSE)


#######################################
# PLOT WORD CLOUDS
#######################################

# Extract the article names
article_names <- articles$Article.Title  # Assuming the column with article names is 'name'

# Function to clean and tokenize text
clean_tokenize <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  words <- unlist(strsplit(text, split = "\\s+"))
  return(words)
}

# Tokenize all article names
all_words <- unlist(lapply(article_names, clean_tokenize))

# Remove filler words (stop words)
stop_words <- stopwords("en")
filtered_words <- all_words[!all_words %in% stop_words]

# get keywords
# sperate keywords by ','
keywords <- articles$Keywords
keywords <- unlist(strsplit(keywords, split = ", "))


# Calculate word frequency
word_freq <- data.frame(table(filtered_words))
keyword_freq <- data.frame(table(keywords))

colnames(word_freq) <- c("word", "freq")
colnames(keyword_freq) <- c("word", "freq")

# Sort by frequency
word_freq <- word_freq %>% arrange(desc(freq))
keyword_freq <- keyword_freq %>% arrange(desc(freq))

# Create the word cloud plot
wordcloud(words = word_freq$word, 
          freq = word_freq$freq, scale = c(4, 0.5), 
          min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
ggsave("word_TITLES_cloud.png", plot = last_plot(), device = "png")
wordcloud(words = keyword_freq$word, 
          freq = keyword_freq$freq, scale = c(4, 0.5), 
          min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
ggsave("word_KEYWORDS_cloud.png", plot = last_plot(), device = "png")

#######################################
#######################################
# SAVE
#######################################
#######################################

# save the word frequency data frame
write.csv(word_freq, "output/word_freq.csv", row.names = FALSE)
write.csv(keyword_freq, "output/keyword_freq.csv", row.names = FALSE)
