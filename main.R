#### 1 Set Up Datasets ####

#Set Working Directory
setwd("/Users/quach/Downloads/")

#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("arrow", "dplyr", "tidyverse", "tidygraph", "rtweet", "ggraph", "tidytext", "stopwords", "sentimentr", "lubridate", "textfeatures", "wordcloud", "RColorBrewer", "quanteda", "quanteda.dictionaries", "syuzhet", "Rcpp", "tidyverse", "MASS", "lmtest", "QuantPsyc", "pastecs", "psych", "tidytext", "plyr", "jtools", "dotwhisker", "cowplot", "meta", "metafor", "metaviz")
ipak(packages)

# Define the path where the input data chunks are stored
data_path <- "Data"

# List all data chunks in the directory
chunk_list <- list.files(data_path, pattern = "\\.parquet$", full.names = TRUE)

# Read each data chunk and combine them into one dataframe
all_tweets_df <- chunk_list %>%
  lapply(read_parquet) %>%
  bind_rows()

# Select the columns relevant for later analysis
all_tweets_df <- all_tweets_df[,c("text",'favorite_count', 'retweet_count', "screen_name", "Party", "followers_count")]

# Create two new columns to scan for retweets, and tweet containing URL
all_tweets_df <- all_tweets_df %>%
  mutate(is_retweet = grepl("^RT @", text)) %>%
  mutate(has_URL = grepl(":/", text)) %>%
  distinct(text,screen_name, .keep_all = TRUE)

# Set the aisle
congressRepub <- subset(all_tweets_df, Party == "R")
congressDem <- subset(all_tweets_df, Party == "D")


# Save datasets at crucial checkpoint
#saveRDS(all_tweets_df, file = "all_tweets_df.rds")
#
# Print a message to indicate completion
#cat("All tweets have been read and combined into an RDS file.\n")



#### 2 Set Up Dictionaries ####
## Load in dictionaries provided by the original study
MoralEmotional <- scan("Dictionaries/MoralEmotional.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotional <- strsplit(MoralEmotional, "[[:space:]]+")
TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\n", skipNul = TRUE)
# Trim leading and trailing whitespace from each entry
TopDemocrat <- trimws(TopDemocrat)
# Filter out any empty lines
TopDemocrat <- TopDemocrat[TopDemocrat != ""]
DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)
## Identity Dictionaries
liberalidentity = c("socialist*", "communist*", "marxist*", "leftist*", "liberal*", "left-wing*", "progressive*", "social justice warrior", "antifa", "democrat*", "dem", "dems", "libs")
conservativeidentity = c("conservative*", "gop", "republican*", "libertarian*", "alt-right", "right-wing", "fascist*", "far-right", "far right", "repub", "repubs", "maga")
# Access the VADER lexicon and separate them into positive and negative dicts
lexicon_path <- "Dictionaries/vader_lexicon.txt"
lexicon <- read.table(lexicon_path, sep = "\t", quote = "", comment.char = "", skip = 202)
# Extract terms and sentiment scores
terms <- lexicon[,1]
scores <- as.numeric(lexicon[,2])
# Create dictionaries based on sentiment scores
positive_terms <- terms[scores > 0.05]
negative_terms <- terms[scores < -0.05]
neutral_terms <- terms[scores <= 0.05 & scores >= -0.05]

## Create Dictionaries 
group = quanteda::dictionary(list(liberalidentity = liberalidentity,
                        conservativeidentity = conservativeidentity,
                        TopDemocrat = TopDemocrat, 
                        TopRepublican = TopRepublican, 
                        DemocratCongress = DemocratCongress,
                        RepublicanCongress = RepublicanCongress, 
                        Democrat = TopDemocrat,
                        Democrat = DemocratCongress,
                        Democrat = liberalidentity,
                        Republican = TopRepublican,
                        Republican = RepublicanCongress,
                        Republican = conservativeidentity,
                        Democrat2 = TopDemocrat,
                        Democrat2 = liberalidentity,
                        Republican2 = TopRepublican,
                        Republican2 = conservativeidentity,
                        MoralEmotional = MoralEmotional,
                        Positive = positive_terms,
                        Negative = negative_terms))


#### 3 Text Processing ####

# Tokenize the Republican Congress tweets and look up using the prepared dictionaries
congressRepubcorpus = corpus(congressRepub, text_field = 'text')
toks <- tokens(congressRepubcorpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
congressRepubdict <- dfm_lookup(dfm(toks), group, verbose=TRUE)
congressRepub_dict_df <- quanteda::convert(congressRepubdict, to='data.frame')
congressRepubTwitter = cbind(congressRepub_dict_df, congressRepub)
congressRepubTwitter$doc_id <- NULL

# Tokenize the Democrat Congress tweets and look up using the prepared dictionaries
congressDemcorpus = corpus(congressDem, text_field = 'text')
toks <- tokens(congressDemcorpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
congressDemdict <- dfm_lookup(dfm(toks), group, verbose=TRUE)
congressDem_dict_df <- quanteda::convert(congressDemdict, to='data.frame')
congressDemTwitter = cbind(congressDem_dict_df, congressDem)
congressDemTwitter$doc_id <- NULL

#Log Transform Retweet Count
congressRepubTwitter$retweet_log <- log(congressRepubTwitter$retweet_count + 1)
congressDemTwitter$retweet_log <- log(congressDemTwitter$retweet_count + 1)

#Log Transform Favorite Count
congressRepubTwitter$favorites_log <- log(congressRepubTwitter$favorite_count + 1)
congressDemTwitter$favorite_log <- log(congressDemTwitter$favorite_count + 1)

# Save datasets at crucial checkpoint
#saveRDS(congressRepubTwitter, "Data/ConservativeCongressTwitter.rds")
#saveRDS(congressDemTwitter, "Data/LiberalCongressTwitter.rds")



#### 4 OLS Regression ####

# Conservative Congress Analysis 
congressRepubTwitterMod <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + followers_count + is_retweet, data=congressRepubTwitter)
congressRepubTwitterSumm <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterSumm

# Liberal Congress Analysis 
congressDemTwitterMod <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + followers_count + is_retweet, data=congressDemTwitter)
congressDemTwitterSumm <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterSumm

#### Regressions Without Control Variables #### 

# Conservative Congress Analysis 
congressRepubTwitterMod <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional, data=congressRepubTwitter)
congressRepubTwitterNC <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterNC

# Liberal Congress Analysis 
congressDemTwitterMod <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional, data=congressDemTwitter)
congressDemTwitterNC <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterNC

# Export Regression Results
export_summs(congressDemTwitterSumm,
             congressRepubTwitterSumm,
             error_format = "[{conf.low}, {conf.high}]",
             model.names = c("Liberal Congress Twitter", "Conservative Congress Twitter"), 
             to.file = "html", 
             file.name = "StudyTwoModels.html")
export_summs(congressDemTwitterNC,
             congressRepubTwitterNC,
             error_format = "[{conf.low}, {conf.high}]",
             model.names = c("Liberal Congress Twitter", "Conservative Congress Twitter"), 
             to.file = "html", 
             file.name = "StudyTwoModelsNoControls.html")


#### 5 Plotting ####
library(dotwhisker)
library(broom)
library(ggpubr)
library(cowplot)

# Remove the control variables before plotting
concongressmodels <- tidy(congressRepubTwitterSumm) %>% filter(term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")
libcongressmodels <- tidy(congressDemTwitterSumm) %>% filter(term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")

# Liberal Congress Plotting
plot <- dwplot(libcongressmodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Ingroup)", Republican = "Conservative (Outgroup)", Negative = "Negative ", Positive = "Positive ", MoralEmotional = "Moral Emotional")) +
  xlim(0.7, 2.9)

two_brackets <- list(c("Identity", "Liberal (Ingroup)", "Conservative (Outgroup)"), 
                     c("Emotion", "Negative ", "Moral Emotional"))

libcongressplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Liberal Congress") +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)
libcongressplot

# Conservative Congress Plotting
plot <- dwplot(concongressmodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Outgroup)", Republican = "Conservative (Ingroup)", Negative = "Negative ", Positive = "Positive ", MoralEmotional = "Moral Emotional")) 

two_brackets <- list(c("Identity", "Liberal (Outgroup)", "Conservative (Ingroup)"), 
                     c("Emotion", "Negative ", "Moral Emotional"))

conservativecongressplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Conservative Congress") +
    xlim(0.7, 2.9) +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(170, 260)) 
)} %>% 
  add_brackets(two_brackets)
conservativecongressplot

# Combine the 2 plots in one viewing
combinedcongressplots <- plot_grid(libcongressplot, 
                                   conservativecongressplot, 
                                   nrow = 3,
                                   labels = c("A", "B"),
                                   rel_heights = c(1, 1, .3))

combinedcongressplots 

ggsave("Output/combinedcongressplots.png")


#### 6 Retweet vs. Favorite ####

# Conservative Congress
# Models
retweet <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + is_retweet + followers_count, data = congressRepubTwitter)
favorite <- glm(favorites_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + is_retweet + followers_count, data = congressRepubTwitter)
retweetsumm <- summ(retweet, exp = TRUE, center = TRUE, vifs = TRUE)
favoritesumm <- summ(favorite, exp = TRUE, center = TRUE, vifs = TRUE)
retweets <- tidy(retweetsumm)  %>% filter(term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "Negative", term != "MoralEmotional", term != "Positive") %>% mutate(model = "Retweet")
favorites <- tidy(favoritesumm)  %>% filter(term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "Negative", term != "MoralEmotional", term != "Positive") %>% mutate(model = "Favorite")
concongressmodels <- rbind(retweets, favorites)
concongressmodels$termtemp <- concongressmodels$term
concongressmodels$modeltemp <- concongressmodels$model
concongressmodels$term <- concongressmodels$modeltemp
concongressmodels$model <- concongressmodels$termtemp
concongressmodels[c(1, 4), 1] <- "(Intercept)"

concongressplot <- {dwplot(concongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(0, 260), labels = c("Conservative Words", "Liberal Words")) +
    theme_apa(legend.pos = "bottom",
              legend.font.size = 12) +
    xlim(0.7, 3.0) +
    ggtitle("Conservative Congress")}
concongressplot

ggsave("Output/conservativecongressreactionplot.png")

# Liberal Congress
#Models
retweet <- glm(retweet_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + is_retweet + followers_count, data = congressDemTwitter)
favorite <- glm(favorite_log ~ Democrat + Republican + Negative + Positive + MoralEmotional + has_URL + is_retweet + followers_count, data = congressDemTwitter)

retweetsumm <- summ(retweet, exp = TRUE, center = TRUE, vifs = TRUE)
favoritesumm <- summ(favorite, exp = TRUE, center = TRUE, vifs = TRUE)

retweet <- tidy(retweetsumm)  %>% filter(term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "Negative", term != "MoralEmotional", term != "Positive") %>% mutate(model = "Retweet")
favorite <- tidy(favoritesumm)  %>% filter(term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "Negative", term != "MoralEmotional", term != "Positive") %>% mutate(model = "Favorite")

libcongressmodels <- rbind(retweet, favorite)
libcongressmodels$termtemp <- libcongressmodels$term
libcongressmodels$modeltemp <- libcongressmodels$model
libcongressmodels$term <- libcongressmodels$modeltemp
libcongressmodels$model <- libcongressmodels$termtemp
libcongressmodels[c(1, 4), 1] <- "(Intercept)"

libcongressplot <- {dwplot(libcongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(0, 260), labels = c("Conservative Words", "Liberal Words")) +
    theme_apa(legend.pos = "bottom",
              legend.font.size = 12) +
    xlim(0.7, 3.0) +
    ggtitle("Liberal Congress")}
libcongressplot

ggsave("Output/liberalcongressreactionplot.png", width = 6, height = 4)

# Combined plot
combinedcongressplots <- plot_grid(libcongressplot, 
                                     concongressplot, 
                                     nrow = 3,
                                     labels = c("A", "B"),
                                     rel_heights = c(1, 1, .3))
combinedcongressplots 

ggsave("Output/combinedcongressreactionplot.png")

#### 7 Meta Analysis Alternate Dictionaries ####

library("meta")
library(forestplot)
library(metafor)
library(metaviz)

# Outgroup Variable Construction
congressRepubTwitter$outgroup <- ifelse((congressRepubTwitter$Democrat > 0 & congressRepubTwitter$Republican < 1), 1, NA)
congressRepubTwitter$outgroup <- ifelse((congressRepubTwitter$Republican > 0 & congressRepubTwitter$Democrat < 1), 0, congressRepubTwitter$outgroup)
congressRepubTwitter$outgroupidentity <- ifelse((congressRepubTwitter$liberalidentity > 0 & congressRepubTwitter$conservativeidentity < 1), 1, NA)
congressRepubTwitter$outgroupidentity <- ifelse((congressRepubTwitter$conservativeidentity > 0 & congressRepubTwitter$liberalidentity < 1), 0, congressRepubTwitter$outgroupidentity)
congressRepubTwitter$outgrouptop <- ifelse((congressRepubTwitter$TopDemocrat > 0 & congressRepubTwitter$TopRepublican < 1), 1, NA)
congressRepubTwitter$outgrouptop <- ifelse((congressRepubTwitter$TopRepublican > 0 & congressRepubTwitter$TopDemocrat < 1), 0, congressRepubTwitter$outgrouptop)
congressRepubTwitter$outgroupcongress <- ifelse((congressRepubTwitter$DemocratCongress > 0 & congressRepubTwitter$RepublicanCongress < 1), 1, NA)
congressRepubTwitter$outgroupcongress <- ifelse((congressRepubTwitter$RepublicanCongress > 0 & congressRepubTwitter$DemocratCongress < 1), 0, congressRepubTwitter$outgroupcongress)

congressDemTwitter$outgroup <- ifelse((congressDemTwitter$Republican > 0 & congressDemTwitter$Democrat < 1), 1, NA)
congressDemTwitter$outgroup <- ifelse((congressDemTwitter$Democrat > 0 & congressDemTwitter$Republican < 1), 0, congressDemTwitter$outgroup)
congressDemTwitter$outgroupidentity <- ifelse((congressDemTwitter$conservativeidentity > 0 & congressDemTwitter$liberalidentity < 1), 1, NA)
congressDemTwitter$outgroupidentity <- ifelse((congressDemTwitter$liberalidentity > 0 & congressDemTwitter$conservativeidentity < 1), 0, congressDemTwitter$outgroupidentity)
congressDemTwitter$outgrouptop <- ifelse((congressDemTwitter$TopRepublican > 0 & congressDemTwitter$TopDemocrat < 1), 1, NA)
congressDemTwitter$outgrouptop <- ifelse((congressDemTwitter$TopDemocrat > 0 & congressDemTwitter$TopRepublican < 1), 0, congressDemTwitter$outgrouptop)
congressDemTwitter$outgroupcongress <- ifelse((congressDemTwitter$RepublicanCongress > 0 & congressDemTwitter$DemocratCongress < 1), 1, NA)
congressDemTwitter$outgroupcongress <- ifelse((congressDemTwitter$DemocratCongress > 0 & congressDemTwitter$RepublicanCongress < 1), 0, congressDemTwitter$outgroupcongress)

# Regression Construction
conCongressTwitterOutgroup <- glm(retweet_log ~ outgroup + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupTop <- glm(retweet_log ~ outgrouptop + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupIdentity <- glm(retweet_log ~ outgroupidentity + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupCongress <- glm(retweet_log ~ outgroupcongress + has_URL + followers_count + is_retweet, data = congressRepubTwitter)

libCongressTwitterOutgroup <- glm(retweet_log ~ outgroup + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupTop <- glm(retweet_log ~ outgrouptop + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupIdentity <- glm(retweet_log ~ outgroupidentity + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupCongress <- glm(retweet_log ~ outgroupcongress + has_URL + followers_count + is_retweet, data = congressDemTwitter)

# Tidy Regressions
concongressT <- tidy(summ(conCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
concongressT2 <- tidy(summ(conCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
concongressT3 <- tidy(summ(conCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
concongressT4 <- tidy(summ(conCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libcongressT <- tidy(summ(libCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
libcongressT2 <- tidy(summ(libCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libcongressT3 <- tidy(summ(libCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libcongressT4 <- tidy(summ(libCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")


metadatasetreorder <- rbind(libcongressT, libcongressT2, libcongressT3, libcongressT4,
                            concongressT, concongressT2, concongressT3, concongressT4)

metadatasetdf <- as.data.frame(metadatasetreorder)
metadatasetdf$dataset2 <- NULL
metadatasetdf$dataset2 <- ifelse(metadatasetdf$label == "Outgroup" | metadatasetdf$label == "Outgroup (Top 100)", metadatasetdf$dataset2 <- metadatasetdf$dataset, metadatasetdf$dataset2 <- " ")

summary_table <- metadatasetdf[1:8, c("label", "dataset2")]
metadatasetdf$main <- ifelse(metadatasetdf$label == "Outgroup", metadatasetdf$main <- TRUE, metadatasetdf$main <- FALSE)
metadatasetdf$mainreverse <- metadatasetdf$main == FALSE
metadatasetdf$color <- ifelse(metadatasetdf$party == "Conservative", metadatasetdf$color <- "firebrick", metadatasetdf$color <- "steelblue4")

summary_labels <- data.frame(
  Dataset = c("All", "All"),
  Variable = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"))
head(summary_labels)

metaplot <- viz_forest(x = metadatasetdf[1:8, c("estimate", "std.error")], 
                       study_labels = metadatasetdf[1:8, c("dataset")],
                       method = "DL",
                       annotate_CI = T,
                       study_table = summary_table[1:8, 2:1],
                       summary_table = summary_labels,
                       group = metadatasetdf[1:8, "mainreverse"],
                       x_trans_function = exp, 
                       xlab = "Odds Ratio",
                       summary_label = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"), 
                       col = as.vector(metadatasetdf$color), 
                       summary_col = c("slateblue4"),
                       table_headers = c("Dataset", "Variable"),
                       text_size = 3.8)


metaplot
ggsave("Output/Meta_Alternate_Dictionaries.png")



#### 8 Meta Analysis of Main Effects ####
ConCongressTwitter <- tidy(summ(congressRepubTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter")
LibCongressTwitter <- tidy(summ(congressDemTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter")

meta2 <- rbind(ConCongressTwitter, LibCongressTwitter) %>%
  arrange(party, term)
meta2$liberal <- ifelse(meta2$party == "Liberal", 1, 0)
meta2$twitter <- ifelse(meta2$platform == "Twitter", 1, 0)
meta2$congress <- ifelse(meta2$type == "Congress", 1, 0)

meta2[2, 1] <- "outgroup"
meta2[6, 1] <- "ingroup"
meta2[8, 1] <- "ingroup"
meta2[12, 1] <- "outgroup"

meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "outgroup"))
metaoutgroup <- tidy()
metaoutgroup <- tibble(name = "Outgroup", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "ingroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Ingroup", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "Negative"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Negative ", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "Positive"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Positive ", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "MoralEmotional"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Moral Emotional", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

metaoutgroup$term <- metaoutgroup$name
plot <- dwplot(metaoutgroup, conf.level = .95,  
               dot_args = list(size = 1.2), whisker_args = list(size = 1)) 

metaplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Mean Effect Sizes") +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    xlim(0.7, 4.0) +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)}
metaplot

ggsave("Output/Meta_Mean_Effect_Sizes.png")


metaR3 <- libcongressmodels %>% mutate(dataset = "Liberal Congress", party = "Liberal", type = "Congress")
metaR4 <- concongressmodels %>% mutate(dataset = "Conservative Congress", party = "Conservative", type = "Congress")

metaR <- rbind(metaR3, metaR4)

metaR <- metaR %>% 
  arrange(model, party)

metaR <- metaR[5:12, ]

metaR$coef <- ""
metaR$coef[1:3] <- "Outgroup"
metaR$coef[3:7] <- "Ingroup"
metaR$coef[7:8] <- "Outgroup"

metaR$term[c(1, 3, 5, 7)] <- "Retweets"
metaR$term[c(2, 4, 6, 8)] <- "Favorites"

##Outgroup
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Retweets" & metaR$coef == "Outgroup"))
metaoutgroup <- tibble(name = "Retweets", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Favorites" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Favorites", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

##Ingroup
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Retweets" & metaR$coef == "Ingroup"))
metaingroup <- tibble(name = "Retweets", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Favorites" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Favorites", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

metaoutgroup <- metaoutgroup %>% mutate(model = "Outgroup")
metaingroup <- metaingroup %>% mutate(model = "Ingroup")
metaReaction <- rbind(metaoutgroup, metaingroup)
metaReaction$term <- metaReaction$name

metaR$liberal <- ifelse(metaR$party == "Liberal", 1, 0)
metaR$congress <- ifelse(metaR$type == "Congress", 1, 0)

####

metaPlot <- {dwplot(metaReaction, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "none") + 
    xlab("Change in odds of engagement") +
    scale_colour_hue(h = c(0, 260)) + 
    xlim(0.7, 4.0) +
    ggtitle("Mean Effect Sizes (Reactions)")}

leg2 <- get_legend(dwplot(metaReaction, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) + 
                     geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
                     theme_apa(legend.pos = "right",
                               legend.font.size = 12) + xlab("Odds ratio") +
                     scale_colour_hue(h = c(260, 0), labels = c("Outgroup", "Ingroup")))

metagrid <- plot_grid(metaplot, 
                      metaPlot,
                      leg2,
                      ncol = 3, 
                      labels = c("A", "B", " "),
                      rel_widths = c(1, 1, 0.25))
metagrid

write.table(metaReaction, file = "olstab.txt", sep = ",", quote = FALSE, row.names = F)

ggsave("Output/Meta_Means_Effect_Sizes_wFavorite.png")


#### 9 Most Popular Pages ####

gettoppagesTwitter <- function(dataset) {
  rankpages <- dataset %>%
    dplyr::select(screen_name, text, retweet_count, favorite_count) %>% 
    arrange(desc(retweet_count)) 
  
  toppages <- rankpages %>%
    group_by(screen_name) %>%
    dplyr::summarize(n = n(),
                     retweets = mean(retweet_count), # using geometric mean
                     favorites = mean(favorite_count)) %>%
    filter(n > 10) %>%
    arrange(desc(retweets))
}

makeplot <- function(dataset, color) {
  dataset %>%
    head(10) %>%
    plyr::mutate(screen_name = fct_reorder(screen_name, retweets)) %>%
    ggplot(aes(screen_name, retweets)) +
    coord_flip() +
    geom_col(show.legend = TRUE, fill = color) + 
    theme_apa() 
}

makeplot(gettoppagesTwitter(congressRepubTwitter), "firebrick")
ggsave("Output/topRepAcct.png")

makeplot(gettoppagesTwitter(congressDemTwitter), "steelblue4")
ggsave("Output/topDemAcct.png")

bigplot <- plot_grid(makeplot(gettoppagesTwitter(congressRepubTwitter), "firebrick"),
                     makeplot(gettoppagesTwitter(congressDemTwitter), "steelblue4"),
                     #
                     ncol = 1,
                     nrow = 2,
                     labels = c("R", "D"),
                     rel_heights = c(1, 1))
ggsave("Output/bigdescriptiveplot.png")

############################################