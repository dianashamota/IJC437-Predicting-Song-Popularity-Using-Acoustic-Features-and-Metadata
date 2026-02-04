#------------------------------
# Final Report IJC437
# Music popularity prediction using Logistic Regression
# Dataset: MusicOSet
# -----------------------------

# Required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(viridisLite)
library(pROC)
library(caret)
library(DescTools)
library(broom)

#1) Loading data 
data_song_features <- read_table("C:/Users/diana/Documents/acoustic_features.csv")
data_song_pop <- read_table("C:/Users/diana/Documents/song_pop.csv")

#songs.csv is tab separated
data_songs_metadata <- read_delim( #file is tab separated
  "C:/Users/diana/Documents/songs.csv",
  delim = "\t"
  )

#2) Data cleaning and Preparation
#Convert song_id to character to guarantee correct and predictable joins
data_song_features$song_id <- as.character(data_song_features$song_id)
data_song_pop$song_id <- as.character(data_song_pop$song_id)
data_songs_metadata$song_id <- as.character(data_songs_metadata$song_id)

#Checking for duplicated song_id values
any(duplicated(data_song_features$song_id))
any(duplicated(data_song_pop$song_id)) # TRUE -> fix before joining
any(duplicated(data_songs_metadata$song_id))

#Because popularity was recorded multiple times per song, values were aggregated
#to the song level by retaining the maximum observed popularity for each song prior to merging datasets.
data_song_pop_clean <- data_song_pop %>%
  group_by(song_id) %>%
  summarise(
    year_end_score = max (year_end_score, na.rm = TRUE),
    is_pop = max(is_pop, na.rm = TRUE),
    year = max(year, na.rm = TRUE),
    .groups = "drop"
  )

any(duplicated(data_song_pop_clean$song_id)) #FALSE

#3) Joining datasets using song_id
combined_data <- data_song_features %>%
  left_join(data_song_pop_clean, by = "song_id") %>%
  left_join(data_songs_metadata, by = "song_id")

#4)Filter: only keep songs from 1986 onwards and with non-missing popularity info
combined_data_1985 <- combined_data %>%
  filter(!is.na(year), year > 1985)

#5)Data check for correct joining
dim(combined_data_1985)
colSums(is.na(combined_data_1985))

#6)Selecting variables for analysis
combined_data_1985 <- combined_data_1985 %>%
  select(-key, -time_signature) #droping unused variables

#7)coding is_pop as 0/1 factor for classificatio 
combined_data_1985$is_pop <- factor(combined_data_1985$is_pop, levels = c(0, 1), labels = c("Not Popular", "Popular"))

# -----------------------------
# Exploratory Data Analysis (EDA). 
# -----------------------------

# -----------------------------
# Distributions of acoustic features to analyse skewness
# -----------------------------

#Duration
ggplot(combined_data_1985, aes(x = duration_ms)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Song Duration (ms)",
    x = "Duration",
    y = "Count"
  )

#Acousticness
ggplot(combined_data_1985, aes(x = acousticness)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Acousticness",
    x = "Acousticness",
    y = "Count"
  )

#Danceability
ggplot(combined_data_1985, aes(x = danceability)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Danceability",
    x = "Danceability",
    y = "Count"
  )

#Energy
ggplot(combined_data_1985, aes(x = energy)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Energy", 
    x = "Energy", 
    y = "Count"
  )

#Loudness
ggplot(combined_data_1985, aes(x = loudness)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Loudness", 
    x = "Loudness", 
    y = "Count"
  )

#Tempo
ggplot(combined_data_1985, aes(x = tempo)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Tempo", 
    x = "Tempo", 
    y = "Count"
  )

#Instrumentalness
ggplot(combined_data_1985, aes(x = instrumentalness)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Instrumentalness", 
    x = "Instrumentalness", 
    y = "Count"
  )

#Liveness
ggplot(combined_data_1985, aes(x = liveness)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Liveness", 
    x = "Liveness", 
    y = "Count"
  )

#Speechiness
ggplot(combined_data_1985, aes(x = speechiness)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Speechiness", 
    x = "Speechiness", 
    y = "Count"
  )

#Valence
ggplot(combined_data_1985, aes(x = valence)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Valence", 
    x = "Valence", 
    y = "Count"
  )

# -----------------------------
# Correlation matrix of acoustic features
# -----------------------------

#Correlation Matrix - are some acoustic features too strongly related to keep both?
acoustic_vars <- combined_data_1985 %>%
  select(
    duration_ms,
    acousticness,
    danceability,
    energy,
    instrumentalness,
    liveness,
    loudness,
    speechiness,
    valence,
    tempo
  )

cor_mat <- cor(acoustic_vars)

# Visualize correlation matrix
col_div <- viridisLite::viridis(200)
corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  diag = FALSE,
  col = col_div,
  tl.col = "black",
  tl.cex = 0.9,
  tl.srt = 45,           # rotate labels for readability
  addCoef.col = "black",
  number.cex = 0.6,
  number.digits = 2,
  addgrid.col = "grey80",
  mar = c(0, 0, 1, 0)
)

# -----------------------------
# Boxplots of acoustic variables
# -----------------------------

#Duration
ggplot(combined_data_1985, aes(x = is_pop, y = duration_ms, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Duration by Song Popularity",
    x = "Popularity",
    y = "Duration"
  ) +
  theme(legend.position = "none")

#Acousticness
ggplot(combined_data_1985, aes(x = is_pop, y = acousticness, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Acousticness by Song Popularity",
    x = "Popularity",
    y = "Acousticness"
  ) +
  theme(legend.position = "none")

#Danceability
ggplot(combined_data_1985, aes(x = is_pop, y = danceability, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Danceability by Song Popularity",
    x = "Popularity",
    y = "Danceability"
  ) +
  theme(legend.position = "none") #x-axis already tells us which is and isnt popular - legend would be useless

#Energy
ggplot(combined_data_1985, aes(x = is_pop, y = energy, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Energy by Song Popularity",
    x = "Popularity",
    y = "Energy"
  ) +
  theme(legend.position = "none")

#Instrumentalness
ggplot(combined_data_1985, aes(x = is_pop, y = instrumentalness, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Instrumentalness by Song Popularity",
    x = "Popularity",
    y = "Instrumentalness"
  ) +
  theme(legend.position = "none")

#Liveness
ggplot(combined_data_1985, aes(x = is_pop, y = liveness, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Liveness by Song Popularity",
    x = "Popularity",
    y = "Liveness"
  ) +
  theme(legend.position = "none")

#Loudness
ggplot(combined_data_1985, aes(x = is_pop, y = loudness, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Loudness by Song Popularity",
    x = "Popularity",
    y = "Loudness"
  ) +
  theme(legend.position = "none")

#Speechiness
ggplot(combined_data_1985, aes(x = is_pop, y = speechiness, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Speechiness by Song Popularity",
    x = "Popularity",
    y = "Speechiness"
  ) +
  theme(legend.position = "none")

#Valence
ggplot(combined_data_1985, aes(x = is_pop, y = valence, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Valence by Song Popularity",
    x = "Popularity",
    y = "Valence"
  ) +
  theme(legend.position = "none")

#Tempo
ggplot(combined_data_1985, aes(x = is_pop, y = tempo, fill = is_pop)) +
  geom_boxplot() +
  labs(
    title = "Tempo by Song Popularity",
    x = "Popularity",
    y = "Tempo"
  ) +
  theme(legend.position = "none")

# -----------------------------
# EDA: Categorical Metadata
# -----------------------------

#Recoding metadata variables into factors
combined_data_1985$song_type <- factor(combined_data_1985$song_type,
                                       levels = c("Solo", "Collaboration"))
combined_data_1985$explicit <- factor(
  combined_data_1985$explicit,
  levels = c(FALSE, TRUE),
  labels = c("Non-Explicit", "Explicit")
)

#Distribution plots
ggplot(combined_data_1985, aes(x = song_type)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Collaboration vs Solo songs",
    x = "Song Type",
    y = "Number of Songs"
  )


ggplot(combined_data_1985, aes(x = explicit)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Explicit and Non-explicit songs",
    x = "Song type",
    y = "Number of Songs"
  )

#Popularity by metadata (stacked bar plots) 
ggplot(combined_data_1985,
       aes(x = song_type, fill = is_pop)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Popularity by Collaboration and Solo songs",
    x = "Song Type",
    y = "Proportion of Songs",
    fill = "Popularity"
  )

ggplot(combined_data_1985,
       aes(x = explicit, fill = is_pop)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Popularity by Explicit and Non-Explicit songs",
    x = "Song type",
    y = "Proportion of Songs",
    fill = "Popularity"
  )

# Chi-squared tests
chisq_explicit <- table(combined_data_1985$explicit, combined_data_1985$is_pop)
chisq.test(chisq_explicit)

chisq_songtype <- table(combined_data_1985$song_type, combined_data_1985$is_pop)
chisq.test(chisq_songtype)


# -----------------------------
# Logistic Regression Models
# -----------------------------

# Create training and test sets (70/30 split)

set.seed(123)
n <- nrow(combined_data_1985)
train_idx <- sample(seq_len(n), size = 0.7 * n)
train_data <- combined_data_1985[train_idx, ]
test_data  <- combined_data_1985[-train_idx, ]

# -----------------------------
# Model 1: Acoustic Features Only
# -----------------------------
model_rq2 <- glm(
  is_pop ~ duration_ms + acousticness + danceability + energy +
    instrumentalness + liveness + loudness + speechiness + valence + tempo,
  data = train_data,
  family = binomial
)
summary(model_rq2)

# Odds ratios
odds_ratios <- exp(coef(model_rq2))
odds_ratios

# Confidence intervals 
exp(confint(model_rq2))

# Regression output
reg_table <- tidy(model_rq2, exponentiate = TRUE, conf.int = TRUE)
reg_table

# Pseudo R² - Nagelkerke
PseudoR2(model_rq2, which = "Nagelkerke")

# Predictions on test data
test_data$pred_prob <- predict(model_rq2, newdata = test_data, type = "response")

# Convert probabilities -> predicted class (0.5 threshold)
test_data$pred_class <- ifelse(test_data$pred_prob > 0.5, "Popular", "Not Popular")
test_data$pred_class <- factor(test_data$pred_class, levels = levels(test_data$is_pop))

# Confusion matrix + ROC
conf_mat <- table(
  Actual = test_data$is_pop,
  Predicted = test_data$pred_class
)
conf_mat
conf_df <- as.data.frame(conf_mat)

ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title = "Confusion Matrix: Logistic Regression Model 1\n (Acoustic features only)",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


confusionMatrix(test_data$pred_class, test_data$is_pop, positive = "Popular")

roc_obj <- roc(test_data$is_pop, test_data$pred_prob)
auc(roc_obj)

# -----------------------------
# Model 2: Metadata Only
# -----------------------------

# Fit model on TRAIN data
model_metadata <- glm(is_pop ~ song_type + explicit,
  data = train_data,family = binomial)
summary(model_metadata)

# Odds Ratios
odds_ratios_metadata <- exp(coef(model_metadata))
odds_ratios_metadata

# Confidence intervals 
exp(confint(model_metadata))

# Regression output
reg_table_metadata <- tidy(model_metadata, exponentiate = TRUE, conf.int = TRUE)
reg_table_metadata

# Pseudo R² - Nagelkerke
PseudoR2(model_metadata, which = "Nagelkerke")

# Predict on test data
test_data$pred_prob_metadata <- predict(model_metadata, newdata = test_data, type = "response")

# Convert probabilities -> predicted class (0.5 threshold)
test_data$pred_class_metadata <- ifelse(test_data$pred_prob_metadata > 0.5, "Popular", "Not Popular")
test_data$pred_class_metadata <- factor(test_data$pred_class_metadata, levels = levels(test_data$is_pop))

# Confusion Matrix and ROC
conf_mat_metadata <- table(
  Actual = test_data$is_pop,
  Predicted = test_data$pred_class_metadata
)
conf_mat_metadata
conf_df_metadata <- as.data.frame(conf_mat_metadata)

# Confusion Matrix plot
ggplot(conf_df_metadata, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title = "Confusion Matrix: Logistic Regression Model 2\n (Metadata only)",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Confusion matrix stats
confusionMatrix(test_data$pred_class_metadata, test_data$is_pop, positive = "Popular")

# ROC Curve & AUC
roc_metadata <- roc(test_data$is_pop, test_data$pred_prob_metadata)
auc(roc_metadata)

# -----------------------------
# Model 3: Combined Acoustic + Metadata
# -----------------------------
model_combined <- glm(
  is_pop ~ duration_ms + acousticness + danceability + energy +
    instrumentalness + liveness + loudness + speechiness + valence + tempo +
    song_type + explicit,
  data = train_data, family = binomial)

# Summary
summary(model_combined)

# Odds Ratios
exp(coef(model_combined))
exp(confint(model_combined))

# Model results
reg_table_combined <- tidy(model_combined, exponentiate = TRUE, conf.int = TRUE)
reg_table_combined

# Pseudo R²
PseudoR2(model_combined, which = "Nagelkerke")

# Predict on test set
test_data$pred_prob_combined <- predict(model_combined, newdata = test_data, type = "response")

# Predicted class
test_data$pred_class_combined <- ifelse(test_data$pred_prob_combined > 0.5, "Popular", "Not Popular")
test_data$pred_class_combined <- factor(test_data$pred_class_combined, levels = levels(test_data$is_pop))

# Confusion matrix
conf_mat_combined <- table(
  Actual = test_data$is_pop,
  Predicted = test_data$pred_class_combined
)
conf_mat_combined
conf_df_combined <- as.data.frame(conf_mat_combined)

ggplot(conf_df_combined, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title = "Confusion Matrix: Logistic Regression Model 3\n (Combined Acoustics and Metadata)",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

confusionMatrix(test_data$pred_class_combined, test_data$is_pop, positive = "Popular")

# ROC and AUC
roc_combined <- roc(test_data$is_pop, test_data$pred_prob_combined)
auc(roc_combined)

# -----------------------------
# Final Model Comparison Table
# -----------------------------

model_comparison <- data.frame(
  Model = c("Acoustic Only", "Metadata Only", "Combined"),
  Pseudo_R2 = c(
    PseudoR2(model_rq2, which = "Nagelkerke"),
    PseudoR2(model_metadata, which = "Nagelkerke"),
    PseudoR2(model_combined, which = "Nagelkerke")
  ),
  AUC = c(
    auc(roc_obj),
    auc(roc_metadata),
    auc(roc_combined)
  )
)

model_comparison