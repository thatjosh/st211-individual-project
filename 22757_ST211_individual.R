###############################################################################
# 0. File Setup
###############################################################################

# Setup
rm(list = ls())
setwd('C:/Users/joshu/OneDrive/Desktop/ST211/Individual')

# Library imports
library(arm)
library(car)
library(dplyr)
library(ggplot2)
library(gridExtra)

raw_data <- read.csv('individual_data.csv', header=TRUE, stringsAsFactors=TRUE)
summary(raw_data)

###############################################################################
# 1. Check Sumamry 
###############################################################################

# High degree of missing data (>10%):
# rsect, rsuper, persinc2, chattnd2, work, knowtg, glborn, polpart2, knowgl
high_missing_preds <- c("rsect", "rsuper", "persinc2", "chattnd2", "work",
                        "knowtg", "glborn", "polpart2", "knowgl")

# Moderate degree (4-10%):
# knowgl
mid_missing_preds <- c("knowgl", "hincpast")
                       
# Low level (<4%):
# hincpast, famrelig, religcat, uprejgay, glchild, tunionsa, tea, ruhappy,
# healthyr, tenshort, orient, eqnow3, eqnow7, eqnow8, eqnow9, eqnow11, anyhcond,
# rage, rmarstat, ansseca, umineth

low_missing_preds <- c("famrelig", "religcat", "uprejgay","glchild", "tunionsa",
                       "tea", "ruhappy", "healthyr", "tenshort", "orient",
                       "eqnow3", "eqnow7", "eqnow8", "eqnow9", "eqnow11",
                       "anyhcond", "rage", "rmarstat", "ansseca", "umineth")

###############################################################################
# 2. EDA
###############################################################################

# Deal with low missing values (<4%)
clean.support.data <- subset(raw_data, 
                            famrelig != 'NA' & 
                            religcat != 'NA' & 
                            uprejgay != 'NA' & 
                            glchild != 'NA' & 
                            tunionsa != 'NA' & 
                            tea != 'NA' & 
                            ruhappy != 'NA' & 
                            healthyr != 'NA' & 
                            tenshort != 'NA' & 
                            orient != 'NA' & 
                            eqnow3 != 'NA' & 
                            eqnow7 != 'NA' & 
                            eqnow8 != 'NA' & 
                            eqnow9 != 'NA' & 
                            eqnow11 != 'NA' & 
                            anyhcond != 'NA' & 
                            rage != 'NA' & 
                            rmarstat != 'NA' & 
                            ansseca != 'NA' & 
                            umineth != 'NA')

percentage_intact = nrow(clean.support.data)/nrow(raw_data)
percentage_intact
# Dropped ~10% of data
# 90.4% remaining


# Investigate dependencies
summary(clean.support.data)
corr_matrix <- round(cor(clean.support.data), 1)
corr_matrix

# Predictor classification
categorical_pred <- c("glsocdist", "glvis", "rsect", "chattnd2", "work",
                      "polpart2", "hincpast", "famrelig", "religcat", "uprejgay",
                      "glchild", "tea", "ruhappy", "healthyr", "tenshort",
                      "orient", "rmarstat", "ansseca","highqual", "househld")
# without outcome
binary_pred <- c("rsuper", "knowtg", "glborn", "knowgl", "tunionsa",
                 "eqnow3", "eqnow7", "eqnow8", "eqnow9", "eqnow11",
                 "anyhcond", "umineth", "rsex", "intwww", "carehome")
# with outcome
binary_var <- c(binary_pred, "ssexmarr")
continuous_pred <- c("persinc2", "rage", "livearea")
total_variables <- (
  length(categorical_pred) + length(binary_var) + length(continuous_pred)
)
total_variables  # Make sure total is 39

table(raw_data$ssexmarr)
# Re-encoded binary variables
reencode_binary <- function(data, columns) {
  for (col in columns) {
    data[[col]] <- ifelse(data[[col]] == 1, 0, ifelse(data[[col]] == 2, 1, NA))
  }
  return(data)
}
columns_to_recode <- binary_var
clean.support.data <- reencode_binary(clean.support.data, columns_to_recode)
summary(clean.support.data)

# Convert categorical predictors to factors
clean.support.data[categorical_pred] <- lapply(
  clean.support.data[categorical_pred], function(x) as.factor(x)
)
summary(clean.support.data)

# Bias Check
table(clean.support.data$rsex)
# Perform z-test
prop.test(546, 546+416, p = 0.51, correct = FALSE)

# Reset baseline for categorical pred to level with most observation
relevel_predictors_to_most_frequent <- function(data, categorical_cols) {
  for (col_name in categorical_cols) {
    if (is.factor(data[[col_name]])) {
      level_frequencies <- table(data[[col_name]])
      most_frequent_level <- names(which.max(level_frequencies))
      data[[col_name]] <- relevel(data[[col_name]], ref = most_frequent_level)
    }
  }
  return(data)
}
clean.support.data <- relevel_predictors_to_most_frequent(
  clean.support.data, categorical_pred
  )
summary(clean.support.data)

# Copy for merging
merge.support.data <- clean.support.data
complete_case.data <- clean.support.data

# Plots
# _____________________________________________________________________________
# Helper function for continuous plot
continuous_plot <- function(data, x_var) {
  # Convert ssexmarr to numeric
  data$ssexmarr_numeric <- as.numeric(as.factor(data$ssexmarr)) - 1

  # Create the plot
  p <- ggplot(data, aes_string(x = x_var, y = "ssexmarr_numeric")) +
    geom_count() +
    geom_smooth(
      method = "glm",
      method.args = list(family = "binomial"),
      formula = y ~ x,  # Ensuring clarity with explicit formula
      se = FALSE
    ) +
    theme_bw() +
    labs(title = paste("Logistic Regression of ssexmarr over", x_var),
         x = x_var,
         y = "Probability of ssexmarr = 1")
  print(p)
  # Clean up: Remove the temporary numeric variable
  data$ssexmarr_numeric <- NULL
  return (p)
}
continuous_flipped_box_plot <- function(data, x_var) {
  p <- ggplot(data, aes_string(x = factor(x_var), y = "ssexmarr")
              )+ geom_boxplot()+ coord_flip()
  p <- p+theme(legend.position = "none")
  return (p)
}


# _____________________________________________________________________________
# Helper function for cat plot
categorical_plot <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, fill = "factor(ssexmarr)")) +
    geom_bar(position = "fill") +
    scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
}

# _____________________________________________________________________________
# Helper function for turn na to missing
recode_na_to_missing <- function(data, column_name, missing_level = "missing") {
  data[[column_name]] <- ifelse(is.na(data[[column_name]]), missing_level,
  data[[column_name]])
  return(data)
}

# Check bias for sex
# _____________________________________________________________________________

n_males <- 457
n_females <- 607
total_samples <- n_males + n_females
sample_prop_males <- n_males / total_samples
sample_prop_females <- n_females / total_samples
p_males <- 0.49
p_females <- 0.51
bias_males <- sample_prop_males - p_males
bias_females <- sample_prop_females - p_females
bias_males
bias_females
# Male, under: -0.06048872
# Female, over: 0.06048872

# Check bias for sex
# _____________________________________________________________________________


# Male, under: -0.06048872
# Female, over: 0.06048872

# _____________________________________________________________________________
# Plots
# _____________________________________________________________________________

# househld
p1 <- categorical_plot(clean.support.data, "househld")
p1
# Simple GLM
merge.househld <- glm(ssexmarr ~ factor(househld), data = clean.support.data,
                        family = binomial(link = "logit"))
summary(merge.househld)
merge.support.data <- merge.support.data %>%
  mutate(househld = recode(househld,
                            "1" = "1",
                            "2" = "2",
                            "3" = ">2",
                            "4" = ">2",
                            "5" = ">2",
                            "6" = ">2",
                            "7" = ">2",
                            "8" = ">2",
  ))
table(merge.support.data['househld'])

# rage
# Check distribution of Age
age_histogram <- ggplot(clean.support.data, aes(x = rage)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal()
age_histogram
# Plot Logistic Curve and Scatter Plot
support.breaks <- mutate(clean.support.data, bin = cut(
  rage, breaks = seq(15, 95, by = 5)))
prop <- prop.table(with(support.breaks, table(ssexmarr, bin)), 2)[2,]
midbin <- seq(17.5, 92.5, by = 5)
support.bin <- data.frame(midbin, prop)
p2 <- ggplot(support.breaks, aes(x = rage, y = as.numeric(ssexmarr)))
p2 <- p2 + geom_count()
p2 <- p2 + geom_smooth(method = "glm", method.args = list(
  family = "binomial"), se = FALSE)
p2 <- p2 + geom_point(data = support.bin, aes(x = midbin, y = prop),
                      shape = 2, color = "red", size = 3)
p2

# rsex
table(raw_data$rsex)
p3 <- categorical_plot(clean.support.data, "rsex")
p3

# rmarstat
p4 <- categorical_plot(clean.support.data, "rmarstat")
p4
# Simple GLM
merge.rmarstat <- glm(ssexmarr ~ factor(rmarstat), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.rmarstat)
merge.support.data <- merge.support.data %>%
  mutate(rmarstat = recode(rmarstat,
                           "1" = "Single",
                           "2" = "Married",
                           "3" = "Living_as_married",
                           "4" = "Separated_or_divorced",
                           "5" = "Separated_or_divorced",
                           "6" = "Widowed",
  ))
table(merge.support.data['rmarstat'])

# livearea
p5 <- continuous_plot(clean.support.data, "livearea")
p5
p5_fp<- ggplot(clean.support.data,
               aes(x=factor(ssexmarr),
                   y=livearea))+ geom_boxplot()+ coord_flip()
p5_fp

# hincpast
clean.support.data <- recode_na_to_missing(clean.support.data, "hincpast",
                                           "missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "hincpast",
                                           "missing")
p6 <- categorical_plot(clean.support.data, "hincpast")
p6
# Simple GLM
merge.hincpast <- glm(ssexmarr ~ factor(hincpast), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.hincpast)
# Merge
merge.support.data <- merge.support.data %>%
  mutate(hincpast = recode(hincpast,
                           "1" = "Fallen_behind",
                           "2" = "Kept_up",
                           "3" = "Gone_up",
                           "missing" = "Missing",
  ))
table(merge.support.data['hincpast'])

# intwww
p7 <- categorical_plot(clean.support.data, "intwww")
p7

# umineth
p8 <- categorical_plot(clean.support.data, "umineth")
p8

# eqnow3-11
p9 <- categorical_plot(clean.support.data, "eqnow3")
p9
p10 <- categorical_plot(clean.support.data, "eqnow7")
p10
p11 <- categorical_plot(clean.support.data, "eqnow8")
p11
p12 <- categorical_plot(clean.support.data, "eqnow9")
p12
p13 <- categorical_plot(clean.support.data, "eqnow11")
p13

# tenshort
p14 <- categorical_plot(clean.support.data, "tenshort")
p14
merge.tenshort <- glm(ssexmarr ~ factor(tenshort), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.tenshort)
merge.support.data <- merge.support.data %>%
  mutate(tenshort = recode(tenshort,
                           "1" = "Own_house",
                           "2" = "Buying_with_mortgage_inc_part_rent",
                           "3" = "Buying_with_mortgage_inc_part_rent",
                           "4" = "Rent_with_exec_or_asoc",
                           "5" = "Rent_with_exec_or_asoc",
                           "6" = "Rent_from_private",
                           "7" = "Buying_with_mortgage_inc_part_rent",
  ))
table(merge.support.data['tenshort'])

# highqual
p15 <- categorical_plot(clean.support.data, "highqual")
p15
merge.highqual <- glm(ssexmarr ~ factor(highqual), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.highqual)
table(clean.support.data['highqual'])
merge.support.data <- merge.support.data %>%
  mutate(highqual = recode(highqual,
                           "1" = "Alevel_and_higher_ed",
                           "2" = "Alevel_and_higher_ed",
                           "3" = "Alevel_and_higher_ed",
                           "4" = "Good_GCSE_or_other",
                           "5" = "Bad_GCSE_or_no_qual",
                           "6" = "Bad_GCSE_or_no_qual",
                           "7" = "Good_GCSE_or_other",
                           "9" = "Good_GCSE_or_other",
  ))
table(merge.support.data['highqual'])

# tea
p16 <- categorical_plot(clean.support.data, "tea")
p16
table(clean.support.data['tea'])
merge.support.data <- merge.support.data %>%
  mutate(tea = recode(tea,
                           "1" = "15_or_under",
                           "2" = "16_and_over",
                           "3" = "16_and_over",
                           "4" = "16_and_over",
                           "5" = "16_and_over",
                           "6" = "Still_in_school",
                           "7" = "Still_in_uni",
  ))
table(merge.support.data['tea'])

# work
p17 <- categorical_plot(clean.support.data, "work")
p17
table(clean.support.data['work'])
# Recode NA to missing, likely just category issue 
merge.support.data$work <- ifelse(is.na(merge.support.data$work), "Missing",
                                  merge.support.data$work)
merge.support.data <- merge.support.data %>%
  mutate(work = recode(work,
                      "1" = "Employee",
                      "2" = "Foreperson_or_supevisor",
                      "3" = "Self_employed",
                      "4" = "Manager",
                      "Missing" = "Missing",
  ))
table(merge.support.data['work'])

# rsuper
clean.support.data <- recode_na_to_missing(clean.support.data, "rsuper",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "rsuper",
                                           "Missing")
p18 <- categorical_plot(clean.support.data, "rsuper")
p18
merge.support.data <- merge.support.data %>%
  mutate(rsuper = recode(rsuper,
                       "0" = "Yes",
                       "1" = "No",
                       "Missing" = "Missing",
  ))
summary(clean.support.data)

# rsect
clean.support.data <- recode_na_to_missing(clean.support.data, "rsect",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "rsect",
                                           "Missing")
p19 <- categorical_plot(clean.support.data, "rsect")
p19
# Simple GLM
merge.rsect <- glm(ssexmarr ~ factor(rsect), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.rsect)
table(clean.support.data['rsect'])
merge.support.data <- merge.support.data %>%
  mutate(rsect = recode(rsect,
                           "1" = "Public",
                           "2" = "Private",
                           "3" = "Voluntary_or_other",
                           "4" = "Voluntary_or_other",
                           "Missing" = "Missing",
  ))
table(merge.support.data['rsect'])

# tunionsa
p20 <- categorical_plot(clean.support.data, "tunionsa")
p20

# ansseca
p21 <- categorical_plot(clean.support.data, "ansseca")
p21
# Simple GLM
merge.ansseca <- glm(ssexmarr ~ factor(ansseca), data = clean.support.data,
                   family = binomial(link = "logit"))
summary(merge.ansseca)
table(clean.support.data['ansseca'])
merge.support.data <- merge.support.data %>%
  mutate(ansseca = recode(ansseca,
                           "1" = "Higher_prof",
                           "2" = "Higher_prof",
                           "3" = "Higher_prof",
                           "4" = "Intermediate_or_low_prof",
                           "5" = "Self_employed",
                           "6" = "Intermediate_or_low_prof",
                           "7" = "Intermediate_or_low_prof",
                           "8" = "Intermediate_or_low_prof",
                           "9" = "Never_worked_or_long_term_unemployed",
                           "10" = "N/A",
  ))
table(merge.support.data['ansseca'])

# religcat
p22 <- categorical_plot(clean.support.data, "religcat")
p22
  merge.support.data <- merge.support.data %>%
  mutate(religcat = recode(religcat,
                          "1" = "Catholic",
                          "2" = "Protestant",
                          "3" = "No_religion",
  ))
table(merge.support.data['religcat'])

# famrelig
p23 <- categorical_plot(clean.support.data, "famrelig")
p23
merge.support.data <- merge.support.data %>%
  mutate(famrelig = recode(famrelig,
                           "1" = "Catholic",
                           "2" = "Protestant",
                           "3" = "No_religion",
  ))
table(merge.support.data['famrelig'])

# chattnd2
clean.support.data <- recode_na_to_missing(clean.support.data, "chattnd2",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "chattnd2",
                                           "Missing")
p24 <- categorical_plot(clean.support.data, "chattnd2")
p24
merge.chattnd2 <- glm(ssexmarr ~ factor(chattnd2), data = clean.support.data,
                     family = binomial(link = "logit"))
summary(merge.chattnd2)
table(clean.support.data['chattnd2'])
merge.support.data <- merge.support.data %>%
  mutate(chattnd2 = recode(chattnd2,
                          "1" = "Multiple_times_a_month",
                          "2" = "Multiple_times_a_month",
                          "3" = "Multiple_times_a_month",
                          "4" = "Several_times_a_year",
                          "5" = "Several_times_a_year",
                          "6" = "Once_a_year",
                          "7" = "Several_times_a_year",
                          "8" = "Never",
                          "Missing" = "Not_religious",
  ))
table(merge.support.data['chattnd2'])

# carehome
p25 <- categorical_plot(clean.support.data, "carehome")
p25
merge.carehome <- glm(ssexmarr ~ factor(carehome), data = clean.support.data,
                     family = binomial(link = "logit"))
summary(merge.carehome)

# anyhcond
p26 <- categorical_plot(clean.support.data, "anyhcond")
p26

# persinc2
p27 <- continuous_plot(clean.support.data, "persinc2")
p27
  
# orient
p28 <- categorical_plot(clean.support.data, "orient")
p28
merge.orient <- glm(ssexmarr ~ factor(orient), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.orient)
table(clean.support.data['orient'])
merge.support.data <- merge.support.data %>%
  mutate(orient = recode(orient,
                           "1" = "Heterosexual",
                           "2" = "Homosexual",
                           "3" = "Bisexual",
                           "4" = "Other",
  ))
table(merge.support.data['orient'])

# polpart2
clean.support.data <- recode_na_to_missing(clean.support.data, "polpart2",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "polpart2",
                                           "Missing")

p29 <- categorical_plot(clean.support.data, "polpart2")
p29
merge.polpart2 <- glm(ssexmarr ~ factor(polpart2), data = clean.support.data,
                    family = binomial(link = "logit"))
summary(merge.polpart2)
table(clean.support.data['polpart2'])
merge.support.data <- merge.support.data %>%
  mutate(polpart2 = recode(polpart2,
                         "1" = "Right_or_center",
                         "2" = "Left_or_center",
                         "3" = "Right_or_center",
                         "4" = "Left_or_center",
                         "5" = "Center",
                         "6" = "Other_or_missing",
                         "7" = "Other_or_missing",
                         "missing" = "Other_or_missing",
  ))
table(merge.support.data['polpart2'])

# ruhappy
p30 <- categorical_plot(clean.support.data, "ruhappy")
p30
table(merge.support.data['ruhappy'])
merge.support.data <- merge.support.data %>%
  mutate(ruhappy = recode(ruhappy,
                           "1" = "Happy_or_indifferent",
                           "2" = "Happy_or_indifferent",
                           "3" = "Unhappy",
                           "4" = "Unhappy",
                           "8" = "Happy_or_indifferent",
  ))
table(merge.support.data['ruhappy'])


# healthyr
p31 <- categorical_plot(clean.support.data, "healthyr")
p31
table(merge.support.data['healthyr'])
merge.support.data <- merge.support.data %>%
  mutate(healthyr = recode(healthyr,
                          "1" = "Good",
                          "2" = "Good",
                          "3" = "Fair_or_cant_choose",
                          "4" = "Bad",
                          "5" = "Bad",
                          "8" = "Fair_or_cant_choose",
  ))
table(merge.support.data['healthyr'])

# uprejgay
p32 <- categorical_plot(clean.support.data, "uprejgay")
p32
table(merge.support.data['uprejgay'])
merge.support.data <- merge.support.data %>%
  mutate(uprejgay = recode(uprejgay,
                           "1" = "Very",
                           "2" = "A_little",
                           "3" = "Not",
                           "4" = "A_little",
  ))
table(merge.support.data['uprejgay'])


# glchild
p33 <- categorical_plot(clean.support.data, "glchild")
p33
table(merge.support.data['glchild'])
merge.support.data <- merge.support.data %>%
  mutate(glchild = recode(glchild,
                           "1" = "Very_comfortable",
                           "2" = "Fairly_comfortable",
                           "3" = "Indifferent",
                           "4" = "Fairly_uncomfortable",
                           "5" = "Very_uncomfortable",
  ))
table(merge.support.data['glchild'])

# glsocdist
p34 <- categorical_plot(clean.support.data, "glsocdist")
p34
merge.glsocdist <- glm(ssexmarr ~ factor(glsocdist), data = clean.support.data,
                      family = binomial(link = "logit"))
summary(merge.glsocdist)
table(merge.support.data['glsocdist'])
merge.support.data <- merge.support.data %>%
  mutate(glsocdist = recode(glsocdist,
                          "0" = "Less_than_3",
                          "1" = "Less_than_3",
                          "2" = "Less_than_3",
                          "3" = "Less_than_3",
                          "4" = "3_to_5",
                          "5" = "3_to_5",
                          "6" = "More_than_5",
                          "7" = "More_than_5",
                          "8" = "More_than_5",
                          "9" = "More_than_5",
                          "10" = "More_than_5",
                          "11" = "More_than_5",
  ))
table(merge.support.data['glsocdist'])

# glvis
p35 <- categorical_plot(clean.support.data, "glvis")
p35
table(merge.support.data['glvis'])
merge.support.data <- merge.support.data %>%
  mutate(glvis = recode(glvis,
                          "0" = "0_scenarios",
                          "1" = "More_than_1_scenario",
                          "2" = "More_than_1_scenario",
                          "3" = "More_than_1_scenario",
  ))
table(merge.support.data['glvis'])

# glborn
clean.support.data <- recode_na_to_missing(clean.support.data, "glborn",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "glborn",
                                           "Missing")
p36 <- categorical_plot(clean.support.data, "glborn")
p36
table(merge.support.data['glborn'])
merge.support.data <- merge.support.data %>%
  mutate(glborn = recode(glborn,
                        "0" = "Born_that_way",
                        "1" = "choose",
                        "Missing" = "Dont_know",
  ))
table(merge.support.data['glborn'])

# knowgl
clean.support.data <- recode_na_to_missing(clean.support.data, "knowgl",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "knowgl",
                                           "Missing")
p37 <- categorical_plot(clean.support.data, "knowgl")
p37
table(merge.support.data['knowgl'])
merge.support.data <- merge.support.data %>%
  mutate(knowgl = recode(knowgl,
                         "0" = "Know",
                         "1" = "Dont_know",
                         "Missing" = "Not_sure_or_no_answer",
  ))
table(merge.support.data['knowgl'])

# knowtg
clean.support.data <- recode_na_to_missing(clean.support.data, "knowtg",
                                           "Missing")
merge.support.data <- recode_na_to_missing(merge.support.data, "knowtg",
                                           "Missing")
p38 <- categorical_plot(clean.support.data, "knowtg")
p38
table(merge.support.data['knowtg'])
merge.support.data <- merge.support.data %>%
  mutate(knowtg = recode(knowtg,
                         "0" = "Know",
                         "1" = "Dont_know",
                         "missing" = "Not_sure_or_no_answer",
  ))
table(merge.support.data['knowtg'])

# Set Categorical predictors to become factors
summary(merge.support.data)
merge.support.data[categorical_pred] <- lapply(
  merge.support.data[categorical_pred], function(x) as.factor(x)
)
summary(merge.support.data)

# Set binary predictors to become factors
merge.support.data[binary_pred] <- lapply(
  merge.support.data[binary_pred], function(x) as.factor(x)
)
summary(merge.support.data)

# Recode binary preds to yes and no
# Exclude those already recoded
to_recode <- binary_pred[!binary_pred %in% c("glborn", "knowgl", "knowtg",
                                          "rsuper")]
merge.support.data <- merge.support.data %>%
  mutate(across(to_recode, ~ifelse(. == "0", "Yes", "No")))
merge.support.data[binary_pred] <- lapply(
  merge.support.data[binary_pred], function(x) as.factor(x)
)
summary(merge.support.data)

# Reset Baseline to level with most number of observations
relevel_predictors_to_most_frequent <- function(data) {
  for (col_name in names(data)) {
    if (is.factor(data[[col_name]])) {
      level_frequencies <- table(data[[col_name]])
      most_frequent_level <- names(which.max(level_frequencies))
      data[[col_name]] <- relevel(data[[col_name]], ref = most_frequent_level)
    }
  }
  return(data)
}
merge.support.data <- relevel_predictors_to_most_frequent(merge.support.data)
summary(merge.support.data)

# Examine all level factors
factors_list <- sapply(merge.support.data, is.factor)  # Identify factors
factors_data <- merge.support.data[, factors_list]     # Subset only factors
leveling <- lapply(factors_data, levels)
leveling

# Outcome Distribution
table(merge.support.data['ssexmarr'])

###############################################################################
# 3. Full Model & Model 2 (Backwards Elimination)
###############################################################################

# Function to create classification tables
ct.op<-function(predicted,observed){
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the rows we need and stack them on top
  #of each other with rbind
  op.tab<-rbind(op.tab,c(round(prop.table(op.tab,2)[1,1],2),
                         round((prop.table(op.tab,2)[2,2]),2)))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

# Helper function to extract significant predictors
extract_significant_vars <- function(anova_output, n) {
  # Filter rows where Pr(>Chi) is greater than 0.05
  significant_vars <- subset(anova_output, `Pr(>Chi)` > 0.05)
  # Order the results by Pr(>Chi) in descending order (less significant first)
  significant_vars <- significant_vars[order(significant_vars$`Pr(>Chi)`), ]
  # Select the top n variables
  if (nrow(significant_vars) > n) {
    significant_vars <- head(significant_vars, n)
  }
  # Return the names of these variables
  return(rownames(significant_vars))
}

extract_significant_vars_from_summary <- function(model, n = 3) {
  # Extract the summary of the model and its coefficients
  summary_model <- summary(model)
  # Coefficients are stored in a matrix within the model summary
  coef_matrix <- summary_model$coefficients
  # Create a data frame from the coefficients matrix
  coef_df <- as.data.frame(coef_matrix, stringsAsFactors = FALSE)
  # Add the variable names as a new column
  coef_df$Variable <- rownames(coef_matrix)
  # Order the data frame by the 'Pr(>|z|)' column in ascending order
  coef_df <- coef_df[order(coef_df$`Pr(>|z|)`), ]
  # Return the names of the top n variables
  return(coef_df$Variable[1:n])
}

cleaned_data <- na.omit(merge.support.data)

# Null model
null_model <- glm(ssexmarr ~ 1,
                  data = cleaned_data,
                  family = binomial(link = "logit"))

# Run with full model
support.full.glm <- glm(ssexmarr ~ .,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
summary(support.full.glm)
res <-anova(support.full.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
res
top_5
# Remove: "tenshort" "glborn"   "highqual" "orient"   "glvis" 

# Pred for full model
pred.support <- as.numeric(support.full.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)
# 0.91   0.68
anova(null_model, support.full.glm, test = "Chisq")

# Iteration 1
support.be.1.glm <- glm(ssexmarr ~ househld + rage + rsex + rmarstat + livearea +
                          hincpast + intwww + umineth + eqnow3 + eqnow7 + eqnow8 +
                          eqnow9 + eqnow11 + tea + work + rsuper + rsect + tunionsa +
                          ansseca + religcat + famrelig + chattnd2 +carehome +
                          anyhcond + persinc2 + polpart2 + ruhappy + healthyr + 
                          uprejgay + glsocdist + knowgl + knowtg + glchild,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ househld + rage + rsex + rmarstat + livearea +
         hincpast + intwww + umineth + eqnow3 + eqnow7 + eqnow8 +
         eqnow9 + eqnow11 + tea + work + rsuper + rsect + tunionsa +
         ansseca + religcat + famrelig + chattnd2 +carehome +
         anyhcond + persinc2 + polpart2 + ruhappy + healthyr + 
         uprejgay + glsocdist + knowgl + knowtg,
       data = cleaned_data)) # ok
res <-anova(support.be.1.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
# Remove: "polpart2" "rsuper"   "umineth"  "persinc2" "rmarstat"
pred.support <- as.numeric(support.be.1.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)

# Iteration 2
support.be.2.glm <- glm(ssexmarr ~ househld + rage + rsex + livearea + hincpast +
                          intwww + eqnow3 + eqnow7 + eqnow8 + eqnow9 + eqnow11 +  
                          tea + work + rsect + tunionsa + ansseca + religcat +
                          famrelig + chattnd2 + carehome + anyhcond + ruhappy +
                          healthyr + uprejgay + glsocdist + knowgl + knowtg
                          + glchild,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ househld + rage + rsex + livearea + hincpast +
         intwww + eqnow3 + eqnow7 + eqnow8 + eqnow9 + eqnow11 +  
         tea + work + rsect + tunionsa + ansseca + religcat +
         famrelig + chattnd2 + carehome + anyhcond + ruhappy +
         healthyr + uprejgay + glsocdist + knowgl + knowtg,
       data = cleaned_data)) # ok
res <-anova(support.be.2.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
# Remove: "ansseca"  "eqnow9"   "rsect"    "tea"      "hincpast"
pred.support <- as.numeric(support.be.2.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)

# Iteration 3
support.be.3.glm <- glm(ssexmarr ~ househld + rage + rsex + livearea + intwww + 
                          eqnow3 + eqnow7 + eqnow8 + eqnow11 +  
                          work + tunionsa + religcat + famrelig + chattnd2 + 
                          carehome + anyhcond + ruhappy + healthyr + 
                          uprejgay + glsocdist + knowgl + knowtg + glchild,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ househld + rage + rsex + livearea + intwww + 
         eqnow3 + eqnow7 + eqnow8 + eqnow11 +  
         work + tunionsa + religcat + famrelig + chattnd2 + 
         carehome + anyhcond + ruhappy + healthyr + 
         uprejgay + glsocdist + knowgl + knowtg,
       data = cleaned_data)) # ok
res <-anova(support.be.3.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
# Remove: "work"     "carehome" "knowtg"   "healthyr" "eqnow7"
pred.support <- as.numeric(support.be.3.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)

# Iteration 4
support.be.4.glm <- glm(ssexmarr ~ househld + rage + rsex + livearea + intwww + 
                          eqnow3 + eqnow8 + eqnow11 + glchild +
                          tunionsa + religcat + famrelig + chattnd2 + 
                          anyhcond + ruhappy + 
                          uprejgay + glsocdist + knowgl,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ househld + rage + rsex + livearea + intwww + 
         eqnow3 + eqnow8 + eqnow11 +  
         tunionsa + religcat + famrelig + chattnd2 + 
         anyhcond + ruhappy + 
         uprejgay + glsocdist + knowgl,
       data = cleaned_data))
res <-anova(support.be.4.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
# Remove: "knowgl" "tunionsa" "famrelig" "livearea" "chattnd2"
pred.support <- as.numeric(support.be.4.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)

# Iteration 5
support.be.5.glm <- glm(ssexmarr ~ househld + rage + rsex + intwww + 
                          eqnow3 + eqnow8 + eqnow11 + religcat + 
                          anyhcond + ruhappy +uprejgay + glsocdist + glchild,
                        data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ househld + rage + rsex + intwww + 
         eqnow3 + eqnow8 + eqnow11 + religcat + chattnd2 + 
         anyhcond + ruhappy +uprejgay + glsocdist,
       data = cleaned_data))
res <-anova(support.be.5.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
# Done by anova
res
summary(support.be.5.glm)

pred.support <- as.numeric(support.be.5.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)

# Full vs reduced model
anova(support.be.5.glm, support.full.glm, test="Chisq")

# 92% for Yes, 59% for No, around #80.4% overall

###############################################################################
# 4. Standardisation & Centering
###############################################################################

# Standardise Age
rage_mean <- mean(cleaned_data$rage)
rage_sd <- sd(cleaned_data$rage)
cleaned_data$rage_standardized <- (cleaned_data$rage - rage_mean) / rage_sd

# Run regression
support.std.glm <- glm(ssexmarr ~ househld + rage_standardized + rsex + intwww + 
                         eqnow3 + eqnow8 + eqnow11 + religcat + 
                         anyhcond + ruhappy +uprejgay + glsocdist + glchild,
                       data = cleaned_data,
                        family = binomial(link = "logit"))
vif(lm(ssexmarr ~ rage_standardized + rsex + religcat + chattnd2
       + ruhappy + uprejgay + glsocdist,
       data = cleaned_data))
res <-anova(support.std.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)

# Check results
res
summary(support.std.glm)

rage_mean
# Mean is 49 years old

age_above_mean <- rage_mean + rage_sd
age_above_mean
# one SD above is 66 years old

age_below_mean <- rage_mean - rage_sd
age_below_mean
# one SD below is 30 years old

pred.support <- as.numeric(support.std.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)
# Same

# Centering
#______________________________________________________________________________

# Centering Age
rage_mean <- mean(cleaned_data$rage)
cleaned_data$rage_centered <- (cleaned_data$rage - rage_mean) / rage_sd

# Run regression
support.cent.glm <- glm(ssexmarr ~ househld + rage_centered + rsex + intwww + 
                          eqnow3 + eqnow8 + eqnow11 + religcat + 
                          anyhcond + ruhappy +uprejgay + glsocdist + glchild,
                        data = cleaned_data,
                       family = binomial(link = "logit"))
vif(lm(ssexmarr ~ rage_centered + rsex + religcat + chattnd2
       + ruhappy + uprejgay + glsocdist,
       data = cleaned_data))
res <-anova(support.cent.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)

# Check results
res
summary(support.cent.glm)
anova(null_model, support.cent.glm, test = "Chisq")

# Pred
pred.support <- as.numeric(support.cent.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)


rage_mean
# Mean is 49 years old

###############################################################################
# Outlier
###############################################################################

show_outliers<-function(the.linear.model,topN){
  #length of data
  n=length(fitted(the.linear.model))
  #number of parameters estimated
  p=length(coef(the.linear.model))
  #standardised residuals over 3
  res.out<-which(abs(rstandard(the.linear.model))>3) #sometimes >2
  #topN values
  res.top<-head(rev(sort(abs(rstandard(the.linear.model)))),topN)
  #high leverage values
  lev.out<-which(lm.influence(the.linear.model)$hat>2*p/n)
  #topN values
  lev.top<-head(rev(sort(lm.influence(the.linear.model)$hat)),topN)
  #high diffits
  dffits.out<-which(dffits(the.linear.model)>2*sqrt(p/n))
  #topN values
  dffits.top<-head(rev(sort(dffits(the.linear.model))),topN)
  #Cook's over 1
  cooks.out<-which(cooks.distance(the.linear.model)>1)
  #topN cooks
  cooks.top<-head(rev(sort(cooks.distance(the.linear.model))),topN)
  #Create a list with the statistics -- cant do a data frame as different lengths 
  list.of.stats<-list(Std.res=res.out,Std.res.top=res.top, Leverage=lev.out,
                      Leverage.top=lev.top, DFFITS=dffits.out,
                      DFFITS.top=dffits.top, Cooks=cooks.out,
                      Cooks.top=cooks.top)
  #return the statistics
  list.of.stats
}
support.outlier<-show_outliers(support.be.5.glm, 10)
outliers<-intersect(
  intersect(support.outlier$DFFITS, support.outlier$Leverage),
  support.outlier$Cooks
)
outliers
# No outliers


###############################################################################
# Interaction
###############################################################################

# Run regression
support.int.glm <- glm(ssexmarr ~ househld + rage + rsex + intwww + 
                         eqnow3 + eqnow8 + eqnow11 + religcat + 
                         anyhcond + ruhappy +uprejgay + glsocdist + glchild +
                         religcat*glsocdist,
                       data = cleaned_data,
                       family = binomial(link = "logit"))
vif(lm(ssexmarr ~ rage + rsex + religcat + chattnd2
       + ruhappy + uprejgay + glsocdist + glsocdist*religcat,
       data = cleaned_data))
res <-anova(support.int.glm, test="Chisq")
top_5 <- extract_significant_vars(res,5)
print(top_5)
res

summary(support.int.glm)
anova(null_model, support.int.glm, test = "Chisq")

# Pred
pred.support <- as.numeric(support.int.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = cleaned_data$ssexmarr)
ct.op(pred.support, cleaned_data$ssexmarr)


###############################################################################
# Complete Case: Iterations Not Kept
###############################################################################

# househld
complete_case.data <- complete_case.data %>%
  mutate(househld = recode(househld,
                           "1" = "1",
                           "2" = "2",
                           "3" = "3+",
                           "4" = "3+",
                           "5" = "3+",
                           "6" = "3+",
                           "7" = "3+",
                           "8" = "3+",
  ))
table(complete_case.data['househld'])

# rage - ok
# rsex - ok

# rmarstat
complete_case.data <- complete_case.data %>%
  mutate(rmarstat = recode(rmarstat,
                           "1" = "single",
                           "2" = "married",
                           "3" = "living_as_married",
                           "4" = "separated_or_divorced",
                           "5" = "separated_or_divorced",
                           "6" = "widowed",
  ))
table(complete_case.data['rmarstat'])

# livearea - ok

# hincpast
complete_case.data <- complete_case.data %>%
  mutate(hincpast = recode(hincpast,
                           "1" = "Fallen_behind",
                           "2" = "Kept_up",
                           "3" = "Gone_up",
  ))
table(complete_case.data['hincpast'])
# intwww - ok
# umineth - ok
# eqnow3-11 - ok

# tenshort
complete_case.data <- complete_case.data %>%
  mutate(tenshort = recode(tenshort,
                           "1" = "Own_house",
                           "2" = "Buying_with_mortgage_inc_part_rent",
                           "3" = "Buying_with_mortgage_inc_part_rent",
                           "4" = "Rent_with_exec_or_asoc",
                           "5" = "Rent_with_exec_or_asoc",
                           "6" = "Rent_from_private",
                           "7" = "Buying_with_mortgage_inc_part_rent",
  ))
table(complete_case.data['tenshort'])

# highqual
complete_case.data <- complete_case.data %>%
  mutate(highqual = recode(highqual,
                           "1" = "Alevel_and_higher_ed",
                           "2" = "Alevel_and_higher_ed",
                           "3" = "Alevel_and_higher_ed",
                           "4" = "Good_GCSE_or_other",
                           "5" = "Bad_GCSE_or_no_qual",
                           "6" = "Bad_GCSE_or_no_qual",
                           "7" = "Good_GCSE_or_other",
                           "9" = "Good_GCSE_or_other",
  ))
table(complete_case.data['highqual'])

# tea
complete_case.data <- complete_case.data %>%
  mutate(tea = recode(tea,
                      "1" = "15_or_under",
                      "2" = "16_and_over",
                      "3" = "16_and_over",
                      "4" = "16_and_over",
                      "5" = "16_and_over",
                      "6" = "Still_in_school",
                      "7" = "Still_in_uni",
  ))
table(complete_case.data['tea'])

# work
complete_case.data <- complete_case.data %>%
  mutate(work = recode(work,
                       "1" = "Employee",
                       "2" = "Foreperson_or_supevisor",
                       "3" = "Self_employed",
                       "4" = "Manager",
  ))
table(complete_case.data['work'])

# rsuper
p18 <- categorical_plot(clean.support.data, "rsuper")
p18
complete_case.data <- complete_case.data %>%
  mutate(rsuper = recode(rsuper,
                         "0" = "Yes",
                         "1" = "No",
  ))
summary(clean.support.data)

# rsect
complete_case.data <- complete_case.data %>%
  mutate(rsect = recode(rsect,
                        "1" = "Public",
                        "2" = "Private",
                        "3" = "Voluntary_or_other",
                        "4" = "Voluntary_or_other",
  ))
table(complete_case.data['rsect'])

# tunionsa - ok

# ansseca
table(clean.support.data['ansseca'])
complete_case.data <- complete_case.data %>%
  mutate(ansseca = recode(ansseca,
                          "1" = "Employee",
                          "2" = "Higher_prof",
                          "3" = "Higher_prof",
                          "4" = "Intermediate_or_low_prof",
                          "5" = "Self_employed",
                          "6" = "Intermediate_or_low_prof",
                          "7" = "Intermediate_or_low_prof",
                          "8" = "Intermediate_or_low_prof",
                          "9" = "Never_worked_or_long_term_unemployed",
                          "10" = "N/A",
  ))
table(complete_case.data['ansseca'])

# religcat
complete_case.data <- complete_case.data %>%
  mutate(religcat = recode(religcat,
                           "1" = "Catholic",
                           "2" = "Protestant",
                           "3" = "No_religion",
  ))
table(complete_case.data['religcat'])

# famrelig
complete_case.data <- complete_case.data %>%
  mutate(famrelig = recode(famrelig,
                           "1" = "Catholic",
                           "2" = "Protestant",
                           "3" = "No_religion",
  ))
table(complete_case.data['famrelig'])

# chattnd2
complete_case.data <- complete_case.data %>%
  mutate(chattnd2 = recode(chattnd2,
                           "1" = "Multiple_times_a_month",
                           "2" = "Multiple_times_a_month",
                           "3" = "Multiple_times_a_month",
                           "4" = "Several_times_a_year",
                           "5" = "Several_times_a_year",
                           "6" = "Once_a_year",
                           "7" = "Several_times_a_year",
                           "8" = "Never",
  ))
table(complete_case.data['chattnd2'])

# carehome - ok
# anyhcond - ok
# persinc2 - ok

# orient
complete_case.data <- complete_case.data %>%
  mutate(orient = recode(orient,
                         "1" = "Heterosexual",
                         "2" = "Homosexual",
                         "3" = "Bisexual",
                         "4" = "Other",
  ))
table(complete_case.data['orient'])

# polpart2
complete_case.data <- complete_case.data %>%
  mutate(polpart2 = recode(polpart2,
                           "1" = "Right_or_center",
                           "2" = "Left_or_center",
                           "3" = "Right_or_center",
                           "4" = "Left_or_center",
                           "5" = "Center",
                           "6" = "Other_or_missing",
                           "7" = "Other_or_missing",
  ))
table(complete_case.data['polpart2'])

# ruhappy
complete_case.data <- complete_case.data %>%
  mutate(ruhappy = recode(ruhappy,
                          "1" = "Happy_or_indifferent",
                          "2" = "Happy_or_indifferent",
                          "3" = "Unhappy",
                          "4" = "Unhappy",
                          "8" = "Happy_or_indifferent",
  ))
table(complete_case.data['ruhappy'])

# healthyr
complete_case.data <- complete_case.data %>%
  mutate(healthyr = recode(healthyr,
                           "1" = "Good",
                           "2" = "Good",
                           "3" = "Fair_or_cant_choose",
                           "4" = "Bad",
                           "5" = "Bad",
                           "8" = "Fair_or_cant_choose",
  ))
table(complete_case.data['healthyr'])

# uprejgay
complete_case.data <- complete_case.data %>%
  mutate(uprejgay = recode(uprejgay,
                           "1" = "Very",
                           "2" = "A_little",
                           "3" = "Not",
                           "4" = "A_little",
  ))
table(complete_case.data['uprejgay'])

# glchild
complete_case.data <- complete_case.data %>%
  mutate(glchild = recode(glchild,
                          "1" = "Very_comfortable",
                          "2" = "Fairly_comfortable",
                          "3" = "Indifferent",
                          "4" = "Fairly_uncomfortable",
                          "5" = "Very_uncomfortable",
  ))
table(complete_case.data['glchild'])

# glsocdist
complete_case.data <- complete_case.data %>%
  mutate(glsocdist = recode(glsocdist,
                            "0" = "Less_than_3",
                            "1" = "Less_than_3",
                            "2" = "Less_than_3",
                            "3" = "Less_than_3",
                            "4" = "3_to_5",
                            "5" = "3_to_5",
                            "6" = "More_than_5",
                            "7" = "More_than_5",
                            "8" = "More_than_5",
                            "9" = "More_than_5",
                            "10" = "More_than_5",
                            "11" = "More_than_5",
  ))
table(complete_case.data['glsocdist'])

# glvis
complete_case.data <- complete_case.data %>%
  mutate(glvis = recode(glvis,
                        "0" = "0_scenarios",
                        "1" = "More_than_1_scenario",
                        "2" = "More_than_1_scenario",
                        "3" = "More_than_1_scenario",
  ))
table(complete_case.data['glvis'])

# glborn
complete_case.data <- complete_case.data %>%
  mutate(glborn = recode(glborn,
                         "0" = "Born_that_way",
                         "1" = "choose",
  ))
table(complete_case.data['glborn'])

# knowgl
complete_case.data <- complete_case.data %>%
  mutate(knowgl = recode(knowgl,
                         "0" = "Know",
                         "1" = "Dont_know",
  ))
table(complete_case.data['knowgl'])

# knowtg
complete_case.data <- complete_case.data %>%
  mutate(knowtg = recode(knowtg,
                         "0" = "Know",
                         "1" = "Dont_know",
  ))
table(complete_case.data['knowtg'])

# Set Categorical predictors to become factors
summary(complete_case.data)
complete_case.data[categorical_pred] <- lapply(
  complete_case.data[categorical_pred], function(x) as.factor(x)
)
summary(complete_case.data)

# Set binary predictors to become factors
complete_case.data[binary_pred] <- lapply(
  complete_case.data[binary_pred], function(x) as.factor(x)
)
summary(complete_case.data)

# Recode binary preds to yes and no
to_recode <- binary_pred[!binary_pred %in% c("glborn", "knowgl", "knowtg",
                                             "rsuper")]
complete_case.data <- complete_case.data %>%
  mutate(across(to_recode, ~ifelse(. == "0", "Yes", "No")))
complete_case.data[binary_pred] <- lapply(
  complete_case.data[binary_pred], function(x) as.factor(x)
)
summary(complete_case.data)

# Reset Baseline to level with most number of observations
relevel_predictors_to_most_frequent <- function(data) {
  for (col_name in names(data)) {
    if (is.factor(data[[col_name]])) {
      level_frequencies <- table(data[[col_name]])
      most_frequent_level <- names(which.max(level_frequencies))
      data[[col_name]] <- relevel(data[[col_name]], ref = most_frequent_level)
    }
  }
  return(data)
}
complete_case.data <- relevel_predictors_to_most_frequent(complete_case.data)
summary(complete_case.data)

# Examine all level factors
factors_list <- sapply(complete_case.data, is.factor)  # Identify factors
factors_data <- complete_case.data[, factors_list]     # Subset only factors
leveling <- lapply(factors_data, levels)
leveling

# Outcome Distribution
table(complete_case.data['ssexmarr'])

complete_case_clean.data <- na.omit(complete_case.data)
table(complete_case_clean.data['ssexmarr'])

# Run regression - iterations not kept
complete.case.glm <- glm(ssexmarr ~ rage + rsex + eqnow11 + rsect + religcat +
                           anyhcond + uprejgay + glchild,
                         family = binomial(link = "logit"),
                         data = complete_case_clean.data)
res <-anova(complete.case.glm, test="Chisq")
res
top_5 <- extract_significant_vars(res,5)

top_5
# Iter 1 drop:
# [1] "knowtg"   "umineth"  "glvis"    "tenshort" "chattnd2"
# Iter 2 drop:
# [1] "eqnow8"    "glborn"    "polpart2"  "glsocdist" "orient"   
# Iter 3 drop:
# [1] "eqnow9"   "eqnow7"   "rmarstat" "persinc2" "intwww"  
# Iter 4 drop:
# [1] "healthyr" "tunionsa" "famrelig" "work"     "hincpast"
# Iter 5 drop:
# [1] "carehome" "knowgl"   "highqual" "rsuper"   "ansseca" 
# Iter 6 drop:
# [1] "tea"      "livearea"

summary(complete.case.glm)
# Iter 7 drop (t-test):
# househld, eqnow3, ruhappy

# Pred
pred.support <- as.numeric(complete.case.glm$fitted.values > 0.5)
glm.support.dat <- data.frame(predicted = pred.support,
                              observed = complete_case_clean.data$ssexmarr)
ct.op(pred.support, complete_case_clean.data$ssexmarr)
# 0.88  0.57

summary(complete.case.glm)

null_model <- glm(ssexmarr ~ 1,
                  family = binomial(link = "logit"),
                  data = complete_case_clean.data) 
anova(null_model, complete.case.glm, test = "Chisq")

# Null deviance: 418.85  on 331  degrees of freedom
# Residual deviance: 292.41  on 318  degrees of freedom

