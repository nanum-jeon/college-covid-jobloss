# scripts/33_create_moderator.R
# -----------------------------------------
# Creates race and race skin color vars
# -----------------------------------------

source(here::here("scripts", "00_setup.R"))  # sets DIR_* and loads packages
# 00_setup.R should source your src/ files that define the helpers above.

message("[31] Loading NLSY97 data...")

# skin color data 
raw_path <- here::here(DIR_DATA, "college_covid_jobloss", "college_covid_jobloss.R")
raw <- load_nlsy97_core(raw_path)

# interviewer  data 
interviewer_id_path <- here::here(DIR_DATA, "nlsy97_interviewer_id", "nlsy97_interviewer_id.R")
interviewer_id <- load_nlsy97_core(interviewer_id_path)


# --- Skin color 
moderator <- make_moderator(raw)


# --- 3. Skin Color Rating Adjustment ---

# Select and join the skin color and interviewer ID data
debiased_skin_color <- raw %>% 
  select(PUBID, matches("YIR-530_")) %>%
  left_join(interviewer_id, by = "PUBID")

# Reshape data from wide to long format
df_long <- debiased_skin_color %>%
  pivot_longer(
    cols = -PUBID,
    names_to = c(".value", "interview_round"),
    names_pattern = "(.*)_(\\d{4})"
  ) %>%
  rename(
    skin_color_rating = `YIR-530`,
    interviewer_id = INTERVIEWER_PUBID
  ) %>%
  # Filter to keep only valid, measured observations for each respondent
  filter(skin_color_rating >= 0)


# --- 4. Run Mixed-Effects Model and Create Debiased Score ---

# Fit the model to estimate interviewer effects
model <- lmer(skin_color_rating ~ interview_round + (1 | interviewer_id), 
              data = df_long) 

# Extract the interviewer-specific adjustments (biases)
interviewer_adjustments <- ranef(model)$interviewer_id %>%
  as.data.frame() %>%
  rownames_to_column("interviewer_id") %>%
  rename(interviewer_bias = `(Intercept)`) %>%
  # Fix data type for joining
  mutate(interviewer_id = as.integer(interviewer_id))

# Join adjustments back to main data and calculate the debiased continuous score
interviewer_bias <- df_long %>%
  left_join(interviewer_adjustments, by = "interviewer_id") %>%
  select(PUBID, interviewer_bias)
  
 
moderator <- moderator %>% 
  left_join(interviewer_bias, by = "PUBID") %>% 
  mutate(
    skin_tone_debiased = skin_tone_scale - interviewer_bias
  )

# --- 5. Create Categorical Race and Skin Tone Variables ---

# Calculate the equivalent cut-point on the debiased scale
prop_dark_original <- mean(moderator$skin_tone_scale >= 7, na.rm = TRUE)
percentile_cutoff <- 1 - prop_dark_original
equivalent_cutoff <- quantile(moderator$skin_tone_scale, probs = percentile_cutoff, na.rm = TRUE)
 
 
# Create the final df_skin dataframe with all categorical variables
moderator <- moderator %>% 
  mutate( 
    # Debiased light/dark based on percentile-equivalent cutoff
    skin_debiased = ifelse(skin_tone_debiased <= equivalent_cutoff, "light", "dark"),
    # Combined race and debiased skin tone
    race_skin_debiased = case_when(
      race == "white"                       ~ "white",
      race == "black" & !is.na(skin_debiased) ~ paste0(skin_debiased, "_black"),
      TRUE                                    ~ NA_character_
    ),
    # Set factor levels for modeling  
    race_skin_debiased = factor(race_skin_debiased, levels = c("white", "light_black", "dark_black"))
  )  


# Create the final data frame with dummy variables added
moderator_vars <- moderator %>%
  bind_cols(
    make_dummies_keep_ref(moderator$race_skin_debiased, "de_")
  ) 

moderator_vars %>% 
  group_by(race_skin, race_skin_debiased) %>% 
  count()

# ---- export moderator_vars to intermediate for downstream scripts ----
out_dir <- here::here(DIR_INTERIM)   # DIR_INTERIM set in 00_setup.R
 
rds_file <- file.path(out_dir, "moderator_vars.rds")
 
saveRDS(moderator_vars, rds_file)

message("[moderator_vars] Saved: ", rds_file)

# Return invisibly for interactive sourcing
invisible(moderator_vars)
