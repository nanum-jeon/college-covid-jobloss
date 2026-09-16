
# Reorder 
reorder_by_var <- function(df, var_order) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>%
    mutate(variable = factor(variable, levels = var_order)) %>%
    arrange(variable)
}

# Helpers
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt3_paren <- function(x, y) ifelse(is.na(x), "", sprintf("%.3f (%.3f)", x, ifelse(is.na(y), 0, y)))
fmt_missing <- function(n_miss, prop) {
  ifelse(is.na(n_miss), "",
         sprintf("%d (%.1f\\%%)", n_miss, ifelse(is.na(prop), 0, 100 * prop)))
}


check_var_alignment <- function(var_order, var_labels, var_section) {
  all_vars <- unique(var_order)
  
  # check labels
  missing_labels <- setdiff(all_vars, names(var_labels))
  extra_labels   <- setdiff(names(var_labels), all_vars)
  
  # check sections
  missing_sections <- setdiff(all_vars, names(var_section))
  extra_sections   <- setdiff(names(var_section), all_vars)
  
  # report
  if (length(missing_labels) == 0 && length(extra_labels) == 0 &&
      length(missing_sections) == 0 && length(extra_sections) == 0) {
    message("✅ All variables aligned across var_order, var_labels, and var_section.")
  } else {
    if (length(missing_labels)) 
      message("⚠️ Missing labels for: ", paste(missing_labels, collapse = ", "))
    if (length(extra_labels))   
      message("⚠️ Extra labels not in var_order: ", paste(extra_labels, collapse = ", "))
    if (length(missing_sections)) 
      message("⚠️ Missing section assignments for: ", paste(missing_sections, collapse = ", "))
    if (length(extra_sections))   
      message("⚠️ Extra section assignments not in var_order: ", paste(extra_sections, collapse = ", "))
  }
}


# ---- core time variables ----
build_dob_months <- function(raw) {
  raw |>
    dplyr::rename(
      age1997   = `CV_AGE(MONTHS)_INT_DATE_1997`,
      month1997 = CV_INTERVIEW_CMONTH_1997
    ) |>
    dplyr::mutate(dob_m = month1997 - age1997) |>
    dplyr::select(PUBID, dob_m)
}

build_int_month <- function(raw, dob_m) {
  raw |>
    dplyr::select(PUBID, dplyr::starts_with("CV_INTERVIEW_CMONTH")) |>
    tidyr::pivot_longer(dplyr::starts_with("CV_INTERVIEW_CMONTH"),
                        values_to = "int_month", names_to = "year") |>
    dplyr::mutate(
      year = as.numeric(gsub("CV_INTERVIEW_CMONTH_", "", year)),
      wave = dplyr::case_when(
        year <= 2011 ~ year - 1996,
        year == 2013 ~ 16,
        year == 2015 ~ 17,
        year == 2017 ~ 18,
        year == 2019 ~ 19
      )
    ) |>
    dplyr::left_join(dob_m, by = "PUBID") |>
    dplyr::mutate(age = (int_month - dob_m) / 12) |>
    dplyr::select(PUBID, wave, year, int_month, age)
}

build_age_last <- function(raw, int_month) {
  raw |>
    dplyr::rename(wave = CVC_RND) |>
    dplyr::select(PUBID, wave) |>
    dplyr::left_join(int_month, by = c("PUBID", "wave")) |>
    dplyr::rename(age_last = age) |>
    dplyr::select(PUBID, age_last)
}


p2star <- function(x){
  case_when(between(x, 0.05, 1) ~ "",
            between(x, 0.01, 0.05) ~ "*",
            between(x, 0.001, 0.01) ~ "**",
            between(x, 0, 0.001) ~ "***")
}

create_mean_tab <- function(data) {
  data %>% mutate(
    mean_p_value = 2 * pnorm(-abs(mean / se)),  # Calculate p-value
    mean_star = p2star(mean_p_value),           # Add significance stars
    gap_star = p2star(p_value)                  # Add significance stars for gap
  ) %>%
    pivot_longer(cols = c(mean, se, gap), names_to = "type", values_to = "value") %>%
    mutate(
      final = case_when(
        (mean_star != "" & type == "mean") ~ paste0(sprintf("%.3f", value), "$^{" , mean_star, "}$"), 
        (mean_star == "" & type == "mean") ~ paste0(sprintf("%.3f", value)), 
        (type == "se") ~ paste0("(", sprintf("%.3f", value), ")"),           # Added missing comma here
        (gap_star != "" & type == "gap") ~ paste0(sprintf("%.3f", value), "$^{" , gap_star, "}$"), 
        (gap_star == "" & type == "gap") ~ paste0(sprintf("%.3f", value))
      )
    ) %>%
    select(compcoll25, hiskil19, race, type, final) %>%
    pivot_wider(names_from = race, values_from = final) %>% 
    arrange(compcoll25) 
}
create_gap_tab <- function(data){
  data %>%
    mutate(
      mean_p_value = 2 * pnorm(-abs(mean / se)),  # Calculate p-value
      mean_star = p2star(mean_p_value),           # Add significance stars
      gap_star = p2star(p_value)                  # Add significance stars for gap
    ) %>%
    pivot_longer(cols = c(mean, se, gap), names_to = "type", values_to = "value") %>%
    mutate(
      final = case_when(
        (mean_star != "" & type == "mean") ~ paste0(sprintf("%.3f", value), "$^{" , mean_star, "}$"), 
        (mean_star == "" & type == "mean") ~ paste0(sprintf("%.3f", value)), 
        (type == "se") ~ paste0("(", sprintf("%.3f", value), ")"),           # Added missing comma here
        (gap_star != "" & type == "gap") ~ paste0(sprintf("%.3f", value), "$^{" , gap_star, "}$"), 
        (gap_star == "" & type == "gap") ~ paste0(sprintf("%.3f", value))
      )
    ) %>%
    select(compcoll25, hiskil19, race, type, final) %>%
    pivot_wider(names_from = race, values_from = final)  %>%
    filter(type == "gap") %>% 
    pivot_wider(names_from = type,          # Pivot based on the 'type' column (mean, se, gap)
                values_from = c(White, Black, Light_Black, Dark_Black)) %>% 
    arrange(compcoll25) 
}


