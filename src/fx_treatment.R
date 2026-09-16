# src/treatment.R


make_treatment <- function(df,
                           id_col = "PUBID",
                           ba_month_col = "CVC_BA_DEGREE",
                           highest_deg_col = "CVC_HIGHEST_DEGREE_EVER",
                           dob_month_col = "dob_m") {
  # deps: dplyr (>=1.0), rlang
  need <- c(id_col, ba_month_col, highest_deg_col, dob_month_col)
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  out <- df |>
    dplyr::transmute(
      !!rlang::sym(id_col),
      age_at_ba = dplyr::if_else(.data[[ba_month_col]] > 0,
                                 (.data[[ba_month_col]] - .data[[dob_month_col]])/12,
                                 NA_real_),
      ba_code = .data[[ba_month_col]],
      hi_deg  = .data[[highest_deg_col]]
    ) |>
    dplyr::mutate(
      ba_by25 = dplyr::case_when(
        !is.na(age_at_ba) & age_at_ba <= 25 ~ 1,            # completed by 25
        ba_code == -4 ~ 0,                                  # never BA
        ba_code == -3 & hi_deg >= 4 ~ NA_real_,            # BA somewhere, timing unknown
        TRUE ~ 0                                           # known age >=26 or no BA evidence
      )
    )
  
  out |>
    dplyr::select(!!rlang::sym(id_col), age_at_ba, ba_by25)
}





# Expect df to contain: PUBID, dob_m, CVC_HS_DIPLOMA, CVC_GED, CVC_HIGHEST_DEGREE_EVER
make_hs_by20 <- function(df,
                            id_col = "PUBID",
                            dobm_col = "dob_m",
                            hs_month_col = "CVC_HS_DIPLOMA",
                            ged_month_col = "CVC_GED",
                            highest_deg_col = "CVC_HIGHEST_DEGREE_EVER") {
  
  # deps: dplyr, rlang
  need <- c(id_col, dobm_col, hs_month_col, ged_month_col, highest_deg_col)
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  out <- df %>%
    dplyr::transmute(
      !!rlang::sym(id_col),
      # Ages (years) when month indices are > 0
      age_at_hs  = dplyr::if_else(.data[[hs_month_col]]  > 0,
                                  (.data[[hs_month_col]]  - .data[[dobm_col]]) / 12,
                                  NA_real_),
      age_at_ged = dplyr::if_else(.data[[ged_month_col]] > 0,
                                  (.data[[ged_month_col]] - .data[[dobm_col]]) / 12,
                                  NA_real_),
      
      # keep raw codes for -3 (unknown timing) / -4 (never)
      hs_code  = .data[[hs_month_col]],
      ged_code = .data[[ged_month_col]],
      hi_deg   = .data[[highest_deg_col]]
    ) %>%
    dplyr::mutate(
      # HS-by-20: HS diploma <21 OR GED <21
      hs_by20 = dplyr::case_when(
        (!is.na(age_at_hs)  & age_at_hs  <= 20) |
          (!is.na(age_at_ged) & age_at_ged <= 20) ~ 1,
        
        # Explicit "never" only when BOTH are valid skips (-4)
        hs_code == -4 & ged_code == -4 ~ 0,
        
        # Timing unknown (-3) but eventually HS/GED/BA+ → leave NA
        (hs_code == -3 | ged_code == -3) & hi_deg >= 2 ~ NA_real_,
        
        # Otherwise unknown
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(dplyr::all_of(id_col), hs_by20, age_at_hs, age_at_ged)
  
  out
}


# Expect df to contain: PUBID, dob_m, CVC_HS_DIPLOMA, CVC_GED, CVC_HIGHEST_DEGREE_EVER
make_hs_completed <- function(df) {
  
  high_school_completed <- df %>% 
    # Denote completion age
    # those whose values are -3 (invalid missing) do not meet either
    # requirement below and get coded as NA
    mutate(completion_age = case_when(CVC_HS_DIPLOMA == -4 ~ NA,
                                      CVC_HS_DIPLOMA >= 0 ~ ((CVC_HS_DIPLOMA - dob_m) / 12))) %>%
    dplyr::select(PUBID, CVC_HS_DIPLOMA, CVC_HIGHEST_DEGREE_EVER, completion_age) %>%
    # left_join(age_last, by = "PUBID") %>%
    mutate(high_school_completed = case_when(CVC_HS_DIPLOMA >= 0 & completion_age <= 20 ~ 1,
                                             CVC_HS_DIPLOMA == -4 ~ 0, 
                                             CVC_HS_DIPLOMA == -3 & CVC_HIGHEST_DEGREE_EVER >=2~ NA, 
                                             CVC_HS_DIPLOMA > 0 & completion_age  > 20 ~ NA)) %>% 
    dplyr::select(PUBID, high_school_completed)
  
  GED <- df %>% 
    # Denote completion age
    # those whose values are -3 (invalid missing) do not meet either
    # requirement below and get coded as NA
    mutate(completion_age = case_when(CVC_GED == -4 ~ NA,
                                      CVC_GED >= 0 ~ ((CVC_GED - dob_m) / 12))) %>%
    dplyr::select(PUBID, CVC_GED, completion_age, CVC_HIGHEST_DEGREE_EVER) %>%
    # left_join(age_last, by = "PUBID") %>%
    mutate(GED_completed = case_when(CVC_GED >= 0 & completion_age <= 20 ~ 1,
                                     CVC_GED == -4 ~ 0, 
                                     CVC_GED == -3 & CVC_HIGHEST_DEGREE_EVER >=1~ NA, 
                                     CVC_GED > 0 & completion_age > 20 ~ NA)) %>%
    dplyr::select(PUBID, GED_completed) 
  
  out <- high_school_completed %>% 
    left_join(GED, by = "PUBID") %>% 
    mutate(hs_completed = case_when(high_school_completed == 1 | GED_completed == 1 ~ 1, 
                                    high_school_completed == 0 & GED_completed == 0 ~ 0)) %>% 
    dplyr::select(PUBID, hs_completed)
  
  out
}


