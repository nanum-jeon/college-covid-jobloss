# src/moderator.R

# NA-aware one-hot; keeps ALL factor levels (no dropping)
make_dummies_keep_ref <- function(f, prefix) {
  stopifnot(is.factor(f))
  lvls <- levels(f)
  cols <- lapply(lvls, function(lvl) ifelse(is.na(f), NA_integer_, as.integer(f == lvl)))
  names(cols) <- paste0(prefix, gsub("[^a-z0-9]+", "_", lvls))
  tibble::as_tibble(cols)
}
 

make_moderator <- function (raw, 
                            pubid_col = "PUBID", 
                            race_col = "KEY_RACE_ETHNICITY_1997", 
                            tone_cols = c("YIR-530_2008", "YIR-530_2009", "YIR-530_2010"), 
                            invalid_codes = c(-4, -5), 
                            drop_non_bw_rows = FALSE, make_dummies = TRUE){
  
  stopifnot(all(c(pubid_col, race_col, tone_cols) %in% names(raw)))
  base <- raw %>% transmute(PUBID = .data[[pubid_col]], race = case_when(.data[[race_col]] == 
                                                                           4 ~ "white", .data[[race_col]] == 1 ~ "black", TRUE ~ 
                                                                           NA_character_))
  tones <- raw %>% select(PUBID, tone_cols) %>% mutate(skin_color_scale = case_when(`YIR-530_2008` != 
                                                                                      -5 ~ `YIR-530_2008`, (`YIR-530_2009` != -4 | `YIR-530_2009` != 
                                                                                                              -5) ~ `YIR-530_2009`, (`YIR-530_2010` != -4 | `YIR-530_2010` != 
                                                                                                                                       -5) ~ `YIR-530_2010`)) %>% mutate(skin_tone_scale = case_when(skin_color_scale < 
                                                                                                                                                                                                       0 ~ NA, TRUE ~ skin_color_scale)) %>% select(PUBID, skin_tone_scale)
  base <- base %>% left_join(tones, by = "PUBID")
  out <- base %>% mutate(skin_tone = case_when(dplyr::between(skin_tone_scale, 
                                                              0, 6) ~ "light", dplyr::between(skin_tone_scale, 7, 10) ~ 
                                                 "dark", TRUE ~ NA_character_), race_skin = case_when(race == 
                                                                                                        "white" ~ "white", race == "black" & !is.na(skin_tone) ~ 
                                                                                                        paste0(skin_tone, "_black"), TRUE ~ NA_character_), race = factor(race, 
                                                                                                                                                                          levels = c("white", "black")), skin_tone = factor(skin_tone, 
                                                                                                                                                                                                                            levels = c("light", "dark")), race_skin = factor(race_skin, 
                                                                                                                                                                                                                                                                             levels = c("white", "light_black", "dark_black"))) %>% 
    select(PUBID, race, race_skin, skin_tone_scale)
  if (drop_non_bw_rows) {
    out <- out %>% filter(!is.na(race))
  }
  if (!make_dummies) 
    return(out)
  make_dummies_keep_ref <- function(f, prefix) {
    stopifnot(is.factor(f))
    lvls <- levels(f)
    cols <- lapply(lvls, function(lvl) ifelse(is.na(f), NA_integer_, 
                                              as.integer(f == lvl)))
    names(cols) <- paste0(prefix, gsub("[^a-z0-9]+", "_", 
                                       lvls))
    tibble::as_tibble(cols)
  }
  out %>% bind_cols(make_dummies_keep_ref(out$race, "race_"), 
                    make_dummies_keep_ref(out$race_skin, "rs_"))
  
} 





  