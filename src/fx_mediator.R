# src/mediator.R

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(rlang)
})

# --- high skill --- # 
make_highskill_mediator <- function(df,
                          id_col = "PUBID",
                          occ_col = "YEMP_OCCODE-2002.01_2019") {
  stopifnot(all(c(id_col, occ_col) %in% names(df)))
  df %>%
    dplyr::transmute(
      !!rlang::sym(id_col),
      occ_2019 = .data[[occ_col]],
      highskill_19 = dplyr::case_when(
        occ_2019 < 0 ~ NA_real_,
        occ_2019 >= 2600 & occ_2019 <= 2760 ~ 0,
        occ_2019 >= 10   & occ_2019 <= 3250 ~ 1,
        TRUE ~ 0
      )
    ) %>% 
    select(PUBID, highskill_19)
}


# Employed in 2019 from the occupation code
# Logic:
#   occ_2019 >= 0  -> employed_2019 = 1
#   occ_2019 == -4 -> employed_2019 = 0   (not working)
#   occ_2019 in {-1,-2,-3,-5} -> NA       (unknown / non-interview / refused)
compute_employed_2019 <- function(df,
                                  id_col = "PUBID",
                                  occ_col = "YEMP_OCCODE-2002.01_2019") {
  stopifnot(all(c(id_col, occ_col) %in% names(df)))
  
  df %>%
    dplyr::transmute(
      !!rlang::sym(id_col),
      occ_2019 = .data[[occ_col]],
      employed_2019 = dplyr::case_when(
        occ_2019 >= 0 ~ 1,     # includes military/uncodable 9800–9990 (still employed)
        occ_2019 == -4 ~ 0,    # not employed
        TRUE ~ NA_real_        # -1,-2,-3,-5 etc.
      )
    ) %>% 
    select(PUBID, employed_2019)
}

 

# --- remote --- #
 


# 1) Load/prepare O*NET "remote" scores (Telephone, Email, Letters/Memos)
#    onet_df columns expected: onet_soc, title, element, value
make_onet_remote_index <- function(onet_df) {
  onet_df %>%
    filter(element %in% c("Telephone", "Electronic Mail", "Letters and Memos")) %>%
    mutate(soc = str_extract(onet_soc, "\\d{2}-\\d{1}")) %>%
    group_by(soc) %>%
    summarise(remote = mean(value, na.rm = TRUE))
}

make_remote_mediator <- function(raw, 
                                 crosswalk = coc_soc_xwalk, 
                                 onet_index = onet_remote_idx, 
                                 occ_col = "YEMP_OCCODE-2002.01_2019", 
                                 threshold = 4) {
  raw %>%
    transmute(
      PUBID = PUBID,
      coc_2002 = ifelse(.data[[occ_col]] >= 0,
                        .data[[occ_col]], NA)
    ) %>%
    arrange(coc_2002) %>%
    left_join(crosswalk, by = "coc_2002") %>%
    left_join(onet_index, by = "soc") %>%
    select(PUBID, remote) %>%
    mutate(remote_2019 = case_when(
      remote >= threshold ~ 1,
      remote < threshold ~ 0
    )) %>% 
    select(PUBID, remote_2019)
}
 