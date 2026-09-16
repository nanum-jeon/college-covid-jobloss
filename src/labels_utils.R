# src/labels.R
# -------------------------------------------------------------------
# Variable metadata used across scripts (tables, figures, diagnostics)
# -------------------------------------------------------------------

# 1. Desired variable order (for consistent tables/figures)
var_order <- c(
  "female", "maeduc", "faeduc", "parinc",
  "rural_12", "south_12", "intact_12", "sibsz",
  "asvab_pst", "hs_gpa", "col_prep",
  "delinq", "subs_use", "fight",
  "cohabited_18", "child_18",
  "tchgd", "schsafe",
  "stolen", "threatened",
  "pct_peer_about75", "pct_peer_more_than90"
)

# 2. Pretty labels for output (LaTeX-safe)
var_labels <- c(
  female    = "Female",
  maeduc    = "Mother's Years of Education",
  faeduc    = "Father's Years of Education",
  parinc    = "Parents' Income (\\$1,000s)",
  rural_12  = "Rural Residence at Age 12",
  south_12  = "South Residence at Age 12",
  intact_12 = "Intact Family at Age 12",
  sibsz     = "Number of Siblings",
  asvab_pst = "ASVAB Percentile Score",
  hs_gpa    = "High School GPA",
  col_prep  = "College Prep Program",
  delinq    = "Delinquency Index",
  subs_use  = "Substance Use Index",
  fight     = "Fights at School",
  cohabited_18 = "Cohabited at Age 18",
  child_18     = "Had Child at Age 18",
  tchgd     = "Teacher Cares",
  schsafe   = "School Feels Safe",
  stolen    = "Stolen at School",
  threatened = "Threatened at School",
  pct_peer_about75    = "75\\% Peers Expected College",
  pct_peer_more_than90= "90\\%+ Peers Expected College"
)

# 3. Section mapping (for grouping in tables/figures)
var_section <- c(
  # Background
  female    = "Background",
  maeduc    = "Background",
  faeduc    = "Background",
  parinc    = "Background",
  rural_12  = "Background",
  south_12  = "Background",
  intact_12 = "Background",
  sibsz     = "Background",
  
  # Achievement and Behavior
  asvab_pst   = "Achievement and Behavior",
  hs_gpa      = "Achievement and Behavior",
  col_prep    = "Achievement and Behavior",
  delinq      = "Achievement and Behavior",
  subs_use    = "Achievement and Behavior",
  fight       = "Achievement and Behavior",
  cohabited_18 = "Achievement and Behavior",
  child_18     = "Achievement and Behavior",
  
  # School Environment
  tchgd               = "School Environment",
  schsafe             = "School Environment",
  stolen              = "School Environment",
  threatened          = "School Environment",
  pct_peer_about75    = "School Environment",
  pct_peer_more_than90= "School Environment"
)

# 4. Section levels (ordering of sections in outputs)
section_levels <- c("Background", "Achievement and Behavior", "School Environment", "Other")





# -------------------------------------------------------------------
# Variable metadata used across scripts (tables, figures, diagnostics)
# -------------------------------------------------------------------

# 1. Desired variable order (for consistent tables/figures)
des_var_order <- c(
  "compcoll25", "hiskil19", "covid_jobloss", 
  "female", "maeduc", "faeduc", "parinc",
  "rural_12", "south_12", "intact_12", "sibsz",
  "asvab_pst", "hs_gpa", "col_prep",
  "delinq", "subs_use", "fight",
  "cohabited_18", "child_18",
  "tchgd", "schsafe",
  "stolen", "threatened",
  "pct_peer_about75", "pct_peer_more_than90"
)

# 2. Pretty labels for output (LaTeX-safe)
des_var_labels <- c(
  compcoll25   = "Four-year College Completion", 
  hiskil19 = "High Skilled Occupation", 
  covid_jobloss = "COVID-19 Job Loss",
  female    = "Female",
  maeduc    = "Mother's Years of Education",
  faeduc    = "Father's Years of Education",
  parinc    = "Parents' Income (\\$1,000s)",
  rural_12  = "Rural Residence at Age 12",
  south_12  = "South Residence at Age 12",
  intact_12 = "Intact Family at Age 12",
  sibsz     = "Number of Siblings",
  asvab_pst = "ASVAB Percentile Score",
  hs_gpa    = "High School GPA",
  col_prep  = "College Prep Program",
  delinq    = "Delinquency Index",
  subs_use  = "Substance Use Index",
  fight     = "Fights at School",
  cohabited_18 = "Cohabited at Age 18",
  child_18     = "Had Child at Age 18",
  tchgd     = "Teacher Cares",
  schsafe   = "School Feels Safe",
  stolen    = "Stolen at School",
  threatened = "Threatened at School",
  pct_peer_about75    = "75\\% Peers Expected College",
  pct_peer_more_than90= "90\\%+ Peers Expected College"
)

# 3. Section mapping (for grouping in tables/figures)
des_var_section <- c(
  "Four-year College Completion" = "Treatment, Mediator, and Outcome",
  "High Skilled Occupation" = "Treatment, Mediator, and Outcome", 
  "COVID-19 Job Loss" = "Treatment, Mediator, and Outcome",
  "Female" = "Background",
  "Mother's Years of Education" = "Background",
  "Father's Years of Education" = "Background", 
  "Parents' Income (\\$1,000s)" = "Background",
  "Rural Residence at Age 12" = "Background",
  "South Residence at Age 12" = "Background",
  "Intact Family at Age 12" = "Background",
  "Number of Siblings" = "Background",
  "ASVAB Percentile Score" = "Achievement and Behavior",
  "High School GPA" = "Achievement and Behavior",
  "College Prep Program" = "Achievement and Behavior", 
  "Delinquency Index" = "Achievement and Behavior",
  "Substance Use Index" = "Achievement and Behavior",
  "Fights at School" = "Achievement and Behavior",
  "Cohabited at Age 18" = "Achievement and Behavior",
  "Had Child at Age 18" = "Achievement and Behavior",
  "Teacher Cares" = "School Environment",
  "School Feels Safe" = "School Environment",
  "Stolen at School" = "School Environment", 
  "Threatened at School" = "School Environment",
  "75\\% Peers Expected College" = "School Environment",
  "90\\%+ Peers Expected College" = "School Environment"
)

# 4. Section levels (ordering of sections in outputs)
des_section_levels <- c(
  "Treatment, Mediator, and Outcome",
  "Background", "Achievement and Behavior", 
  "School Environment", 
  "Other")

