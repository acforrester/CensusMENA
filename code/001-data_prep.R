#
#   1. Data Preparation
#
#   Set up the ACS PUMS data by contruction person and household databases
#   to query using SQL. For each database, develop an indicator variable
#   for the Middle East and North Africa using birthplace (`bpld`) and
#   ancestry (`ancestr1`).
#
#   MENA respondents are classified at the person level by looking first
#   at a MENA ancestry or a MENA birthplace for immigrants. This method
#   mirrors the "diaspora" approach implemented by the Migration Policy
#   Institute in a 2015 report on MENA immigrants.
#
# *****************************************************************************


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PARAMETERS ----
#
#   Set the birthplace codes for the MENA classification and the columns to
#   subset in the IPUMS-USA data. See Appendix Table A1 for the `bpld` codes.
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Middle East and North Africa `bpld` codes
mena_countries <- c(52000,        # Afghanistan
                    52200,        # Iran
                    53000:54700,  # Middle East
                    60010:60019)  # North Africa

# Columns to subset by type
id_cols    <- c("serial", "cluster", "cbhhtype", "strata", "pernum", "gq")
demog_vars <- c("hhincome",
                "met2013",
                "statefip",
                "incwage",
                "ftotinc",
                "famunit",
                "famsize",
                "age",
                "sex",
                "raced",
                "hispan",
                "ancestr1",
                "citizen",
                "bpld",
                "bpl",
                "speakeng",
                "educd",
                "empstatd",
                "empstat",
                "occsoc",
                "indnaics",
                "poverty")

# Concatenate columns
cols_to_subset <- list(
  "household" = c(id_cols, demog_vars, c("hhwt", paste0("repwt", 1:80))),
  "person"    = c(id_cols, demog_vars, c("perwt", paste0("repwtp", 1:80)))
)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# LOAD THE DATA AND PLACE INTO DATABASE ----
#
#   Create a person and household level database from the IPUMS-USA data.
#
#   Apply the 2023 proposed OMB standards for race and ethnicity by first
#   approximating MENA by ancestry or birthplace. Then combine race and
#   ethnicity into a single category `RACENEW`. The categories are:
#
#     0 = Middle East and North Africa
#     1 = White only
#     2 = Black only
#     3 = American Indian and Alaska Native (AI/AN) only
#     4 = Asian only
#     5 = Pacific Island and Hawaiian Native (NHPI) only
#     6 = Some other race
#     7 = Two or more races
#     8 = Hispanic or Latino
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Set up database
db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

## Person-level data ----
read_stata("./data/usa_00042.dta", col_select = all_of(cols_to_subset$person)) %>%

  # Recode MENA
  mutate(

    MENA2 = case_when(
      bpl >= 150 & (ancestr1 %in% 400:496 | bpld %in% mena_countries) ~ 1,
      bpl  < 150 &  ancestr1 %in% 400:496 ~ 1,
      TRUE ~ 0),

    # New Race classifier
    RACENEW = case_when(
      MENA2 == 0 & hispan == 0 & raced %in% 100:150 ~ "1_White",
      MENA2 == 0 & hispan == 0 & raced  ==  200     ~ "2_Black",
      MENA2 == 0 & hispan == 0 & raced %in% 300:399 ~ "3_AIAN",
      MENA2 == 0 & hispan == 0 & raced %in% c(400:620, 640:679) ~ "4_Asian",
      MENA2 == 0 & hispan == 0 & raced %in% c(630:634, 680:699) ~ "5_PacificIslander",
      MENA2 == 0 & hispan == 0 & raced  ==  700     ~ "6_Other",
      MENA2 == 0 & hispan == 0 & raced   >  700     ~ "7_TwoPlus",
      MENA2 == 0 & hispan  > 0                      ~ "8_Hispanic",
      MENA2 == 1 ~ "0_MENA")) %>%

  # Place in DB
  copy_to(db, ., "acs_p", overwrite = TRUE, temporary = FALSE)


## Household-level data ----
read_stata("./data/usa_00042.dta", col_select = all_of(cols_to_subset$household)) %>%

  # Keep first record
  filter(pernum == 1) %>%

  # Recode MENA
  mutate(

    MENA2 = case_when(
      bpl >= 150 & (ancestr1 %in% 400:496 | bpld %in% mena_countries) ~ 1,
      bpl  < 150 &  ancestr1 %in% 400:496 ~ 1,
      TRUE ~ 0),

    # New Race classifier
    RACENEW = case_when(
      MENA2 == 0 & hispan == 0 & raced %in% 100:150 ~ "1_White",
      MENA2 == 0 & hispan == 0 & raced  ==  200     ~ "2_Black",
      MENA2 == 0 & hispan == 0 & raced %in% 300:399 ~ "3_AIAN",
      MENA2 == 0 & hispan == 0 & raced %in% c(400:620, 640:679) ~ "4_Asian",
      MENA2 == 0 & hispan == 0 & raced %in% c(630:634, 680:699) ~ "5_PacificIslander",
      MENA2 == 0 & hispan == 0 & raced  ==  700     ~ "6_Other",
      MENA2 == 0 & hispan == 0 & raced   >  700     ~ "7_TwoPlus",
      MENA2 == 0 & hispan  > 0                      ~ "8_Hispanic",
      MENA2 == 1 ~ "0_MENA")) %>%

  # Place in DB
  copy_to(db, ., "acs_h", overwrite = TRUE, temporary = FALSE)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# APPLY SURVEY DESIGN ----
#
#   Use the `srvyr` package to set the complex survey design for the person
#   and household-level data. Call the data from the database with SQL and
#   specify the appropriate replication weights `repwt` for household analyses
#   and `repwtp` for person-level analyses.
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

acs_db_design_h <- as_survey_rep(
  .data      = tbl(db, "acs_h"),
  weights    = hhwt,
  repweights = repwt1:repwt80,
  scale      = 4/80,
  rscales    = rep(1, 80),
  mse        = TRUE,
  type       = "JK1",
  variables  = -c(matches("^repwt")))


acs_db_design_p <- as_survey_rep(
  .data      = tbl(db, "acs_p"),
  weights    = perwt,
  repweights = repwtp1:repwtp80,
  scale      = 4/80,
  rscales    = rep(1, 80),
  mse        = TRUE,
  type       = "JK1",
  variables  = -c(matches("^repwtp")))


### EOF ###
