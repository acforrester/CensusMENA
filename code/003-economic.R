#
#   3. Tabulate Economic Variables
#
#   Produce tables of various economic variables mirrored from the Data Profile
#   tables from the American Community Survey.
#
#   Use a combination of survey designs for household and person-level
#   variables and the associated rep weights.
#
# *****************************************************************************


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EMPLOYMENT STATUS -----
#
#   Universe: civilian noninstitutional population ages 16 and over
#
#   Tabulations:
#     1. Population ages 16 and over
#     2. Civilian noninstitutional population ages 16 and over
#     3. Employment status (employed, unemployed, not in labor force)
#     4. Unemployment rate
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

TBL_EMPSTAT <- bind_rows(

  ## Population ages 16 and over ----
  acs_db_design_p %>%

    # Make universe and identifiers
    mutate(

      # Identifiers
      LINECODE    = 0,
      VARIABLE    = "Employment Status",

      # Universe
      UNIVERSE    = (age >= 16),

      # Categories
      DESCRIPTION = -1) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Total CNP 16 ----
  acs_db_design_p %>%

    # Make universe and identifiers
    mutate(

      # Identifiers
      LINECODE    = 1,
      VARIABLE    = "Employment Status",

      UNIVERSE    = (age >= 16 & empstatd != 14 & empstatd != 15 & gq != 3),

           DESCRIPTION = 0) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Employment status ----
  acs_db_design_p %>%

    # Make universe and identifiers
    mutate(

      # Identifiers
      LINECODE    = 2,
      VARIABLE    = "Employment Status",

      # Universe
      UNIVERSE    = (age >= 16 & empstatd != 14 & empstatd != 15 & gq != 3),

      # Variable categories
      DESCRIPTION = empstat) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Unemployment Rate ----
  acs_db_design_p %>%

    # Make universe and identifiers
    mutate(UNIVERSE = (age >= 16 & empstatd %in% c(10:12, 20) & gq != 3),
           UR       = case_when(empstat == 2 & UNIVERSE == 1 ~ 1,
                                empstat == 1 & UNIVERSE == 1 ~ 0,
                                TRUE ~ NA),
           LINECODE    = 3,
           VARIABLE    = "EmpStat",
           DESCRIPTION = 99) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_mean(UR, na.rm = T))

  )


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# HOUSEHOLD INCOME ----
#
#   Universe: All households
#     Total households
#       Income group
#       Median income
#       Mean income
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

TBL_HHINC <- bind_rows(

  ## Total Households ----
  acs_db_design_h %>%

    # ID variables and universe
    mutate(

      # Identifiers
      LINECODE    = 0,
      VARIABLE    = "HouseholdType",

      # Universe
      UNIVERSE    = pernum == 1 & gq %in% c(1, 2 ,5),

      # Variable categories
      DESCRIPTION = 0) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Households by income bracket ----
  acs_db_design_h %>%
    mutate(

      # Identifiers
      LINECODE = 1,
      VARIABLE = "Household Income",

      # Universe
      UNIVERSE    = pernum == 1 & gq %in% c(1, 2 ,5),
      hhincome    = na_if(hhincome, 9999999),

      # Variable categories
      DESCRIPTION = case_when(
        hhincome  <    10000        ~ 1,   # Less than 10,000
        hhincome %in%  10000:14999  ~ 2,   # 10,000 - 14,999
        hhincome %in%  15000:24999  ~ 3,   # 15,000 - 24,999
        hhincome %in%  25000:34999  ~ 4,   # 25,000 - 34,999
        hhincome %in%  35000:49999  ~ 5,   # 35,000 - 49,999
        hhincome %in%  50000:74999  ~ 6,   # 50,000 - 74,999
        hhincome %in%  75000:99999  ~ 7,   # 75,000 - 99,999
        hhincome %in% 100000:149999 ~ 8,   # 100,000 - 149,999
        hhincome %in% 150000:199999 ~ 9,   # 150,000 - 199,999
        hhincome  >=  200000        ~ 10), # 200,000 or more

      ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Median income ----
  acs_db_design_h %>%

    # ID variables and universe
    mutate(UNIVERSE    = pernum == 1 & gq %in% c(1, 2 ,5),
           hhincome    = na_if(hhincome, 9999999),
           LINECODE    = 0,
           VARIABLE    = "Household Income",
           DESCRIPTION = 98) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_median(hhincome*UNIVERSE, na.rm = T)),

  ## Mean income ----
  acs_db_design_h %>%

    # ID variables and universe
    mutate(UNIVERSE    = pernum == 1 & gq %in% c(1, 2 ,5),
           hhincome    = na_if(hhincome, 9999999),
           LINECODE    = 0,
           VARIABLE    = "Household Income",
           DESCRIPTION = 99) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_mean(hhincome*UNIVERSE, na.rm = T))

  )


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# FAMILY INCOME ----
#
#   This one is tricky... Need to keep only family households in the universe
#   and proceed like household income. See:
#     https://forum.ipums.org/t/families-secondary-families-and-subfamilies/5229/5
#
#   Universe: families
#     Total families
#       Income group
#       Median income
#       Mean income
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

TBL_FAMINC <- bind_rows(

  ## Total families ----
  acs_db_design_h %>%

    # Wage + salary income
    mutate(

      # Identifiers
      LINECODE = 0,
      VARIABLE = "Family Income",

      # Universe
      UNIVERSE = (pernum == 1 & famsize > 1 & cbhhtype %in% c(1, 2, 6, 7, 10, 11)),

      # Recode income
      ftotinc = na_if(ftotinc, 9999999),

      # Variable categories
      DESCRIPTION = 0

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Families by income bracket ----
  acs_db_design_h %>%

    # Wage + salary income
    mutate(

      # Identifiers
      LINECODE = 0,
      VARIABLE = "Family Income",

      # Universe
      UNIVERSE = (pernum == 1 & famsize > 1 & cbhhtype %in% c(1, 2, 6, 7, 10, 11)),

      # Recode income
      ftotinc = na_if(ftotinc, 9999999),

      DESCRIPTION = case_when(
        ftotinc  <    10000        ~ 1,
        ftotinc %in%  10000:14999  ~ 2,
        ftotinc %in%  15000:24999  ~ 3,
        ftotinc %in%  25000:34999  ~ 4,
        ftotinc %in%  35000:49999  ~ 5,
        ftotinc %in%  50000:74999  ~ 6,
        ftotinc %in%  75000:99999  ~ 7,
        ftotinc %in% 100000:149999 ~ 8,
        ftotinc %in% 150000:199999 ~ 9,
        ftotinc  >=  200000        ~ 10,
        is.na(ftotinc)             ~ 11)
    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Median family income ----
  acs_db_design_h %>%

    # Wage + salary income
    mutate(

      # Identifiers
      LINECODE = 98,
      VARIABLE = "Family Income",

      # Universe
      UNIVERSE = (pernum == 1 & famsize > 1 & cbhhtype %in% c(1, 2, 6, 7, 10, 11)),

      # Recode income
      ftotinc = na_if(ftotinc, 9999999),

      # Variable categories
      DESCRIPTION = 98

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW, UNIVERSE) %>%
    summarize(POP = survey_median(ftotinc, na.rm = T), .groups = "drop") %>%
    filter(UNIVERSE == 1) %>%
    select(-UNIVERSE),

  ## Mean family income ----
  acs_db_design_h %>%

    # Wage + salary income
    mutate(

      # Identifiers
      LINECODE = 98,
      VARIABLE = "Family Income",

      # Universe
      UNIVERSE = (pernum == 1 & famsize > 1 & cbhhtype %in% c(1, 2, 6, 7, 10, 11)),

      # Recode income
      ftotinc = na_if(ftotinc, 9999999 | UNIVERSE == 0),

      # Variable categories
      DESCRIPTION = 99

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW, UNIVERSE) %>%
    summarize(POP = survey_mean(ftotinc, na.rm = T), .groups = "drop") %>%
    filter(UNIVERSE == 1) %>%
    select(-UNIVERSE)

)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# POVERTY RATES ----
#
#   Number of persons in poverty by the ratio of income to the poverty level.
#
#   Universe: Persons for whom poverty status can be determined
#     Poverty universe
#       0% to 50% of the poverty level
#       50% to 99% of the poverty level
#       100% to 149% of the poverty level
#       150% t0 199% of the poverty level
#       200% of the poverty level and over
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


TBL_POV <- acs_db_design_p %>%

  # Recode occupation
  mutate(

    # Identifiers
    LINECODE = 0,
    VARIABLE = "Poverty",

    # Universe
    UNIVERSE = (poverty > 0),

    # Recoded occupation
    DESCRIPTION = case_when(
      poverty >    0 & poverty <  50 ~ 1,
      poverty >=  50 & poverty < 100 ~ 2,
      poverty >= 100 & poverty < 150 ~ 3,
      poverty >= 150 & poverty < 200 ~ 4,
      poverty >= 200 ~ 5,
      TRUE ~ 0)

    ) %>%

  # Tabulate the data
  group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
  summarize(POP = survey_total(UNIVERSE))


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# OCCUPATION ----
#
#   Occupations for employed people by Standard Occupation Code (SOC).
#
#   Universe: Civilian employed persons ages 16 and over
#     Civilian employed persons ages 16 and over
#       Management, business, science, and arts
#       Service
#       Sales and office
#       Natural resources, construction, and maintenance
#       Production, transportation, and material moving
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


TBL_OCC <- acs_db_design_p %>%

  # Recode occupation
  mutate(

    # Identifiers
    LINECODE = 0,
    VARIABLE = "Occupation",

    # Universe
    UNIVERSE = (age >= 16 & empstatd %in% 10:12 & gq != 3),

    # Recoded occupation
    DESCRIPTION = case_when(
      as.integer(substr(occsoc, 1, 2)) %in% 11:29 ~ 1,    # Management, business, science, and arts
      as.integer(substr(occsoc, 1, 2)) %in% 31:39 ~ 2,    # Service
      as.integer(substr(occsoc, 1, 2)) %in% 41:43 ~ 3,    # Sales and office
      as.integer(substr(occsoc, 1, 2)) %in% 45:49 ~ 4,    # Natural resources, construction, and maintenance
      as.integer(substr(occsoc, 1, 2)) %in% 51:53 ~ 5,    # Production, transportation, and material moving
      TRUE ~ 0)

  ) %>%

  # Tabulate the data
  group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
  summarize(POP = survey_total(UNIVERSE))


### EOF ###
