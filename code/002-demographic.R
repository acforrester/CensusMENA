#
#   2. Tabulate Demographic Variables
#
#   Produce tables of various demographic variables mirrored from the Data
#   Profile tables from the American Community Survey.
#
#   Use a combination of survey designs for household and person-level
#   variables and the associated rep weights.
#
# *****************************************************************************


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# POPULATION BY AGE AND SEX ----
#
#   Universe: total population
#     Total Population
#       Sex
#         Male
#         Female
#         Sex ratio
#       Age groups
#         0-4 years
#         ...
#         85 years and over
#       Median Age
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


TBL_AGESEX <- bind_rows(

  ## Total population ----
  acs_db_design_p %>%

    # ID variables
    mutate(LINECODE    = 0,
           VARIABLE    = "AgeSex",
           DESCRIPTION = 0) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total()),

  ## Population by sex ----
  acs_db_design_p %>%

    # Age Group
    mutate(LINECODE    = 1,
           VARIABLE    = "AgeSex",
           DESCRIPTION = sex

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total()),

  ## Sex ratio ----
  acs_db_design_p %>%

    # Age Group
    mutate(sex         = ifelse(sex == 2, 0 , 1),
           LINECODE    = 1,
           VARIABLE    = "AgeSex",
           DESCRIPTION = 999

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_mean(sex)),


  ## Population by age ----
  acs_db_design_p %>%

    # Age Group
    mutate(AGEGRP = floor(age/5) + 1,
           AGEGRP = ifelse(AGEGRP >= 18, 18, AGEGRP),

           LINECODE    = 3,
           VARIABLE    = "AgeSex",
           DESCRIPTION = AGEGRP

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total()),



  ## Median age ----
  acs_db_design_p %>%

    # Age Group
    mutate(LINECODE    = 4,
           VARIABLE    = "AgeSex",
           DESCRIPTION = 999

    ) %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_median(age))

)





# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# POPULATION BY NATIVITY ----
#
#   Universe: total population
#     Native Born
#     Foreign-Born
#       Naturalized citizen
#       Noncitizen
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



TBL_NAT <- bind_rows(

  ## Native and Foreign-Born ----
  acs_db_design_p %>%

    # Make ID variables and universe
    mutate(DESCRIPTION = case_when(citizen %in% 0:1 ~ 1,
                                   citizen %in% 2:3 ~ 2),
           LINECODE = 0,
           VARIABLE = "Citizenship") %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total()),

  ## Citizen and Noncitizen
  acs_db_design_p %>%

    # Make ID variables and universe
    mutate(UNIVERSE    = citizen %in% 2:3,
           DESCRIPTION = case_when(citizen  ==    2 ~ 1,
                                   citizen  ==    3 ~ 2),
           LINECODE    = 1,
           VARIABLE    = "Citizenship") %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE))

)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EDUCATIONAL ATTAINMENT ----
#
#   Universe: Population ages 25 and over
#     Population ages 25 and over
#       Attainment by level
#         1 = Less than 9th grade
#         2 = 9th to 12 grade, no diploma
#         3 = High school graduate (incl. equivalency)
#         4 = Some college, no degree
#         5 = Associate's degree
#         6 =
#         7 =
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

TBL_EDUC <- bind_rows(

  ## Total population ages 25 and over ----
  acs_db_design_p %>%

    # ID variables and universe
    mutate(UNIVERSE    = age >= 25,
           DESCRIPTION = -1,
           LINECODE    = 0,
           VARIABLE    = "Education") %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE)),

  ## Educational attainment by level ----
  acs_db_design_p %>%

    # ID variables and universe
    mutate(UNIVERSE    = age >= 25,
           DESCRIPTION = case_when(educd %in% 2:26    ~ 1, # "Less than 9th grade"
                                   educd %in% 30:61   ~ 2, # 9th to 12 grade, no diploma
                                   educd %in% 52:64   ~ 3, # High school graduate (incl. equivalency)
                                   educd %in% 65:71   ~ 4, # Some college, no degree
                                   educd %in% 81      ~ 5, # Associate's degree
                                   educd %in% 101     ~ 6, # Bachelor's degree
                                   educd %in% 114:116 ~ 7, # Graduate or professional degree
                                   TRUE ~ 0),
           LINECODE = 1,
           VARIABLE = "Education") %>%

    # Tabulate the data
    group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
    summarize(POP = survey_total(UNIVERSE))

  )

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ENGLISH LANGUAGE ABILITY ----
#
#   Universe: Population ages 5 and over
#     Population ages 5 and over
#       Does not Speak English
#       Yes, speaks English
#         Only English
#         Very well
#         Well
#         Not very well
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


TBL_SPEAKENG <- acs_db_design_p %>%

  # Universe
  mutate(UNIVERSE    = age >= 5,
         LINECODE    = 0,
         VARIABLE    = "Language",
         DESCRIPTION = speakeng) %>%

  # Tabulate the data
  group_by(LINECODE, VARIABLE, DESCRIPTION, RACENEW) %>%
  summarize(POP = survey_total(UNIVERSE))


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# MARITAL STATUS (UNUSED) ----
#
#   Universe: Ages 15 and older
#
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


acs_db_design_p %>%

  # Universe
  mutate(UNIVERSE = age >= 15,
         LINECODE = 0,
         VARIABLE = paste("MartialStatus", sex)) %>%

  # Tabulate the data
  group_by(RACENEW, marst) %>%
  summarize(POP = survey_total(UNIVERSE))


### EOF ###
