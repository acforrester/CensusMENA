

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



if(!file.exists("../out/blog_acs_table.Rda")) {

  # Prep ACS data
  df_svy <- haven::read_stata("./data/usa_00042.dta", col_select = all_of(cols_to_subset$person)) %>%

    # Recode MENA
    mutate(

      MENA_ancestry = 1*(ancestr1 %in% 400:496),

      MENA1 = case_when(
        bpl >= 150 & (ancestr1 %in% 400:496 | bpld %in% mena_countries) ~ 1,
        bpl  < 150 &  ancestr1 %in% 400:496 ~ 2,
        TRUE ~ 0),

      MENA2 =  ifelse(MENA1 > 0, 1, 0)) %>%

    # Keep MENA
    filter(MENA2 == 1) %>%

    # Make age groups
    mutate(agegrp = floor(age/5) + 1,
           agegrp = ifelse(agegrp >= 18, 18, agegrp)) %>%

    # Tabulate the survey data
    group_by(agegrp, sex) %>%
    reframe(pop     = sum(perwt),
            MENA1   = sum(perwt*MENA_ancestry),
            dataset = "Forrester (2023)")

  save(list = "df_svy", file = "../out/blog_acs_table.Rda")

} else {
  load("../out/blog_acs_table.Rda")
}








