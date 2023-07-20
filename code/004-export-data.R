#
#   04. Export Survey Tabulations
#
#     Export the data to a CSV file. Convert standard errors into 90% margins
#     of error. Additional formatting for the publication is done in Excel for
#     web publishing.
#
# *****************************************************************************

TBL_COMBINED <- bind_rows(
  TBL_AGESEX,
  TBL_EDUC,
  TBL_NAT,
  TBL_SPEAKENG,
  TBL_EMPSTAT,
  TBL_HHINC,
  TBL_FAMINC,
  TBL_POV,
  TBL_OCC)

# Clipboard for Mac
clip <- pipe("pbcopy", "w")

# Write the table
TBL_COMBINED %>%

  # SE to 90% MOE
  mutate(POP_se = 1.645*POP_se) %>%

  # Place new categories in columns
  pivot_wider(names_from  = "RACENEW",
              values_from = c(POP, POP_se)) %>%

  # Organize data for export
  relocate(LINECODE, VARIABLE, DESCRIPTION,
           contains("0"),
           contains("1"),
           contains("2"),
           contains("3"),
           contains("4"),
           contains("5"),
           contains("6"),
           contains("7")) %>%

  # Write data
  write.table(., file = clip, sep = '\t', row.names = FALSE)

close(clip)

### EOF ###
