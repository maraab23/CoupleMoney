# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load and download (if necessary) required packages ----

# use (and install if necessary) pacman package 
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course

pacman::p_load(
  glue,      # helper function for building strings swiftly
  gt,         # Easily Create Presentation-Ready Display Tables
  gtsummary,  # allows to create tables with weighted data and adjusted tests
  haven,      # for converting labelled data to factors
  here,       # enables easy file referencing in project-oriented workflows
  tidyverse,  # universal toolkit for data wrangling and plotting
  srvyr       # takes care of weights, needed for gtsummary 
)


# clear the environment
rm(list = ls())


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data ----

load(here("data", "posted", 
          "01_cplcash.RData"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Distribution of partnership states ----

# Prepare data for table 
data <- map2(splitted, age.panels,
             ~ .x |> 
               select(id, age, cohort, cd1weight, pstatus) |> 
               mutate(age = .y,
                      age = glue("Age {age}"),
                      cohort = as_factor(cohort), 
                      cohort = str_remove_all(cohort, "^\\d "))
             ) |> 
  bind_rows() |>
  arrange(cohort)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define data as survey data
pstatus_svy <- data |> 
  as_survey_design(id = id,
                   weights = cd1weight)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Generate table with gtsummary
pstatus_tbl <- pstatus_svy |>
  tbl_strata(strata = cohort,~.x |>
               tbl_svysummary(by = age, 
                              include= pstatus,
                              statistic = all_categorical() ~ "{p}%<br>{n} [{n_unweighted}]",
                              digits = all_categorical() ~ list(0, 0, 0, style_percent),
               )) |> 
  modify_header(all_stat_cols() ~ "**{level}**",
                label ~ "**Partnership status**")  |> 
  modify_footnote(everything() ~ NA) 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare data for conversion to gt table
pstatus_tbl$table_body <- pstatus_tbl$table_body |> 
  slice(-1)
  

# convert to gt table
pstatus_tbl <- as_gt(pstatus_tbl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# produce gt table
pstatus_tbl <- pstatus_tbl |> 
  tab_header(title = md("**Distribution of partnership states across cohorts and age groups**"),
             subtitle = "Table shows weighted age-specific column percentages and 
             case numbers. Unweighted case numbers in brackets.") |> 
  opt_align_table_header(align = "left") |> 
  fmt_markdown(columns = c(label, starts_with("stat_"))) |> 
  tab_style(style = cell_borders(sides = c("top"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = 1)) |> 
  tab_style(style = cell_borders(sides = c("bottom"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = nrow(pstatus_tbl$`_data`))) |> 
  tab_options(column_labels.font.weight = "bold",
              table.border.top.color = "transparent",
              table.border.bottom.width = px(3),
              table.border.bottom.color = "#8E8E8E",
              heading.border.bottom.width = px(3),
              heading.border.bottom.color = "#8E8E8E",
              heading.padding = px(5)) |> 
  opt_table_font(font = c(google_font(name = "Roboto"), default_fonts()))

pstatus_tbl |>  
  gtsave(here("tables", "tab_1.png"), expand = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Testing cohort differences across age groups ----

# Prepare data for table 
data <- map2(splitted, age.panels,
     ~ .x |> 
       select(id, age, cohort, cd1weight, pstatus, cplcash) |> 
       mutate(age = .y,
              cohort = as_factor(cohort), 
              cohort = str_remove_all(cohort, "^\\d "))
     ) |> 
  bind_rows() |>
  arrange(cohort) |> 
  mutate(age = glue("Age {age}"),
         `Living apart together` = ifelse(pstatus == "Living apart together",
                                          cohort, NA),
         Cohabitation = ifelse(pstatus == "Cohabitation",
                               cohort, NA),
         Marriage = ifelse(pstatus == "Marriage",
                           cohort, NA),
         Total = cohort) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define data as survey data
waffle_svy <- data |> 
  as_survey_design(id = id,
                   weights = cd1weight)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generate table (including tests) with gtsummary
cohort_tbl <- waffle_svy |>
  tbl_strata(strata = age,~.x |>
               tbl_svysummary(by = cplcash,
                              include = c(Total, `Living apart together`,
                                          Cohabitation, Marriage),
                              statistic = all_categorical() ~ "{p}%<br>{n} [{n_unweighted}]",
                              digits = all_categorical() ~ list(0, 0, 0, style_percent),
                              sort = all_categorical() ~ "alphanumeric",
                              percent = "row",
                              missing = "no") |>
               add_p(test = everything() ~ "svy.adj.chisq.test")) |> 
  modify_header(all_stat_cols() ~ "**{level}**",
                label ~ "") |> 
  modify_footnote(everything() ~ NA) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare data for conversion to gt table
cohort_tbl$table_body <- cohort_tbl$table_body |> 
  mutate(idgroup = cur_group_id(), 
         idx = row_number(), 
         .by = variable) |> 
  mutate(idx = case_match(idx, 2~3,3~2, .default = idx)) |> 
  arrange(idgroup, idx) |> 
  mutate(label = ifelse(idx==1, 
                        glue("**{label}**"),
                        glue("&nbsp;&nbsp;&nbsp;&nbsp;{label}")))

# convert to gt table
cohort_tbl <- as_gt(cohort_tbl)
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# produce table
cohort_tbl <- cohort_tbl |>
  tab_header(title = md("**Cohort differences in couples' money arrangements across two age groups**"),
             subtitle = "Table shows weighted row percentages by cohort and 
             case numbers. Unweighted case numbers in brackets.
             p-values from design-adjusted Rao‐Scott chi-square tests.") |> 
  opt_align_table_header(align = "left") |> 
  fmt_markdown(columns = c(label, starts_with("stat_"))) |> 
  tab_style(style = cell_borders(sides = c("top"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = 1)) |> 
  tab_style(style = cell_borders(sides = c("bottom"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = nrow(cohort_tbl$`_data`))) |> 
  tab_options(column_labels.font.weight = "bold",
              table.border.top.color = "transparent",
              table.border.bottom.width = px(3),
              table.border.bottom.color = "#8E8E8E",
              heading.border.bottom.width = px(3),
              heading.border.bottom.color = "#8E8E8E",
              heading.padding = px(5)) |> 
  opt_table_font(font = c(google_font(name = "Roboto"), default_fonts()))


cohort_tbl |>  
  gtsave(here("tables", "tab_2.png"), zoom = 2, expand = 2000)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Testing age group differences in cohort 2 ----

# Prepare data for table 
data2 <- map2(splitted, age.panels,
              ~ .x |> 
                select(id, age, cohort, cd1weight, pstatus, cplcash) |> 
                mutate(age = .y,
                       cohort = as_factor(cohort), 
                       cohort = str_remove_all(cohort, "^\\d "))
              ) |> 
  bind_rows() |>
  arrange(cohort)|> 
  filter(cohort == "1981-1983") |> 
  mutate(`Living apart together` = ifelse(pstatus == "Living apart together",
                                          age, NA),
         Cohabitation = ifelse(pstatus == "Cohabitation",
                               age, NA),
         Marriage = ifelse(pstatus == "Marriage",
                           age, NA),
         Total = age) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define data as survey data
waffle_svy2 <- data2 |> 
  as_survey_design(id = id,
                   weights = cd1weight)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generate table (including tests) with gtsummary
age_coh2_tbl <- waffle_svy2 |> 
  tbl_svysummary(by = cplcash,
                 include = c(Total, `Living apart together`,
                             Cohabitation, Marriage),
                 statistic = all_categorical() ~ "{p}%<br>{n} [{n_unweighted}]",
                 digits = all_categorical() ~ list(0, 0, 0, style_percent),
                 sort = all_categorical() ~ "alphanumeric",
                 percent = "row",
                 missing = "no") |> 
  add_p(test = everything() ~ "svy.adj.chisq.test") |> 
  modify_header(all_stat_cols() ~ "**{level}**") |> 
  modify_footnote(everything() ~ NA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare data for conversion to gt table
age_coh2_tbl$table_body <- age_coh2_tbl$table_body |> 
  mutate(idgroup = cur_group_id(), 
         idx = row_number(), 
         .by = variable) |>
  mutate(label = ifelse(idx==1, 
                        glue("**{label}**"),
                        glue("&nbsp;&nbsp;&nbsp;&nbsp;{label}")))

# convert to gt 
age_coh2_tbl <- as_gt(age_coh2_tbl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# produce table
age_coh2_tbl <- age_coh2_tbl |>
  tab_header(title = md("**Age group differences in couples' money arrangements for the birth cohort 1981-1983**"),
             subtitle = "Table shows weighted row percentages by age group and 
             case numbers. Unweighted case numbers in brackets.
             p-values from design-adjusted Rao‐Scott chi-square tests.") |> 
  opt_align_table_header(align = "left") |> 
  fmt_markdown(columns = c(label, starts_with("stat_"))) |> 
  tab_style(style = cell_borders(sides = c("top"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = 1)) |> 
  tab_style(style = cell_borders(sides = c("bottom"),
                                 color = "#8E8E8E", # "#D3D3D3",
                                 weight = px(3)),
            locations = cells_body(rows = nrow(age_coh2_tbl$`_data`))) |> 
  tab_options(column_labels.font.weight = "bold",
              table.border.top.color = "transparent",
              table.border.bottom.width = px(3),
              table.border.bottom.color = "#8E8E8E",
              heading.border.bottom.width = px(3),
              heading.border.bottom.color = "#8E8E8E",
              heading.padding = px(5)) |> 
  opt_table_font(font = c(google_font(name = "Roboto"), default_fonts()))

age_coh2_tbl  |>  
  gtsave(here("tables", "tab_3.png"), zoom = 2, expand = 2000)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save data/environment ----

# save environment 
save.image(
  file= here("data", "posted", 
             "02_cplcash_plus_tbls.RData"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
