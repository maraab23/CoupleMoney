# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Content ----

# This script loads the pairfam data, joins data from different waves, and 
# creates the files required as input for the waffle plots and for producing 
# descriptive tables

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Load and download (if necessary) required packages ----

# use (and install if necessary) pacman package 
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course
    
pacman::p_load(
  glue,      # helper function for building strings swiftly
  haven,     # import pairfam Stata files
  here,      # enables easy file referencing in project-oriented workflows
  Hmisc,     # used to compute weighted means
  MESS,      # Rounds values to percentages ensuring that they add up to 100
  tidyverse, # universal toolkit for data wrangling and plotting
  scales     # also used for transforming numeric values into percentages
  )


# clear the environment
rm(list = ls())


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Append all files with information on money pooling to one long file ----

# list .dta files in data folder here("data", "pairfam")
files <- list.files(here("data", "pairfam"), 
                    pattern = "*.dta")


data_long <- map(here("data", "pairfam", files),
                 ~read_dta(.x, 
                           col_select = c(id, wave, age, cohort, sex_gen, 
                                          relstat, cd1weight,
                                          school, east, nkidsalv,
                                          starts_with("inc19i")))) |>
  bind_rows() |>
  arrange(id, wave)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define comparison groups ----

# for our cohort comparison we only require information from 
# specific wave-cohort constellations; for one comparison the respondents are 
# in their twenties (group_young) and for the other they are in their 
# thirties (group_old); Cohort 1 and 3 each appear only in one comparison 
# scenario, whereas cohort 2 appears in two (also allowing for a within
# cohort comparison of different age groups)


data_long <- data_long |>
  filter((cohort == 1 & wave == 9) | cohort == 2 | (cohort == 3 & wave == 1)) |> 
  mutate(group_young = (cohort == 2 & wave == 1) | (cohort == 1 & wave == 9),
         group_old = (cohort == 3 & wave == 1) | (cohort == 2 & wave == 9),
         across(starts_with("group_"), as.integer))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age harmonization for cohort comparisons ----

# To compare like with like, we only keep cases that are in the common 
# age range shared by the cohorts that constitute the comparison 

aux <-  data_long |> 
  filter((cohort == 3 & wave == 1) | (cohort == 2 & wave == 9)) |> 
  select(cohort, age)

common.age.old <- intersect(aux$age[aux$cohort==2], 
                            aux$age[aux$cohort==3]) |> 
  sort()


aux <- data_long |> 
  filter((cohort == 2 & wave == 1) | (cohort == 1 & wave == 9)) |>
  select(cohort, age)
  
common.age.young  <- intersect(aux$age[aux$cohort==1], 
                               aux$age[aux$cohort==2]) |> 
  sort()


data_long <- data_long |> 
  mutate(age_drop = (age %in% common.age.old & group_old == 1) | 
           (age %in% common.age.young & group_young == 1),
         age_drop = as.integer(!age_drop)) 
  
# save information on dropped cases due to age harmonization  
age_drop <- data_long |> 
  count(age_drop) |> 
  mutate(total = sum(n),
         share = n / sum(n),
         n_new = total - n,
         share = label_percent(accuracy = .1)(share)) |> 
  filter(age_drop == 1) 

data_long <- data_long |> 
  filter(age_drop != 1) |> 
  select(-age_drop)


reporting <- list(step_01 = age_drop)  
rm(age_drop)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Generate simplified partnership status indicator ----

data_long <- data_long |> 
  mutate(pstatus = case_match(relstat,
                              c(1,6,9)   ~ "Single",
                              c(2,7,10)  ~ "Living apart together",
                              c(3,8,11)  ~ "Cohabitation",
                              c(4,5)     ~ "Marriage", # includes a few non-cohabiting married persons
                              .default  = NA) |> 
           factor(levels = c("Single", "Living apart together", 
                             "Cohabitation", "Marriage")))
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Drop cases with missing on partnership status ----
pstatus_miss <- data_long |> 
  count(pstatus) |> 
  mutate(total = sum(n),
         share = n / sum(n),
         n_new = total - n,
         share = label_percent(accuracy = .1)(share)) |> 
  filter(is.na(pstatus)) 

reporting$step_02 <- pstatus_miss
rm(pstatus_miss)


data_long <- data_long |> 
  filter(!is.na(pstatus))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Drop singles ----

# Analyzing persons in partnerships we drop singles
# we keep record of the distribution of partnership states for the 
# supplement (see reporting$step_03)

pstatus_drop <- data_long |> 
  count(wave, cohort, pstatus) |> 
  mutate(total = sum(n),
         share = n / sum(n),
         n_new = total - n,
         percentage = label_percent(accuracy = .1)(share),
         .by =c(wave, cohort))

reporting$step_03 <- pstatus_drop
rm(pstatus_drop)

data_long <- data_long |> 
  filter(pstatus != "Single") |> 
  mutate(pstatus = fct_drop(pstatus))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# drop cases with missing information on financial arrangement ----


# count occurrences of -1 and -2 in the variables inc19i1 to inc19i7
# and save it in a new "missing" indicator
data_long <- data_long |> 
  mutate(inc19_miss = rowSums(across(starts_with("inc19i"), 
                                     ~.x %in% c(-1, -2, -4)))) |> 
  mutate(inc19_miss = ifelse(inc19_miss == 0, 0, 1),
         cplcash_miss = ifelse(inc19i7 == 1 | inc19_miss == 1, 1, 0),
         cplcash_miss = replace_na(cplcash_miss,0)) 

cplcash_miss <- data_long |> 
  count(cplcash_miss) |> 
  mutate(total = sum(n),
         share = n / sum(n),
         n_new = total - n,
         share = label_percent(accuracy = .1)(share)) |> 
  rename(n_miss = n) |> 
  filter(cplcash_miss == 1) 

reporting$step_04 <- cplcash_miss
rm(cplcash_miss)

# drop the cases with missings on key variable
data_long <- data_long |> 
  filter(cplcash_miss != 1) |> 
  rename(aux_inc19i7 = inc19i7)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Build key indicator: couples' money arrangement ----

data_long <- data_long |>
  mutate(aux = rowSums(across(starts_with("inc19i"),
                              ~.x == 1)),
         aux2 = rowSums(across(c(inc19i1, inc19i2, inc19i3),
                               ~.x == 1)),
         cplcash = case_when(
           inc19i1 == 1 & aux == 1 ~ "Put all money together",
           (inc19i1 == 1 & aux > 1) | inc19i2 == 1 | inc19i3 == 1 ~ "Put some money together",
           (inc19i4 == 1 | inc19i5 == 1) & aux2 == 0 ~ "Keep all money separate",
           .default = NA_character_),
         cplcash = factor(cplcash,
                          levels = c("Put all money together",
                                     "Put some money together",
                                     "Keep all money separate"))
         ) |>
  select(-aux, -aux2) |> 
  mutate(
    wave = as_factor(wave), 
    wave = str_remove(wave, "^\\d\\s")
    )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Addition for revise and resubmit: parental status ----

data_long <- data_long |> 
  mutate(parent = case_match(nkidsalv,
                             0 ~ 0,
                             1:20 ~ 1,
                             .default = NA))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Create the data for waffle/tile plots ----
# Cohort comparisons: wave 1 and 9


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Split data by cohort-comparison groups
#   - Cohort 72 and 82: mid 30s at time of observation
#   - Cohort 82 and 92: mid 20s at time of observation

splitted <- data_long |> 
  group_by(group_old) |> 
  group_split()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# inspection of average age by cohort (approx 1 year difference)
map(splitted,
    ~.x |> 
      summarise(mage = wtd.mean(age, weights = cd1weight), .by = cohort))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# age labels for facet plot
age.lower.panel <- paste(range(common.age.young)[1], 
                         range(common.age.young)[2], 
                         sep = "\u2013")  

age.upper.panel <- paste(range(common.age.old)[1], 
                         range(common.age.old)[2], 
                         sep = "\u2013")  

age.panels <- list(age.lower.panel,
                   age.upper.panel)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare data for waffle plots
splitted.to.plot <- map(splitted,
                        ~ .x |> 
                          count(cohort, wave, cplcash, pstatus, wt = cd1weight, .drop = F) %>% 
                          # note usage of magrittr's pipe (this spares us from using \(x) notation)
                          bind_rows(
                            summarise(., pstatus = "Total", n = sum(n), .by=c(cohort, wave, cplcash))
                          ) |> 
                          mutate(share = n / sum(n),
                                 share = round_percent(100*share),
                                 cohort = as_factor(cohort), 
                                 cohort = str_remove_all(cohort, "^\\d "), 
                                 .by = c(cohort, pstatus)) |> 
                          mutate(pstatus = factor(pstatus,
                                                  levels = c("Total", "Living apart together", 
                                                             "Cohabitation","Marriage"))) |> 
                          arrange(pstatus, cohort, cplcash) |> 
                          mutate(y = cumsum(share),
                                 y = ifelse(y %% 10 == 0, y - 1, y),
                                 y = ceiling(y/10),
                                 .by = c(cohort, pstatus)) |> 
                          select(-n))
    

# Extract case number for each group
casenumber <- map2(splitted, age.panels,
     ~.x |> 
       select(id, age, cohort) |> 
       mutate(age = .y)) |> 
  bind_rows() |> 
  mutate(cohort = as_factor(cohort), 
         cohort = str_remove_all(cohort, "^\\d "), 
         cohort = glue("Cohort<br>{cohort}    "),
         age = glue("Age {age}"),
         age = factor(age)) |> 
  count(cohort, age)


# unite data and add case numbers
facetplotdata <- map2(splitted.to.plot, age.panels,
     ~.x |>
       mutate(age = .y)) |> 
  bind_rows() |> 
  mutate(cohort = glue("Cohort<br>{cohort}    "),
         wave = glue("Year {wave}"),
         age = glue("Age {age}"),
         age = factor(age))


facetplotdata <- facetplotdata |> 
  full_join(
    casenumber,
    by = join_by(cohort, age)
  ) |> 
  mutate(age = glue("{age}<br>*(n={n})*"),
         age = factor(age),
         pstatus = fct_relevel(pstatus, "Total", after = 3),
         cohort = str_replace(cohort, "-", "\u2013")) |> 
  select(-n)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create tibble for labels for geom_text
facetplot.txt.data <- facetplotdata |> 
  mutate(label = glue("{share}%")) |> 
  select(-share) |> 
  filter(y != 0) |> 
  mutate(aux = ifelse(lag(y) == y, 1, 0) |> replace_na(0), 
         .by = c(cohort, age, pstatus)) |> 
  mutate(x = ifelse(aux == 1, 1.64, .64)) |> 
  select(-aux)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Repeat the excercise for partnership duistributions 

pstatus.to.plot <- map2(splitted, age.panels,
     ~ .x |> 
       count(cohort, wave, pstatus, wt = cd1weight, .drop = F) |> 
       mutate(age = .y)) |> 
  bind_rows() |> 
  mutate(share = n/sum(n), 
         share = round_percent(100*share),
         .by = c(cohort, age)) |> 
  mutate(cohort = as_factor(cohort), 
         cohort = str_remove_all(cohort, "^\\d "),
         cohort = glue("Cohort<br>{cohort}    "),
         pstatus = fct_rev(pstatus),
         age = glue("Age {age}"),
         age = factor(age),
         wave = glue("Year {wave}")) |> 
  arrange(cohort, age) |> 
  mutate(y = cumsum(share),
         y = ifelse(y %% 10 == 0, y - 1, y),
         y = ceiling(y/10),
         .by = c(cohort, age)) 



pstatus.to.plot <- pstatus.to.plot |> 
  full_join(
    rename(casenumber, cases = n),
    by = join_by(cohort, age)
  ) |> 
  mutate(age = glue("{age}<br>{wave}<br>*n = {scales::comma(cases)}*"),
         age = factor(age)) |> 
  select(-cases) |> 
  mutate(colvar = "Partnership status",
         cohort = str_replace(cohort, "-", "\u2013"))


pstatus.txt.data <- pstatus.to.plot |> 
  mutate(label = glue("{share}%")) |> 
  select(-share) |> 
  filter(y != 0) |> 
  mutate(aux = ifelse(lag(y) == y, 1, 0) |> replace_na(0), 
         .by = c(cohort, age)) |> 
  mutate(x = ifelse(aux == 1, 1.64, .64)) |> 
  select(-aux)


pstatus.to.plot <- pstatus.to.plot |> 
  uncount(share) |> 
  mutate(x = rep(1:10, n()/10),
         y = rep(
           rep(1:10, each = 10), n()/100))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# final step: expand data for waffle plot
facetplotdata <- facetplotdata |> 
  uncount(share) |> #print(n=200)
  ungroup() %>%
  mutate(x = rep(1:10, n()/10),
         y = rep(
           rep(1:10, each = 10), n()/100))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save data/environment ----


# remove unnecessary objects
rm(list=setdiff(ls(), 
                c("data_long", "splitted", "age.panels",
                  "facetplotdata", "facetplot.txt.data",
                  "pstatus.to.plot", "pstatus.txt.data",
                  "reporting")))


# save environment 
save.image(
  file= here("data", "posted", 
             "01_cplcash.RData"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



