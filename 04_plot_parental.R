# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~
# Addition for R&R ------
# Plot by parental status
# ~~~~~~~~~~~~~~~~~~~~~~~

    # We were asked to inspect the money arrangements by parental status
    # We implemented this with a binary split: parents vs non parents
    # For persons in LAT relationships we have too few respondents with children  
    # for a meaningful analysis -> we dropped them for this exercise
    # the same holds for the young respondents of the 1991 birth cohort


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load and download (if necessary) required packages ----

# use (and install if necessary) pacman package 
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course

pacman::p_load(
  colorspace, # for defining the fill colors
  ggh4x,      # for nested facet plot
  ggtext,     # formatting the axis text
  grid,       # to add lines to the patchwork plot
  here,       # enables easy file referencing in project-oriented workflows
  patchwork,  # for plot composition
  tidyverse,  # universal toolkit for data wrangling and plotting
  scales,     # also used for transforming numeric values into percentages
  showtext    # allows to use of google fonts
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

# add font for plot ----
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Prepare data for plot I: ----
# money arrangements by cohort, age, partnership, and parental status


# add parenting status as a new grouping criterion
splitted.parental <- map(splitted,
                         ~ .x |> 
                           filter(!is.na(parent)) |> 
                           count(cohort, wave, cplcash, parent, pstatus, 
                                 wt = cd1weight, .drop = F) %>% 
                           # note usage of magrittr's pipe
                           # (this spares us from using \(x) notation)
                           bind_rows(
                             summarise(., pstatus = "Total", n = sum(n), 
                                       .by=c(cohort, wave, cplcash, parent))
                           ) |> 
                           mutate(share = n / sum(n),
                                  share = round_percent(100*share),
                                  cohort = as_factor(cohort), 
                                  cohort = str_remove_all(cohort, "^\\d "), 
                                  .by = c(cohort, pstatus, parent)) |> 
                           mutate(pstatus = factor(
                             pstatus,
                             levels = c("Total", "Living apart together", 
                                        "Cohabitation","Marriage"))) |> 
                           arrange(pstatus, cohort, parent, cplcash) |> 
                           mutate(y = cumsum(share),
                                  y = ifelse(y %% 10 == 0, y - 1, y),
                                  y = ceiling(y/10),
                                  .by = c(cohort, pstatus, parent)) |> 
                           select(-n))
                            

# unite data and apply some further adjustments for the plot 
facetplotdata.parental <- map2(splitted.parental, age.panels,
                               ~.x |>
                                 mutate(age = .y)) |> 
  bind_rows() |> 
  mutate(cohort = glue("Cohort<br>{cohort}    "),
         wave = glue("Year {wave}"),
         age = glue("Age {age}"),
         age = factor(age)) |> 
  filter(pstatus %in% c("Cohabitation", "Marriage")) |> 
  filter(!str_detect(cohort, "1991-1993")) |> 
  mutate(parent  = ifelse(parent == 0, "Not Parenting", "Parenting"),
         parent  = factor(parent),
         cohort = str_replace(cohort, "-", "\u2013"))


# create tibble for labels for geom_text
facetplot.parental.txt.data <- facetplotdata.parental |> 
  mutate(label = glue("{share}%")) |> 
  select(-share) |> 
  filter(y != 0) |> 
  mutate(aux = ifelse(lag(y) == y, 1, 0) |> replace_na(0), 
         .by = c(cohort, age, pstatus)) |> 
  mutate(x = ifelse(aux == 1, 1.64, .64)) |> 
  select(-aux)


# final step: expand data for waffle plot
facetplotdata.parental <- facetplotdata.parental |> 
  uncount(share) |> #print(n=200)
  ungroup() %>%
  mutate(x = rep(1:10, n()/10),
         y = rep(
           rep(1:10, each = 10), n()/100))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prepare data II: ----
# joint distribution of parental & partnership status by cohort and age

ppstatus.to.plot <- map2(splitted, age.panels,
                         ~ .x |>
                           filter(!is.na(parent)) |> 
                           filter(cohort != 1) |> 
                           filter(pstatus %in% c("Cohabitation", "Marriage")) |>
                           count(cohort, wave, pstatus, parent, wt = cd1weight) |> 
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
         wave = glue("Year {wave}"),
         parent2  = ifelse(parent == 0, "Not Parenting", "Parenting"),
         #cohort = str_replace(cohort, "-", "\u2013")
         group = paste0(pstatus, ",<br>", parent2),
         group = factor(group)) |> 
  arrange(cohort, age) |> 
  mutate(y = cumsum(share),
         y = ifelse(y %% 10 == 0, y - 1, y),
         y = ceiling(y/10),
         .by = c(cohort, age)) 
                       

# Cohort and age-specific case numbers
casenumbers.pp <- map2(splitted, age.panels,
                             ~.x |>
                               select(id, age, cohort, parent, pstatus) |> 
                               mutate(age = .y) |> 
                               drop_na(parent)) |> 
  bind_rows() |> 
  mutate(cohort = as_factor(cohort), 
         cohort = str_remove_all(cohort, "^\\d "), 
         cohort = glue("Cohort<br>{cohort}    "),
         age = glue("Age {age}"),
         age = factor(age)) |> 
  filter(pstatus %in% c("Cohabitation", "Marriage")) |> 
  filter(!str_detect(cohort, "1991-1993")) |> 
  count(cohort, age)


# add case numbers to the plot data
ppstatus.to.plot <- ppstatus.to.plot |> 
  full_join(
    rename(casenumbers.pp, cases = n),
    by = join_by(cohort, age)
  ) |> 
  mutate(age = glue("{age}<br>{wave}<br>*n = {scales::comma(cases)}*"),
         age = factor(age)) |> 
  select(-cases) |> 
  mutate(colvar = "Partnership and Parenthood",
         cohort = str_replace(cohort, "-", "\u2013"))


# create tibble for labels for geom_text
ppstatus.txt.data <- ppstatus.to.plot |> 
  mutate(label = glue("{share}%")) |> 
  select(-share) |> 
  filter(y != 0) |> 
  mutate(aux = ifelse(lag(y) == y, 1, 0) |> replace_na(0), 
         .by = c(cohort, age)) |> 
  mutate(parent2  = ifelse(parent == 0, "Not Parenting", "Parenting"),
         group = paste0(pstatus, ",<br>", parent2),
         x = ifelse(aux == 1, 1.64, .64)) |> 
  select(-aux)


# final step: expand data for waffle plot
ppstatus.to.plot <- ppstatus.to.plot |> 
  uncount(share) |> 
  mutate(x = rep(1:10, n()/10),
         y = rep(
           rep(1:10, each = 10), n()/100))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Produce the plots ----

# Sub plot with the money arrangements 
pcash.plot <- facetplotdata.parental |>
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill=cplcash), color="white", linewidth=0.5) +
  geom_text(data = facetplot.parental.txt.data,
            aes(x = x , y = y, label = label),
            fontface ="bold",
            family = "Roboto Condensed",
            size = 5,
            hjust = 0) +
  facet_nested(rows = vars(cohort, fct_rev(age)),
               cols = vars(pstatus, parent),
               switch = "y") +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_fill_manual(values = sequential_hcl(3, palette = "OrYel")) +
  theme_void(base_family = "Roboto Condensed") +
  theme(strip.text.x = element_text(size = 22, face = "bold",
                                    family = "Roboto Condensed",
                                    margin = margin(2,0,2,0)),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.spacing.x = unit(.3, 'in'),
        legend.key.size = unit(.3, "in"),
        plot.margin = margin(0))


# sub plot with distribution of partnership & parental states 
ppartner.plot <- ppstatus.to.plot |> 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill=group), color="white", linewidth=0.5) +
  geom_text(data = ppstatus.txt.data,
            aes(x = x , y = y, label = label),
            fontface ="bold",
            family = "Roboto Condensed",
            size = 5,
            hjust = 0) +
  facet_nested(rows = vars(cohort, fct_rev(age)),
               cols = vars(colvar),
               switch = "y") +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_fill_manual(values = sequential_hcl(4,
                                            palette = "Sunset",
                                            rev = T) |> lighten(.25),
                    guide = guide_legend(reverse = TRUE)) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    strip.text.x = element_text(size = 22, face = "bold",
                                family = "Roboto Condensed",
                                margin = margin(2,0,2,0)),
    strip.text.y.left =  element_markdown(size = 22, face = "bold",
                                          family = "Roboto Condensed", angle = 0,
                                          lineheight = 1.2,
                                          margin = margin(2,0,2,0),
                                          hjust = 0),
    legend.position = "bottom",
    legend.location = "plot",
    legend.margin = margin(l=0),
    legend.text = element_markdown(size = 18,
                                   family = "Roboto Condensed"),
    legend.spacing.x = unit(.3, 'in'),
    legend.key.size = unit(.3, "in"),
    plot.background = element_part_rect(side = "r",
                                        colour = "black",
                                        linewidth = 1,
                                        linetype = "solid"),
    plot.margin = margin(0))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compose the final plot ----

ptitle <- paste("Cohort and Age Group Differences in Partnership",
                "and Financial Arrangements Among Couples in Germany")


the.plot <- ppartner.plot + pcash.plot + plot_layout(widths = c(1, 4)) +
  plot_annotation(
    title = ptitle,
    theme = theme(
      plot.title = element_text(size = 38, 
                                margin = margin(5,0,5,0),
                                family = "Roboto Condensed",
                                face = "bold")
      )
    )

# Step 2: Convert the ggplot object to a grob
p_grob <- patchworkGrob(the.plot)

# Step 3: Create additional elements using grid functions
hline <- linesGrob(x = unit(c(0.01, .99), "npc"), 
                   y = unit(c(0.608, 0.608), "npc"))

# Step 4: Combine the ggplot grob and the additional elements into a single grob
the.plot <- grobTree(p_grob, hline)

# Step 5: Save the combined grob using ggsave
showtext_opts(dpi = 300)  
ggsave(here("plots", "the_plot_parental.png"), 
       the.plot, width = 23, height = 12, dpi = 300,
       bg = "white")

ggsave(here("plots", "the_plot_parental.svg"), 
       the.plot, width = 23, height = 12, dpi = 300,
       bg = "white")

showtext_opts(dpi = 96) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
