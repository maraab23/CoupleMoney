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

# Facet plot for couples' money arrangements ----
# ...for different cohorts and age groups and by partnership status

cash.plot <- facetplotdata |>
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill=cplcash), color="white", linewidth=0.5) +
  geom_text(data = facetplot.txt.data,
            aes(x = x , y = y, label = label),
            fontface ="bold",
            family = "Roboto Condensed",
            size = 5,
            hjust = 0) +
  facet_nested(rows = vars(cohort, fct_rev(age)),
               cols = vars(pstatus),
               switch = "y") +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_fill_manual(values = sequential_hcl(3, palette = "OrYel")) +
  
  theme_void(base_family = "Roboto Condensed") +
  theme(strip.text.x = element_text(size = 22, face = "bold",
                                    family = "Roboto Condensed",
                                    margin = margin(2,0,2,0)),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.spacing.x = unit(.3, 'in'),
        legend.key.size = unit(.3, "in"),
        plot.margin = margin(0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Waffle plot showing distribution of partnership states ----
# ... for different cohorts and age groups

partner.plot <- pstatus.to.plot |> 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill=pstatus), color="white", linewidth=0.5) +
  geom_text(data = pstatus.txt.data,
            aes(x = x , y = y, label = label),
            fontface ="bold",
            family = "Roboto Condensed",
            size = 5,
            hjust = 0) +
  facet_nested(rows = vars(cohort, fct_rev(age)),
               cols = vars(colvar),
               switch = "y") +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_fill_manual(values = sequential_hcl(11, 
                                            palette = "Sunset", 
                                            rev = F)[c(2,5,8)] |> lighten(.3),
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
    legend.text = element_text(size = 20),
    legend.spacing.x = unit(.3, 'in'),
    legend.key.size = unit(.3, "in"),
    plot.background = element_part_rect(side = "r",
                                        colour = "black",
                                        linewidth = 1,
                                        linetype = "solid"),
    plot.margin = margin(0))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Composing the final plot ----


# Step 1: Put the patches together
ptitle <- paste("Cohort and Age Group Differences in Partnership",
                "and Financial Arrangements Among Couples in Germany")


the.plot <- partner.plot + cash.plot + plot_layout(widths = c(1, 4)) +
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
                   y = unit(c(0.71, 0.71), "npc"))
hline2 <- linesGrob(x = unit(c(0.01, .99), "npc"), 
                    y = unit(c(0.253, 0.253), "npc"))

# Step 4: Combine the ggplot grob and the additional elements into a single grob
the.plot <- grobTree(p_grob, hline, hline2)

# Step 5: Save the combined grob using ggsave
showtext_opts(dpi = 300)  
ggsave(here("plots", "the_plot.png"), 
       the.plot, width = 23, height = 16, dpi = 300,
       bg = "white")

ggsave(here("plots", "the_plot.svg"), 
       the.plot, width = 23, height = 16, dpi = 300,
       bg = "white")

showtext_opts(dpi = 96) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

