# top keywords and targeted keywords


# 0. setup --------------------------------------------------------------------------
library(here)
library(tidyverse)
library(gganimate)

# 1. data ---------------------------------------------------------------------------
pmpull <- read_rds(here("data/pmpull_2025-06-18.rds"))


# 2. extract keywords from pubmed data ----------------------------------------------

procpmdat <- function(pmpull){

  # split lines, unlist
  pmpull <- map(pmpull, \(x){
    x |>
      str_remove_all("\r") |>
      str_split("\n") |>
      unlist()
  })

  # keep only keywords, put into lower case
  kwds <- map(pmpull, \(x){
    keep <- which(str_detect(x, "^OT(?!O)"))
    kwd <- x[keep] |> str_remove_all("OT  - ") |> str_to_lower()
    return(kwd)
  })

  # tidy data
  kwds <- kwds |>
    unlist() |>
    unname() |>
    tibble() |>
    rename(kwd = 1) |>
    mutate(
      kwd = kwd |>
        str_remove_all("\\*") |>
        str_remove_all("[\"']")
    )
  return(kwds)

}
kwds <- map(pmpull, procpmdat)

# 3. write function to pull top n keywords ------------------------------------------
topkwd <- function(x, top_n){
    x |>
      count(kwd) |>
      arrange(-n) |>
      slice(1:top_n)
}

# test it -- check a bunch of the top keywords to see if there are errors etc
kwds |> map(\(x){topkwd(x, top_n=30)}) |>
  list_rbind() |>
  distinct(kwd) |> print(n=Inf)

# 4. fix common errors --------------------------------------------------------------

# this all comes from a visual inspection of the data, and I did it relatively quickly.
# there are likely errors/duplicates that still exist, but close enough for the current purpose

clean_kwd_errors <- function(x){
  case_when(
    x=="adolescence" ~ "adolescent",
    x=="adolescents" ~ "adolescent",
    x=="alcohol" ~ "alcohol or alcohol use disorder",
    x=="alcohol use disorder" ~ "alcohol or alcohol use disorder",
    x=="problematic alcohol use" ~ "alcohol or alcohol use disorder",
    str_detect(x, "antibiotics") ~ "antibiotics",
    str_detect(x, "audit") ~ "audit",
    str_detect(x, "behaviour change") ~ "behaviour change",
    str_detect(x, "cardiovascular disease") ~ "cardiovascular disease",
    x=="child" ~ "children/youth",
    x=="children" ~ "children/youth",
    x=="youth" ~ "children/youth",
    str_detect(x, "chronic disease") ~ "chronic disease",
    str_detect(x, "clinical practice guideline") ~ "clinical practice guidelines",
    str_detect(x, "clinical trial") ~ "clinical trial",
    str_detect(x, "cluster randomized trial") ~ "cluster randomized trial",
    str_detect(x, "cost-effectiveness") ~ "cost-effectiveness analysis",
    str_detect(x, "covid-19") ~ "covid-19",
    x=="sars-cov-2" ~ "covid-19",
    x == "e-cigarettes" ~ "electronic cigarettes",
    str_detect(x, "guideline") ~ "guidelines",
    x=="health equity" ~ "health equity/inequity/disparities",
    x=="health inequities" ~ "health equity/inequity/disparities",
    x=="health disparities" ~ "health equity/inequity/disparities",
    str_detect(x, "health services") ~ "health services",
    str_detect(x, "implementation") ~ "implementation",
    str_detect(x, "intervention") ~ "interventions",
    str_detect(x, "pragmatic trial") ~ "pragmatic trials",
    x=="qualitative" ~ "qualitative research",
    str_detect(x, "randomized controlled trial") ~ "randomized controlled trials",
    str_detect(x, "reporting guideline") ~ "reporting guidelines",
    x=="research methods" ~ "research methodology",
    x=="statistics &amp; research methods" ~ "statistical methods",
    str_detect(x, "suicid") ~ "suicide",
    x=="systematic review" ~ "systematic reviews",
    x=="telemedicine" ~ "telehealth",
    TRUE ~ x
  )
}
kwds_clean <- map(kwds, \(x){x |> mutate(kwd = clean_kwd_errors(kwd))})

# 5. pull top n cleaned keywords ----------------------------------------------------

kwd20 <- kwds_clean |> map(\(x){topkwd(x, 20)})


# 6. race chart, top 20 keywords, each year ----------------------------------

tmp <- imap(kwd20[-1], ~mutate(.x, year=.y)) |> # first list element is last five years, we want each year
  list_rbind() |>
  # take name of list element (which was labeled based on year and convert to a year variable)
  mutate(year = paste0(20, str_remove_all(year, "y")) |> as.numeric()) |>
  # rank keywords within each year
  group_by(year) |>
  mutate(rank = row_number()) |>
  ungroup()

# first, make a faceted horizontal bar chart for top keywords in each year
p1 <- tmp |>
  mutate(nlab = as.character(n)) |>
  ggplot() +
  aes(xmin=0, xmax=n, ymin=rank-0.45, ymax=rank+0.45, y=rank) +
  facet_wrap(~year) +
  geom_rect(alpha = 0.7, fill = "#3B7D23") +
  geom_text(hjust="right", aes(label = kwd), x=-2, col = "black", size = 6, fontface = 'bold') +
  scale_x_continuous(limits = c(-35, 30)) +
  scale_y_reverse() +
  geom_text(aes(label = nlab, x=n+2), size = 5)

# make labels for the animated chart
labs <- tmp |> transmute(year=year, lab = as.character(year))

# create animated race chart
p2 <- p1+
  facet_null() +
  aes(group = kwd) +
  geom_text(
    aes(x=12, y=17, label=lab),
    data = labs,
    inherit.aes = FALSE,
    size = 24,
    hjust = 0
  ) +
  labs(x=NULL, y=NULL) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 34)
  ) +
  transition_states(year, transition_length = 2, state_length = 4) +
  enter_fade() +
  exit_fade()

# render --> i then just save the gif after viewing it in html, works fine
animate(p2, fps=10, duration = 40, height = 8, width = 13, units = "in", res = 150)

# 7. food and nutrition related keywords over time ----------------------------------

# first, go back to the cleaned kwd data (before we isolate top 20)
food <- kwds_clean[-1] |>
  map(\(x){
    x |>
      count(kwd) |>
      arrange(-n) |>
      rowid_to_column()
  }) |>
  imap(~mutate(.x, year = .y)) |>
  list_rbind() |>
  mutate(
    year = paste0(20, str_remove_all(year, "y")) |> as.numeric()
  )

# pull out keywords with food/nutrition/diet/beverage. there are probably more, but
# this seems to get to the main point
freqdat <- food |>
  mutate(
    check = case_when(
      str_detect(kwd, "food") ~ "check",
      str_detect(kwd, "nutrition") ~ "check",
      str_detect(kwd, "diet") ~ "check",
      str_detect(kwd, "beverage") ~ "check",
      TRUE ~ "no"
    )
  )

# check how we did
freqdat |>
  filter(check == "check") |>
  print(n=Inf)

# looks good, move on
freqdat <- freqdat |>
  filter(check == "check") |>
  group_by(year) |>
  summarise(freq = sum(n))


# create growing line graph
tmp <- freqdat |>
  # don't want to include incomplete years
  filter(year!=2025) |>
  ggplot(aes(x=year, y=freq)) +
  geom_line(colour = "#3B7D23", linewidth = 3) +
  geom_point(colour = "#3B7D23", size = 7) +
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 24, 2)) +
  labs(x=NULL, y = "Frequency\n") +
  theme(
    panel.background = element_rect(fill=NA),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black", size = 14)
  ) +
  transition_reveal(year) +
  shadow_trail()

# animate, save from output
animate(tmp, fps=10, duration = 20, height = 6, width = 9, units = "in", res = 150,
        renderer = gifski_renderer(loop=FALSE))