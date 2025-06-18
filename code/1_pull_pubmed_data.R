# Use a faculty roster to pull pubmed data


# 0. setup --------------------------------------------------------------------------
library(here)
library(tidyverse)

# 1. load data ----------------------------------------------------------------------
dat <- read_csv(
  here("data/faculty_roster_uottawa_seph_june2025.csv"),
  locale = locale(encoding = "ISO-8859-1") # to make sure accents etc. are read correctly
)

# 2. process text  --------------------------------------------------------------
pubmedproc <- function(x){
  x |>
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all(" ", "+")
}

dat$search <- pubmedproc(dat$name)

# 3. generate urls ------------------------------------------------------------------
genurl <- function(x){
  url1 <- "https://pubmed.ncbi.nlm.nih.gov/?term="
  url2 <- "&filter=datesearch.y_5&sort=date&format=pubmed&size=200"
  paste0(url1, x, url2)
}

dat$url <- genurl(dat$search)

# 4. pull data ----------------------------------------------------------------------
pmdat <- map(dat$url, read_file)

# 5. unlist data -------------------------------------------------------------------
names(pmdat) <- dat$name # name the list

pmdat <- map(pmdat, \(x){ # split lines, unlist
  x |>
    str_remove_all("\r") |>
    str_split("\n") |>
    unlist()
})


# 6. process data -------------------------------------------------------------------
proclist <- function(x){
  keep <- which(str_detect(x, "^OT(?!O)"))
  kwd <- x[keep] |> str_remove_all("OT  - ") |> str_to_lower()
  return()
}

kwds <- pmdat |>
  map(proclist) |>
  unlist() |>
  unname() |>
  tibble() |>
  rename(kwd = 1)

kwds <- kwds |>
  mutate(
    kwd = kwd |>
      str_remove_all("\\*") |>
      str_remove_all("[\"']")
  )



# 7. clean errors in popular keywords ------------------------------------------------------------------------

# filter words that appear at least 5 times, arrange alphabeticaly, and print
# use this to see if there are close matches we can combine
kwds |>
  count(kwd) |>
  filter(n>=5) |>
  arrange(kwd) |>
  print(n=Inf)

kwds <- kwds |>
  mutate(
    kwd = case_when(
      kwd=="adolescence" ~ "adolescent",
      kwd=="adolescents" ~ "adolescent",
      kwd=="alcohol" ~ "alcohol or alcohol use disorder",
      kwd=="alcohol use disorder" ~ "alcohol or alcohol use disorder",
      kwd=="problematic alcohol use" ~ "alcohol or alcohol use disorder",
      str_detect(kwd, "antibiotics") ~ "antibiotics",
      str_detect(kwd, "audit") ~ "audit",
      str_detect(kwd, "behaviour change") ~ "behaviour change",
      str_detect(kwd, "cardiovascular disease") ~ "cardiovascular disease",
      kwd=="child" ~ "children/youth",
      kwd=="children" ~ "children/youth",
      kwd=="youth" ~ "children/youth",
      str_detect(kwd, "chronic disease") ~ "chronic disease",
      str_detect(kwd, "clinical practice guideline") ~ "clinical practice guidelines",
      str_detect(kwd, "clinical trial") ~ "clinical trial",
      str_detect(kwd, "cluster randomized trial") ~ "cluster randomized trial",
      str_detect(kwd, "cost-effectiveness") ~ "cost-effectiveness analysis",
      str_detect(kwd, "covid-19") ~ "covid-19",
      kwd=="sars-cov-2" ~ "covid-19",
      kwd == "e-cigarettes" ~ "electronic cigarettes",
      str_detect(kwd, "guideline") ~ "guidelines",
      kwd=="health equity" ~ "health equity/inequity/disparities",
      kwd=="health inequities" ~ "health equity/inequity/disparities",
      kwd=="health disparities" ~ "health equity/inequity/disparities",
      str_detect(kwd, "health services") ~ "health services",
      str_detect(kwd, "implementation") ~ "implementation",
      str_detect(kwd, "intervention") ~ "interventions",
      str_detect(kwd, "pragmatic trial") ~ "pragmatic trials",
      kwd=="qualitative" ~ "qualitative research",
      str_detect(kwd, "randomized controlled trial") ~ "randomized controlled trials",
      str_detect(kwd, "reporting guideline") ~ "reporting guidelines",
      kwd=="research methods" ~ "research methodology",
      kwd=="statistics &amp; research methods" ~ "statistical methods",
      str_detect(kwd, "suicid") ~ "suicide",
      kwd=="systematic review" ~ "systematic reviews",
      kwd=="telemedicine" ~ "telehealth",
      TRUE ~ kwd
    )
  )


# 8. output keywords that appear more than 10 times -------------------------------------------------------------------------------
library(gt)
kwds |>
  count(kwd) |>
  filter(n>=10) |>
  arrange(-n) |>
  gt() |>
  tab_options(
    table.align = "left",
    heading.align = "left",
    table.font.size = 11,
    data_row.padding = 1
  )
