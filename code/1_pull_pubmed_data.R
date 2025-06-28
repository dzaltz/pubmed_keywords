# Use a faculty roster to pull pubmed data


# 0. setup --------------------------------------------------------------------------
library(here)
library(tidyverse)

# 1. collect data -------------------------------------------------------------------

# for large faculty rosters we could write a script to scrape data from institution
# websites, but in order to finish this sooner I just copied and pasted all names
# from the UOttawa SEPH faculty website into an excel spreadsheet and saved as a csv

# 2. load data ----------------------------------------------------------------------
dat <- read_csv(
  here("data/faculty_roster_uottawa_seph_june2025.csv"),
  # to make sure special chars (in this case, accents) are read correctly
  locale = locale(encoding = "ISO-8859-1")
)

# 3. process text  --------------------------------------------------------------

# here we make search terms for pubmed from the faculty names
# the terms are best used lower case, without special characters. also, we will be
# creating unique URLs for each search term which requires first and last names
# separated by a "+"

dat <- dat |>
  mutate(
    search = name |>
      str_to_lower() |> # make lower case
      stringi::stri_trans_general("Latin-ASCII") |> # remove all accents from French names
      str_replace_all(" ", "+") # replace all spaces with "+" sign
  )

# 3. generate urls ------------------------------------------------------------------

# here we write a function to generate unique URLs that will search pubmed for each
# faculty member and return the .txt file (pubmed format) correpsonding to their records
# from a specific time frame. we further add options to the function to be able to specify
# exactly which year start/end to filter the search, or if you want the most recent 5 years

genurl <- function(x, last5y=FALSE, year_start=NULL, year_end=NULL){
  # first part of url will stay the same
  url1 <- "https://pubmed.ncbi.nlm.nih.gov/?term="

  # second part of url will be user specified
  # as of now, we include the ability to pull the most recent data from last 5 years
  # or any user-specified timeframe bound by years
  if(!isTRUE(last5y) & (is.null(year_start) | is.null(year_end))){
    stop("If option last5y is FALSE then you must specify both the year start and end parameters")
  }
  if(isTRUE(last5y) & (!is.null(year_start) | !is.null(year_end))){
    stop("Specifying start and/or end years is not valid when the option last5y is TRUE")
  }
  if(isTRUE(last5y) & (is.null(year_start) | is.null(year_end))){
    url2 <- "&filter=datesearch.y_5&sort=date&format=pubmed&size=200"
  } else if(!isTRUE(last5y) & !is.null(year_start) & !is.null(year_end)){
    url2 <- paste0("&filter=years.", year_start, "-", year_end, "&format=pubmed")
  }
  paste0(url1, x, url2)
}

# now we generate the URLs for each faculty member and for each time period we want.
# i am sure there is a way to do this functionally but sometimes speed is better than eloquence
# if you just need to get it done

dat <- dat |>
  mutate(
    url_last5 = genurl(search, last5y = TRUE),
    url15 = genurl(search, year_start = 2015, year_end = 2015),
    url16 = genurl(search, year_start = 2016, year_end = 2016),
    url17 = genurl(search, year_start = 2017, year_end = 2017),
    url18 = genurl(search, year_start = 2018, year_end = 2018),
    url19 = genurl(search, year_start = 2019, year_end = 2019),
    url20 = genurl(search, year_start = 2020, year_end = 2020),
    url21 = genurl(search, year_start = 2021, year_end = 2021),
    url22 = genurl(search, year_start = 2022, year_end = 2022),
    url23 = genurl(search, year_start = 2023, year_end = 2023),
    url24 = genurl(search, year_start = 2024, year_end = 2024),
    url25 = genurl(search, year_start = 2025, year_end = 2025)
  )

# write tidied data to file
write_rds(dat, here("data/faculty_roster_w_urls_tidy.rds"))

# 4. pull/validate pubmed data ----------------------------------------------------------------------

# pull data from pubmed. we wrap the function in purrr::possibly just in case some of our
# urls are wonky. we can check later.

pmpull <- list(
  last5 = map(dat$url_last5, possibly(read_file)),
  y15 = map(dat$url15, possibly(read_file)),
  y16 = map(dat$url16, possibly(read_file)),
  y17 = map(dat$url17, possibly(read_file)),
  y18 = map(dat$url18, possibly(read_file)),
  y19 = map(dat$url19, possibly(read_file)),
  y20 = map(dat$url20, possibly(read_file)),
  y21 = map(dat$url21, possibly(read_file)),
  y22 = map(dat$url22, possibly(read_file)),
  y23 = map(dat$url23, possibly(read_file)),
  y24 = map(dat$url24, possibly(read_file)),
  y25 = map(dat$url25, possibly(read_file))
)

# check if any results are incomplete or null
check <- pmpull |>
  map(\(x){
    x |>
      unlist() |>
      tibble() |>
      rename(rawdat = 1) |>
      mutate(lnout = nchar(rawdat), # check character length of each record (should be many characters)
             val = is.null(rawdat)) # check if anything returned NULL
  }) |>
  # add name of data source back to individual tibbles
  imap(\(x, y){
    addname <- function(x, name){x |> mutate(record = name, .before = 1)}
    addname(x, y)
  }) |>
  # rectangle
  list_rbind() |>
  # drop rawdat, just need to check
  select(record, lnout, val)

check |> filter(is.null(val)) # expecting 0 rows returned
min(check$lnout) # expecting the min to be in at least the thousands

rm(check) # clean up environment, all checked out well


# 5. write pubmed data to file ------------------------------------------------------

# write data to file so we don't have to keep pulling it. write date to filename to make
# sure we track how up-to-date our data is
fnm <- paste0("pmpull_", Sys.Date(), ".rds")
loc <- paste(here("data/"), fnm, sep="/")
write_rds(pmpull, loc)