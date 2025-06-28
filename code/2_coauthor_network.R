# use pubmed keyword data to generate network map of co-authorships


# 0. setup --------------------------------------------------------------------------

library(here)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

# 1. load data ---------------------------------------------------------------------------
names <- read_rds(here("data/faculty_roster_w_urls_tidy.rds"))
pmpull <- read_rds(here("data/pmpull_2025-06-18.rds"))

# 2. select author data from pubmed records -----------------------------------------
auth <- map(pmpull, \(x){
  # split the list of characters at each carriage or line return, then unlist
  x |>
    str_remove_all("\r") |>
    str_split("\n") |>
    unlist()
}) |>
  map(\(x){
    # keep the lines that start with AU or TI -- AU for author and TI for title
    keep1 <- which(str_starts(x, "AU "))
    keep2 <- which(str_starts(x, "TI"))
    keep <- sort(c(keep1, keep2))
    auth <- x[keep]
    return(auth)
  })

# quick peek
head(auth$last5)


# 3. process faculty names to match pubmed format -----------------------------------

# here we take the faculty roster that we used to generate URLs and pull the data and
# process it so that it aligns with the way pubmed lists authors (Last name, first initial(s))
names <- names |>
  transmute(
    name = name,
    first = word(name, 1),
    # some people have hyphenated first names. just take all upper-case characters from the
    # first names and assume these are the correct initials
    first_initial = str_extract_all(first, "[A-Z]+") |> map_chr(\(x){paste0(x, collapse = "")}),
    last = word(name, -1),
    pmform = paste(last, first_initial)
  )

head(names) # look at data


# some last names are double, change those
names <- names |>
  mutate(
    pmform = case_when(pmform == "Emam K" ~ "El Emam K",
                       pmform == "Freeman E" ~ "Freeman EE",
                       pmform == "Kent M" ~ "Potvin Kent M",
                       TRUE ~ pmform)
  )

# create vector of pubmed author formats for SEPH faculty
pmforms <- names$pmform
# after visual inspection, we see that MPK spells her name different throughout publication record
pmforms <- pmforms |> c("Kent MP")


# 4. tidy author data and label SEPH faculty -------------------------------------------

# here we write a function to create a tibble containing each publication, its authors,
# and then a label if the author is from SEPH.
getauth <- function(x){
  x |>
    # rectangle
    tibble() |>
    rename(char = 1) |>
    mutate(
      author = ifelse(str_starts(char, "AU"), str_remove_all(char, "AU  - "), NA),
      # create a grouping variable corresponding to each publication
      pub = ifelse(str_starts(char, "TI"), 1, 0) |> cumsum()
    ) |>
    distinct(author, pub)|>
    filter(!is.na(author)) |>
    mutate(
      seph = ifelse(author%in%pmforms, 1, 0)
    )
}



n15 <- getauth(auth$y15)
n16 <- getauth(auth$y16)
n17 <- getauth(auth$y17)
n18 <- getauth(auth$y18)
n19 <- getauth(auth$y19)
n20 <- getauth(auth$y20)
n21 <- getauth(auth$y21)
n22 <- getauth(auth$y22)
n23 <- getauth(auth$y23)
n24 <- getauth(auth$y24)


# 5. write function to create static network map ------------------------------------

netmap <- function(x, year){

  # identify edges (connections between authors per publication (authors may have multiple connections across different papers))
  edges <- x |>
    select(pub, author) |>
    distinct() |>
    group_by(pub) |>
    filter(n()>1) |>
    summarise(pairs = list(as_tibble(t(combn(author, 2)))), .groups = "drop") |>
    unnest(pairs) |>
    rename(from = V1, to = V2)

  # use SEPH labels to make sure we can distinguish between them after identifying edges (see prior)
  seph_lookup <- x |>
    distinct(author, seph)

  # weighted edges?
  edges_weighted <- edges |> count(from, to, name = "weight")

  # we are only interested in connections that start from SEPH faculty, not between non-SEPH collaborators
  connected_authors <- unique(c(edges_weighted$from, edges_weighted$to))

  edges_seph <- edges_weighted |>
    left_join(seph_lookup, by = c("from" = "author")) |>
    rename(seph_from = seph) |>
    left_join(seph_lookup, by = c("to" = "author")) |>
    rename(seph_to = seph) |>
    filter(seph_from == 1 | seph_to == 1)

  nodes_connected <- x |>
    distinct(author, seph) |>
    filter(author %in% connected_authors)

  edges_clean <- edges_seph |>
    filter(from%in%nodes_connected$author & to%in%nodes_connected$author)

  graph <- tbl_graph(nodes = nodes_connected, edges = edges_clean, directed = FALSE)
  graph <- graph |> activate(nodes) |> filter(centrality_degree()>0)


  fig <- ggraph(graph, layout = "linear", circular = TRUE, sort.by=author) +
    geom_edge_arc(alpha = 0.1, colour = "#3B7D23") +
    geom_node_point(
      aes(colour = factor(seph), size = factor(seph), alpha = factor(seph)), pch = 16
    ) +
    theme_void() +
    scale_edge_width(range = c(0.2, 2)) +
    scale_colour_manual(values = c("grey70", "#3B7D23")) +
    scale_alpha_manual(values = c(0.4, 0.6)) +
    scale_size_manual(values = c(1, 5))


  years <- 2015:2024
  ys <- seq(0.4, by = -0.1, length.out = length(years))

  for (i in seq_along(years)) {
    if (years[i] == year) {
      fig <- fig + annotate("text", label = as.character(years[i]), x = 1.2, y = ys[i],
                            size = 8, colour = "#3B7D23", fontface = "bold")
    } else {
      fig <- fig + annotate("text", label = as.character(years[i]), x = 1.2, y = ys[i],
                            size = 8, colour = "gray70")
    }
  }


  fig <- fig +
    coord_fixed() +
    theme(legend.position = "none",
          plot.title = element_text(size = 18, face = "bold", hjust=0.5))

  return(fig)
}


# 6. create figures -----------------------------------------------------------------

set.seed(124) # im not sure if this relies on random generators, but maybe it does?
netlist <- list(
  n15 = netmap(n15, 2015),
  n16 = netmap(n16, 2016),
  n17 = netmap(n17, 2017),
  n18 = netmap(n18, 2018),
  n19 = netmap(n19, 2019),
  n20 = netmap(n20, 2020),
  n21 = netmap(n21, 2021),
  n22 = netmap(n22, 2022),
  n23 = netmap(n23, 2023),
  n24 = netmap(n24, 2024),
  # repeats 2024 as the year -- I make these because later when animating the figure
  # i want it to pause on the laste frame for a bit of time before looping back, and this
  # was the most intuitive way I could think of quickly. I just call it n25, 26, and so on
  # because I had already written code to read these back in, and it was the easiest way to
  # do it
  n25 = netmap(n24, 2024),
  n26 = netmap(n24, 2024),
  n27 = netmap(n24, 2024),
  n28 = netmap(n24, 2024)
)


# 7. save static figures -------------------------------------------------------------------

# optimize image size/quality and final file size. rendering the final figure with higher quality images
# can take a very long time, so be careful when specifying the size and DPI. This seemed to work fine
ggsave(filename = here("viz/n15.png"), plot = netlist$n15, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n16.png"), plot = netlist$n16, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n17.png"), plot = netlist$n17, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n18.png"), plot = netlist$n18, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n19.png"), plot = netlist$n19, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n20.png"), plot = netlist$n20, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n21.png"), plot = netlist$n21, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n22.png"), plot = netlist$n22, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n23.png"), plot = netlist$n23, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n24.png"), plot = netlist$n24, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n25.png"), plot = netlist$n25, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n26.png"), plot = netlist$n26, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n27.png"), plot = netlist$n27, height = 10, width = 10, unit = "in", dpi = 150, bg="white")
ggsave(filename = here("viz/n28.png"), plot = netlist$n28, height = 10, width = 10, unit = "in", dpi = 150, bg="white")


# 8. render dynamic figure ----------------------------------------------------------

library(magick)

# read in image files
files <- paste0(here("viz/"), "/n", 15:28, ".png")
img_list <- image_read(files)

# create dynamic figure
morph <- image_morph(img_list, frames = 20)

# render/write (takes a bit)
anim <- image_animate(morph, fps = 10, loop = 0)
image_write(anim, here("viz/network4.gif"))
