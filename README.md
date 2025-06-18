# Extracting keywords from PubMed Data

## **Summary**

The purpose of this brief repository is to describe methods to pull data from PubMed and analyze patterns in keywords that appear across recently-published studies among a group of researchers. All data in this repository are publicly available. As a motivating example, and in anticipation of a job-talk for a tenure-track position at the [University of Ottawa School of Epidemiology and Public Health (SEPH)](https://www.uottawa.ca/faculty-medicine/epidemiology), I show how we can take a list of faculty members and quickly review general topics of their recent publications.

## **Data**

The primary data used in this repository is a simple list of names of all current faculty in the school. This process may be suitable for webscraping, but [the list](https://www.uottawa.ca/faculty-medicine/epidemiology/faculty) was easily copied and pasted into a tabular spreadsheet. As of June 17, 2025, there were 33 faculty members listed as current at the UOttawa SEPH.

## **Workflow**

The entire process is contained within a single script in the \code subfolder. It is divided into 8 sections and a setup:

0.  Setup - here we load libraries [tidyverse](https://www.tidyverse.org/), for general coding purposes, and [here](https://here.r-lib.org/), for easy file retrieval.
1.  Load data - read in the tabular list of all faculty members
2.  Process text - prepare each name to be included in a custom URL
3.  Generate URLs - create unique URLs which we will use to search for each faculty members' respective PubMed records from the last five years.
4.  Pull data - read in data directly from PubMed for each faculty member
5.  Unlist data - wrangle web data into an easily-edited format
6.  Process data - pull only lines corresponding to keywords from each listed manuscript, remove special characters
7.  Clean errors - for the sake of time and brevity, we conduct a visual inspection of the most common keywords to make sure we are not including close matches as distinct words (e.g., trial and trials, review and reviews).
8.  Output popular keywords - output a table of the keywords that occur across all faculty manuscripts ten or more times
