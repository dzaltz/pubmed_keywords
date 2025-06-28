# Extracting keywords from PubMed Data

## **Summary**

The purpose of this brief repository is to describe methods to pull data from PubMed and analyze patterns in keywords that appear across recently-published studies among a group of researchers. All data in this repository are publicly available. As a motivating example, and in anticipation of a job-talk for a tenure-track position at the [University of Ottawa School of Epidemiology and Public Health (SEPH)](https://www.uottawa.ca/faculty-medicine/epidemiology), I show how we can take a list of faculty members and quickly review general topics of their recent publications.

## **Data**

The primary data used in this repository is a simple list of names of all current faculty in the school. This process may be suitable for webscraping, but [the list](https://www.uottawa.ca/faculty-medicine/epidemiology/faculty) was easily copied and pasted into a tabular spreadsheet. As of June 17, 2025, there were 33 faculty members listed as current at the UOttawa SEPH.

## **Workflow**

The code is divided into three scripts:

1. /code/1_pull_pubmed_data.R: this shows how the data are gathered and processed for further analysis
2. /code/2_coauthor_network.R: this shows how to create an animated network of coauthorships
3. /code/3_top_keywords.R: this shows how to create a race chart for top keywords, as well as a growing line chart for specific keywords over time

## **TO DO**

All scripts are commented fairly well, but I may be inclined to continue the tutorial here. Just no time to do so at the moment...
