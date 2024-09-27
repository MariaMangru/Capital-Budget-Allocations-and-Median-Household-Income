# Analysis of Uneven Investments in Toronto's Wards

## Overview

This project focuses on analyzing capital funding and budgeting data for various wards and programs/agencies spanning from 2022 to 2031 in Toronto. This analysis examines the relationship between capital funding and key socioeconomic factors, including median household income, educational attainment, and visible minority populations.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from Open Data Toronto.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `other` contains details about LLM chat interactions and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.
-   `scripts` contains the R scripts used to simulate, download and clean data.

## Requirements 

To run this package, you will need to install R(version 4.10 or higher) and RStudio (optional but highly recommended). 
Afterwards, you'll need to install a few packages in R. These can be installed by running the this line of code:
`install.packages(c("tidyverse", "readr", "stringr", "dplyr", "ggplot2", "knitr"))`

## Statement on LLM usage

Aspects of the code were written with the help of the large language model ChatGPT. The entire chat history is available in `other/llm/usage.txt`.

## Contact
For any questions or feedback, please reach out to me at maria.mangru@mail.utoronto.ca or create an issue in the repository. Thank you!
