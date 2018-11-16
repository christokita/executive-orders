# 1. README

The analysis pipeline for this project consists of the following scripts:

* **0-downloadEOs.R:** Downloads html of Executive Orders (EOs) from the [UCSB Presidency Project Archive](http://www.presidency.ucsb.edu/).
* **1-curateEOs.R:** Extracts the EO text, checks for missing EOs, and appends metadata related to the issuing President.
* **2-queryEOs.R:** Searches through each EO for mentions of other EOs and for use of science and technology related words.
* **3-flagSTtopicEOs.R:** Appends flag for S&T topics identified by topic model. 
* **4-visualizeEOs.R:** Creates edgelists and nodetables for network visualizations.
* **5-analyzeEONetwork.R:** Calculates network metrics based on edgelist.
* **6-makePlots.R:** Makes plots for presentation and paper.
* **7-makeTables.R:** Makes tables for presentation and paper.

The topic model analysis pipeline consists of the following scripts:
* ...