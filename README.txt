# EEC2019
Code for Georgina Halford EEC Masters Thesis. Written by Georgina Halford. 
sparrow_cage2.py and format_interactions.py were created by Alexander Flynn-Caroll and updated by (Georgina Halford). The original can be found here: https://github.com/aflynncarroll/CMEE17
├── sparrow_cage2.py      <-  A general use file to make interaction times from RFID data
│   ├── raw interaction data needed in csv file as one continuous column 
│   ├── complete for each date interaction data was taken for that network
│   ├── outputs sorted and overlap.csv files for each date
│   ├──sorted contains all entries into the cage and overlap contains all interactions between two individuals inside the cage 
├── format_interactions.py       <-  Formats RFID data into one data file 
│   ├── need overlap.csv files for each date data was taken within that network 
│   ├── creates one total.csv file that contains all interactions within that network 
├── sociality_scores.R    <-  Initial analysis of RFID data for gain sociality scores
│   ├── creates sociality scores from the interation data contained in total.csv
│   ├──files needed for each network are coh_sex.csv, validcolourcodes.csv, tblAllCodes.csv and total.csv
│   ├──coh_sex.csv<-contains a column of bird ID against a cloumn of sex and a column with their cohort year 
│   ├──validcolourcodes.csv<-contains a birdID column against a column with their ring colour codes 
│   ├──tblAllCodes.csv<-birdID column and a column with their transponder numbers
│   ├──creates a soc.csv file containing the sociality scores for degree, betweenness and closeness for individuals within the network as well as background data such as cohort, age, sex and ring colour codes
│   ├──individuals that entered the cage but did not interact do not appear in the file but have sociality scores of 0 (inferred from raw data in each Aviary folder, ocassion 1 raw data is shown in the aviary folders and ocassion 2 raw data in the separate file)
├── Repeatability.R       <-  Analyse sociality scores for repeatability and make networks 
│   ├──analyses sociality scores from both rounds of social network analysis to test for repeatability  
│   ├──needs a file containing all sociality scores for every individual (InteractionResults.csv)
│   ├──every indivudual appears within two networks within the dataset 
├── SNAsimulations.R      <-  Randomised social network simulations to create null model 
│   ├──creates a distribution of repeatability from randomised null models 
│   ├──needs a file containing all interactions from every occassion and aviary used in the initial anlysis (total_interactions.csv) 
