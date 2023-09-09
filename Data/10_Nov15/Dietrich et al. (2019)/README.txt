Overview - To replicated a figure or table, please download desired folder. Once downloaded, unzip the appropriate folder and set your working directory to the appropriate folder (e.g., /Users/brycedietrich/Downloads/tables/). Additional instructions can be found after the list of required R packages. Please refer to the preamble of each file and the manifests for more information.

Run Times -
figures.zip: 1 minute, 19 seconds
tables.zip: 4 minutes, 51 seconds
figures.zip and tables.zip: 6 minutes, 15 seconds

Computer Information - 
Computer: MacBook Pro (Retina, 13-inch, Early 2013)
Processor: 3 GHz Intel Core i7
OS: macOS Sierra 10.12.6 

Software Information - 
R Version: 3.5.0 (2018-04-23) -- "Joy in Playing" 
Pyton Version: 3.6.3 (default, Oct  4 2017, 06:09:15)

Python Modules -
matplotlib (2.2.2)
numpy (1.14.3)
pandas (0.23.0)
scipy (1.1.0)

R Packages -
caret_6.0-80
foreign_0.8-70
ggplot2_2.2.1
lattice_0.20-35
lme4_1.1-17
Matrix_1.2-14
stargazer_5.2.2
xtable_1.8-2


To replicate a figure, the basic workflow is as follows:

1.) Download figures.zip
2.) Unzip figures.zip
3.) Change the working directory to path of the unzipped figures folder (i.e., on line 18 of figure_s2.R)
4.) Run the desired script (see figures/scripts folder)
5.) Compare the png output (see figures/output folder) to the desired figure (i.e., Figure S2 of SI)

To replicate a table, the basic workflow is as follows:

1.) Download tables.zip
2.) Unzip tables.zip
3.) Change the working directory to path of the unzipped tables folder (i.e., on line 23 of table_1.R)
5.) Run the desired script (see tables/scripts folder)
6.) Compare the html output (see tables/output folder) to the desired table (i.e., Table 1 of main text)

The most important files are the following:

1.) figures.zip - Replicates Figures S2-S9 from the Supplemental Information. Please read the preambles for data and package requirements. The manifest includes additional information about each subdirectory.

2.) tables.zip - Replicates Table 1 from the main text and Tables S1-S9 from the Supplemental Information. Please read the preambles for data and package requirements. The manifest includes additional information about each subdirectory.

We also provided the following helpful files:

1.) collecting_data.zip - Various scripts described beginning on page 1 of the Supplemental Information and ending on page 4 right before "Getting Each Justices’ Baseline" section.

2.) finalizing_results.zip - These replication files are referenced in the "Finalizing the Results" section on page 5 of the Supplemental Information. The code is the same as table_1.R, but it is still included because it is referenced in the Supplemental Information.

3.) justice_audio_examples.zip - Example audio clips from Justice Kennedy and Justice Scalia. These are referenced in footnotes 16-21 in the main text.

4.) justice_baseline.zip - Justice baseline example referenced on page 4 of Supplemental Information.

5.) pitch_results.zip - Pitch results example referenced on pages 4-5 in the Supplemental Information.

DO NOT run the following files:

1.) get_transcripts.R - Used this script to download transcripts from Oyez, but the site has changed so the script no longer works. See cases.tab for example output. This file can be found in collecting_data.zip

2.) get_advocates.R - Used this script to add advocate information from Oyez, but the site has changed so the script no longer works. See cases.tab for example output. This file can be found in collecting_data.zip