This is version 1.0 of the public replication archive for:

Osmundsen, Mathias, David J. Hendry, Lasse Laustsen, Kevin B. Smith, and
Michael Bang Petersen. "The Psychophysiology of Political Ideology:
Replications, Reanalysis, and Recommendations." Journal of Politics.

Files included in this replication archive:
 - Main Directory:
    - 01_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 02_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 03_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 04_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 05_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 06_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
    - 07_OsmundsenEtAl-PsychophysiologyIdeology-JOP.do
 - Data Subdirectory:
    - OsmundsenEtAl-PsychophysiologyIdeology-JOP-Original.dta
    - OsmundsenEtAl-PsychophysiologyIdeology-JOP-ManualIdentificationOfProblematicSubjects.dta
    - OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta
    - OsmundsenEtAl-RatingStudy-JOP-Original.dta
    - MetaAnalysis Subdirectory:
       - [Empty]
 - Figures Subdirectory:
    - [Empty]
 - Tables Subdirectory:
    - [Empty]
 - Logs Subdirectory:
    - [Empty]

Notes:

 - Users of this replication file should leave the directory structure
   unchanged and set the working directory to the main directory where the .do
   files are located.
 - Running .do files 1-5 and 7 will reproduce the analyses presented in both the
   published paper and online appendix, with the exception of the meta-analysis.
 - The meta-analysis is a reanalysis of data originally collected by other
   researchers that is not publicly available. We received the data and
   permission to use them through personal correspondence, and they were
   provided under the condition that we would not share them without permission.
   Researchers hoping to analyze these data should contact us to seek
   permission.
 - The code to perform the meta-analysis is provided for transparency in .do
   file 6.
 - Running the files unaltered will produce the tables and figures from the
   published paper and the online appendix and place copies in the "Tables" and
   "Figures" subdirectories, respectively.
 - Users can uncomment one line of code in the beginning and one line of code in
   the end of each .do file to produce a log file that will be placed in the
   "Logs" subdirectory.
 - Data for different aspects of the analysis are separated into four files.
    - OsmundsenEtAl-PsychophysiologyIdeology-JOP-Original.dta
       - Data from the original Danish and American laboratory studies
    - OsmundsenEtAl-RatingStudy-JOP-Original.dta
       - Data from the online picture rating study for all but one picture
    - OsmundsenEtAl-RatingStudy-FollowUp-JOP-Original.dta
       - Data from the online picture rating study for the one picture not
         included in the original study; further explanation provided in the .do
         files.
    - OsmundsenEtAl-PsychophysiologyIdeology-JOP-ManualIdentificationOfProblematicSubjects.dta
       - Data used in the appendix analysis that eliminates subjects based on a
         subjective determination of poor data quality.
********************************************************************************