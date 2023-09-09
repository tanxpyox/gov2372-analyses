README.txt
John G. Bullock
john.bullock@aya.yale.edu
2011 October 30


This file provides notes on the use of replication files associated with

    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
    Informed Electorate."  American Political Science Review 105 (3): 
    496-515.


For the most part, there is a one-to-one association between R syntax files 
and figures.  For example, Figure_A1.R creates only Figure_A1.pdf.  There are
a few exceptions:

    Figure_2.R creates Figure 2.  To create Figure A2, change 
    "EDUC.COMPARE <- FALSE" to "EDUC.COMPARE <- TRUE" in Figure_2.R
    
    Figure_3.R creates Figure 3.  To create Figure A6, change
    "EDUC.COMPARE <- FALSE" to "EDUC.COMPARE <- TRUE" in Figure_3.R

    Medicaid_2008_11_average_effects.R and Medicaid_2010_05_average_effects.R
    are meant to be called by other files rather than run directly.
    They do not create any figures by themselves.
       

        