
COMPUTE filter_$=(physfilter = 1).
VARIABLE LABEL filter_$ 'physfilter = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT threat
  /METHOD=ENTER female Yearborn demog3 demog1 sclspmagbldmean.

COMPUTE filter_$=(physfilter = 1).
VARIABLE LABEL filter_$ 'physfilter = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT threat
  /METHOD=ENTER female Yearborn demog3 demog1 meanampisi.
