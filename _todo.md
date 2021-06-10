# TODO

## 2021

1) Drop CAPACITY and PEAK_CAPACITY entirely from all pages
  

2) DONE: LAST_OVERLAY_THICKNESS - value for every sample: DONE
   Scored as fraction of values with non-missing expansion_factor
   

3) DONE: CURVES_A-F; GRADES_A-F - Extent has to match total extent of sample panel for at least one CURVES/GRADES A-F

Each entry scored as fraction of values with non-missing expansion factor.  Then 
Took max score of each group


Sample extents - "sample panel".  List of sample IDs and begin and end points.  end point - begin point is extent of the sample.  Extent of Curves (Grades) A-F has to match the total extent of the sample panel. 


4) DONE: PSR dropped from score, but included on body pages
PSR don't include in the score - not required for any state .  Can be reported in lieu of IRI in two situations.  On lower functional systems, or when equipment can't work because of low speeds. DONE: Set to "not required" in data_items_required_by_states.csv

5) DONE: Include buffer of coverage for completeness score; e.g. 99% is "complete" DONE; see calc_completeness_all.R


6) DONE: Update symbols on title page to account for items which do not contribute to the overall score


7) Add page to document score calculation - define completeness and quality

8) Add HPMS branding/styling/theme to summary page.