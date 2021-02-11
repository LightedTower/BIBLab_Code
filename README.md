# BIBLab_Code
This repository contains the scripts used to Check the quality of the data for various tasks in this Study when downloaded from Gorilla.sc, and redcap.

Tasks Included: Flanker, Delayed Discount, Simon's, Visual Search, Willingness to pay

Code Features for Flanker, Simon's and Visual Search Tasks:
	1. Read in and bind multiple files in a folder.
	2. Filter by participant's response
	3. Count number of unique ids
	4. Add a `question type` column to seperate congruent and incongruent questions in analysis.
	5. Remove Marked Bots
	6. Remove repeate participant data
	7. Merge participant Behavioral task data with mood score values into one chart
	8. Create Overall, Congruent, and Incongruent descriptive summaries including mean, percent correct, standard deviation, and standard error.
	9. Bind multiple datasets together for easier reading and creating graphics
	10. Isolate participants based on completion level; 50% and 70% complete.
	11. Countof participants excluded based on 70% completion rate.
	12. Create overall, congruent, and incongruent graphics based on summary data.
	
Planned Features for above tasks:
	0. More summary data (skew, kurtosis, etc.)
	1. Trim outlier data
	2. Create a list of participants trimmed
	3. Automatically create folders for data files.
	4. Move data files to that folder on the desktop.