README
=====

This directory and readme file pertain to the study by Andrew Mowry and Dharik Mallapragada, "Grid impacts of highway electric vehicle charging and role for mitigation via energy storage".

This directory contains all data and scripts necessary to generate the scenarios that are solved by PSO, as well as the scripts necessary to generate the plots that present the PSO results. Instructions for both of these tasks are presented below. A data dictionary can be found within the "Inputs" folder.

R version 4.0.2 was used for all processing.

To generate scenarios
-----
Scenario construction is a two-step process. First, the user must configure the desired scenarios by modifying the "run_info.csv" file in the "Inputs" folder. (As uploaded, this file contains the configurations for all of the scenarios used in the study.) Then, the user must run the "generate_scenarios.R" script, which will generate all of the scenario files as PSO requires and will save them in the "Model IO" folder.

IMPORTANT: The user must change the "PATH.BASE" variable in "generate_scenarios.R", on line 20, to the relevant directory on their own system. This is the only edit that must be made to run the script.

The logic of the "generate_scenarios.R" script is such that ALL lines in the "run_info.csv" file that do NOT have a number in the "id" field will be generated. Any lines in the configuration file that DO have an id will be skipped over. (Newly processed scenarios are given an ID and the "run_info.csv" file is updated accordingly, to serve as a record.)

A sample output from scenario production is included in the "Model IO" folder.

To generate results
-----
It is assumed that the user has an independent version of PSO to process the scenarios. When solving the scenarios that have been generated and saved into the "MODEL IO" folder, the user should save the results of each scenario into a "results" folder within the same scenario folder. So the results for the "/2_Model_IO/Scn_000980/" scenario should be saved into the "/2_Model_IO/Scn_000980/results/" folder. Results should be saved in vector (long) format.

A sample output from results generation is included in the "Model IO" folder in the "Scn_000697" subfolder.

To reproduce plots
-----
All plots can be reproduced from the "generate_plots.R" script. This script relies on several other scripts found within the "Other_Scripts" folder, as well as information from the "Inputs" and "Model_IO" folders. If the script is run in its entirety, all of the publication images will be generated and saved into the "Plots" folder. 

IMPORTANT: The user must change the "PATH.BASE" variable in "generate_plots.R", on line 18, to the relevant directory on their own system. This is the only edit that must be made to run the script.

