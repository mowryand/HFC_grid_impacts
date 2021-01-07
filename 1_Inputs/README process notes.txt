To get results:
	- Run extract_bus_lz_map.R
		- creates a map of buses to wz (and lz) areas by parsing the .KML file
		- results are written to csv file "Data/parsed_bus_info.csv"
	- Run process_raw_psse.R
		- formats R data.tables from the PSSE format .raw file
		- stores results in memory
	- Run process_psse_pso.R
		- creates most necessary PSO input files from the R objects created in last step
	- Run process_gen_groups.R
		- NOTE this relies on a manual mapping of PSSE nodes to an SNL table. With new topologies the map be incomplete. Also there may be some innaccuracies for smaller gens
		- Creates injector group tables (PSO inputs) with VOM, HR, Fuel hookups, etc, for thermal gens
	- Run get_charger_network
		- alters INJ_ID and INJ_NET with a charging network
		- adds colocated batteries in INJ_ID, INJ_NET, and SRG_ID
	- Run alter_gen.R
		- scales zonal gen capacity according to LTSA scenarios
	- Run alter_xm.R
		- TODO
	- Run create_sch_tmp.R
		- relies on a few PSO tables from previous step
		- creates area-wide load schedules in PSO format from WZ forecasts
		- creates gen-specific schedules for wind and solar
		- creates charger schedules
	- Run PSO, dump "vector" results to folder
	- Run plot_results.R <- Charts for S&D, pricing, congestion, etc
	- Run plot_lmp_map.R <- Movies of area prices
	- Run plot_pf_net.R <- Movies of slices of power flow network to highlight EV issues


Manually populated PSO tables:
	_
	CYC_ID
	CYC_PRD_ID
	MDL_ID
	SCN_CYC
	SCN_ARA_LOD
	SCN_FUE_CST

Manually populated data tables
	snl_psse_gen_map.csv
	gen_type_defaults.csv

Mapping libs:
R TMAP (ggplot for maps): 
	https://geocompr.robinlovelace.net/adv-map.html
	https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html
	https://www.jla-data.net/eng/adjusting-bounding-box-of-a-tmap-map/
R SF (base package): 
	https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#what_is_a_feature
	https://rstudio-pubs-static.s3.amazonaws.com/488297_b0b54ede7c844e639e47dff70c028b37.html#helpful-sites-besides-sf-vignetes
R raster:
	https://www.jamieafflerbach.com/post/cropping-rasters-down-to-size/
	https://geocompr.robinlovelace.net/geometric-operations.html
P Networkx: 
	https://networkx.github.io/
R igraph:
	https://kateto.net/netscix2016.html