Supercharger network data
	- https://supercharge.info/data
	- /data/supercharger_net_table.html
	- (Tesla more accurate but not accessible?https://www.tesla.com/findus?bounds=33.45797260772103%2C-93.06363709917878%2C29.202305211452252%2C-101.3216939633212&zoom=8&filters=supercharger&location=giddingstxsupercharger)
EV Charging Profile Data
	- EVProject Infrastructure ReportJan13Dec13_HA
	- 2013 csv here: https://avt.inl.gov/project-type/data
	- see pdf here: https://avt.inl.gov/sites/default/files/pdf/EVProj/EVProject%20Infrastructure%20ReportJan13Dec13.pdf
Most of the network topology and node/gen/branch data
	- ZIP from Patrick with ERCOT's monthly CRR package
	- /data/Network1/
Henry Hub daily Gas Prices
	- EIA: https://www.eia.gov/dnav/ng/ng_pri_sum_dcu_nus_m.htm
	- /data/Henry_Hub_...
Gas Price dispersion within Texas
	- NREL Hydra: https://maps.nrel.gov/hydra/?aL=xZMS-8%255Bv%255D%3Dt%26QvexB3%255Bv%255D%3Dt%26QvexB3%255Bd%255D%3D1&bL=clight&cE=0&lR=0&mC=37.24782120155428%2C-88.61572265625&zL=5
	- __________
Renewables Data
	- From Dharik, I think these cap factors were calc'ed in house
	- /data/Historical ERCOT Renewables/
Load Data
	- From Dharik, downloaded from ERCOT?
	- /data/Historical ERCOT Load/
Generation average heat rates
	- SNL: ____
	- /data/misc sources/Texas Generation SNL.xls
Road netowrk
	- TX DOT
	https://gis-txdot.opendata.arcgis.com/datasets/d4f7206d27af4358acb70cb1cc819d10_0/data?geometry=-98.712%2C31.577%2C-97.406%2C31.782&orderBy=ZOOM


ASSUMPTIONS / CONSTANTS
==========
PenaltyForMwViolation = 5000
	- seems like that's what ERCOT uses for Base Case
	- https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=2ahUKEwii-ZjA8ILlAhUr11kKHbtpCQQQFjAAegQIARAC&url=http%3A%2F%2Fwww.ercot.com%2Fcontent%2Fmeetings%2Ftac%2Fkeydocs%2F2010%2F1104%2F05._ercot_buspract_shadow_price_caps_pwr_bal_pen_v0_21.doc&usg=AOvVaw1SMdF4aN_1iQZ9pHtzIKzP
	- https://www.arcgis.com/home/item.html?id=d4f7206d27af4358acb70cb1cc819d10
PenaltyForLoadShed = 20000
	- Just want it to be super high so it never happens
SuppressLogRadialBranch = 1
	- Radial branches are fine in my network
