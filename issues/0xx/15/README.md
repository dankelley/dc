New download pattern.  It seems that NOAA now does not offer up netcdf files,
so we must create them. Here is the pattern I'm using, from a
previously-offered file that I know oce can read):

netcdf topo_81W_72W_22N_31N_1min_gmt {
dimensions:
	side = 2 ;
	xysize = 291600 ;
variables:
	double x_range(side) ;
		x_range:units = "meters" ;
	double y_range(side) ;
		y_range:units = "meters" ;
	double z_range(side) ;
		z_range:units = "meters" ;
	double spacing(side) ;
	int dimension(side) ;
	short z(xysize) ;
		z:scale_factor = 1. ;
		z:add_offset = 0. ;
		z:node_offset = 1 ;

// global attributes:
		:title = "" ;
		:source = "" ;
}


