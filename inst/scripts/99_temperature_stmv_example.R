# 1. stmv interpolations assuming some seasonal pattern
# twostep:  ~ 24 hrs :( .. consider shorter time range and less exhaustive settings


set.seed(12345)

year.assessment = 2018
year.start = 1950

nyrs = year.assessment - year.start

# about 6 min
scale_ram_required_main_process = 0.8 # GB twostep / fft -- 5 min
scale_ram_required_per_process  = 1.25 # twostep / fft /fields vario ..  (mostly 0.5 GB, but up to 5 GB) -- 20 hrs
scale_ncpus = min( parallel::detectCores(), floor( (ram_local()- scale_ram_required_main_process) / scale_ram_required_per_process ) )

# about 32 hrs !
interpolate_ram_required_main_process = 2.5 # GB twostep / fft  -- 4 hrs
interpolate_ram_required_per_process  = 5 # 1 GB seems enough for twostep / fft /fields vario .. but can take more
interpolate_ncpus = min( parallel::detectCores(), floor( (ram_local()- interpolate_ram_required_main_process) / interpolate_ram_required_per_process ) )


# p = aegis.temperature::temperature_parameters( yrs=yrs )  # these are default years

# prep input data:
p0 = aegis::spatial_parameters( spatial_domain="temperature_test",
  aegis_proj4string_planar_km="+proj=utm +ellps=WGS84 +zone=20 +units=km",
  dres=1/60/4, pres=0.5, lon0=-64, lon1=-62, lat0=44, lat1=45, psignif=2 )
# or:  p0 = stmv_test_data( "aegis.test.parameters")

DATA = list(
  input = stmv::stmv_test_data( datasource="aegis.spacetime", p=p0),
  output = list(
    LOCS = spatial_grid(p0),
    COV  = list(
      z = stmv::stmv_test_data( datasource="aegis.bathymetry", p=p0)
    )
  )
)
DATA$input = sf::st_as_sf( DATA$input, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
DATA$input = sf::st_transform( DATA$input, crs=st_crs(p0$aegis_proj4string_planar_km) )
DATA$input = as.data.frame( cbind( DATA$input[, c("t", "z", "tiyr")], st_coordinates(DATA$input) ) )
names(DATA$input) = c("t", "z", "tiyr", "plon", "plat")
DATA$input = DATA$input[ which(is.finite(DATA$input$t)), ]



# quick look of data
dev.new(); fields::surface( fields::as.image( Z=DATA$input$t, x=DATA$input[, c("plon", "plat")], nx=p0$nplons, ny=p0$nplats, na.rm=TRUE) )


p = aegis.temperature::temperature_parameters(
  p=p0,  # start with spatial settings of input data
  project_class="stmv",
  stmv_model_label="gam_fft_twostep_statsgrid10km",
  data_root = file.path(tempdir(), "temperature_test"),
  DATA = DATA,
  spatial_domain = p0$spatial_domain,
  spatial_domain_subareas =NULL,  # prevent subgrid estimation
  inputdata_spatial_discretization_planar_km = 1 / 10, # 0.5==p$pres; controls resolution of data prior to modelling (km .. ie 20 linear units smaller than the final discretization pres)
  yrs = year.start:year.assessment,
  dimensionality="space-time-cyclic",
  stmv_global_modelengine = "none",
  stmv_local_modelengine = "twostep" ,
  stmv_local_modelformula_time = formula( paste(
    't',
    '~ s( yr, k=', floor(nyrs*0.3), ', bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
    '+ s( yr, cos.w, sin.w, k=', floor(nyrs*0.3), ', bs="ts") ',
    '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
    '+ s( log(z), plon, plat, k=6, bs="ts")  '
    ) ),
  stmv_twostep_time = "gam",
  stmv_twostep_space = "fft",  # everything else is too slow ...
#  stmv_fft_filter="lowpass matern tapered modelled fast_predictions",  # options for fft method: also matern, krige (very slow), lowpass, lowpass_matern, stmv_variogram_resolve_time
  stmv_fft_filter="lowpass matern tapered modelled exhaustive_predictions",  # options for fft method: also matern, krige (very slow), lowpass, lowpass_matern, stmv_variogram_resolve_time,
   # exhaustive_predictions required as data density is variable and low
  stmv_lowpass_nu = 0.5,  # 0.5=exponential, 1=gaussian
  stmv_lowpass_phi = stmv::matern_distance2phi( distance=0.5, nu=0.5, cor=0.1 ),  # note: p$pres = 0.5
  stmv_variogram_method = "fft",
  stmv_autocorrelation_fft_taper = 0.75,  # benchmark from which to taper .. user level control of smoothness
  stmv_autocorrelation_localrange = 0.1,  # for reporting in stats
  stmv_autocorrelation_interpolation = c(  0.3, 0.2, 0.1, 0.01 ),  # range finding
  stmv_local_model_distanceweighted = TRUE,
  stmv_filter_depth_m = 10, # the depth covariate is input as units of depth (m) so, choose stats locations with elevation > 10m as being on land
  stmv_rsquared_threshold = 0.001, # lower thindreshold for timeseries model
  stmv_distance_statsgrid = 10, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
  stmv_distance_scale = c( 2, 5, 10, 15, 20, 40, 80  ), # km ... approx guess of 95% AC range, the range also determine limits of localrange
  stmv_distance_interpolation = c( 5, 10, 15, 20, 40, 80  ) , # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
  stmv_distance_prediction_limits =c( 2.5, 5 ), # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
  stmv_nmin = 120,  # min number of unit spatial locations req before attempting to model in a localized space .. control no error in local model
  stmv_nmax = 120*(nyrs/2), # no real upper bound.. just speed / RAM limits  .. can go up to 10 GB / core if too large
  stmv_tmin = floor( nyrs * 1.25 ),
  stmv_force_complete_method = "fft",
  stmv_runmode = list(
    scale = rep("localhost", scale_ncpus),  # 7 min
    interpolate_correlation_basis = list(
      c1 = rep("localhost", interpolate_ncpus),  # ncpus for each runmode
      c2 = rep("localhost", max(1, interpolate_ncpus)),
      c3 = rep("localhost", max(1, interpolate_ncpus-1)),
      c4 = rep("localhost", max(1, interpolate_ncpus-2)),
      c5 = rep("localhost", max(1, interpolate_ncpus-2))
    ),
    # if a good idea of autocorrelation is missing, forcing via explicit distance limits is an option
    # interpolate_distance_basis = list(
    #   d1 = rep("localhost", interpolate_ncpus),
    #   d2 = rep("localhost", interpolate_ncpus),
    #   d3 = rep("localhost", max(1, interpolate_ncpus-1)),
    #   d4 = rep("localhost", max(1, interpolate_ncpus-1)),
    #   d5 = rep("localhost", max(1, interpolate_ncpus-2)),
    #   d6 = rep("localhost", max(1, interpolate_ncpus-2))
    # ),
    # interpolate_predictions = TRUE,
    globalmodel = FALSE,
    restart_load = FALSE,  # FALSE means redo all, TRUE means update currently saved  instance
    save_completed_data = TRUE # just a dummy variable with the correct name
  )  # ncpus for each runmode
)

# clear memory
DATA = NULL
gc()

# run it
stmv( p=p )  # This will take from xx hrs, depending upon system

# to remove temporary files run:  stmv_db( p=p, DS="cleanup.all" )

predictions = stmv_db( p=p, DS="stmv.prediction", ret="mean", yr = year.assessment )
statistics  = stmv_db( p=p, DS="stmv.stats" )
locations   = spatial_grid( p )


# stats
statsvars = dimnames(statistics)[[2]]
# statsvars = c( "sdTotal", "rsquared", "ndata", "sdSpatial", "sdObs", "phi", "nu", "localrange" )
dev.new(); levelplot( rowMeans(predictions[]) ~ locations[,1] + locations[,2], aspect="iso" )


dev.new(); levelplot( statistics[,match("nu", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) # nu
dev.new(); levelplot( statistics[,match("sdTotal", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #sd total
dev.new(); levelplot( statistics[,match("localrange", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #localrange


# time series plots
dev.new()
for (i in p$yrs) {
  predictions = stmv_db( p=p, DS="stmv.prediction", ret="mean", yr =i )
  for (j in 1:p$nw) print( levelplot( predictions[,j] ~ locations[,1] + locations[,2], aspect="iso" ) )
}


# comparisons
dev.new(); surface( as.image( Z=rowMeans(predictions), x=locations, nx=p$nplons, ny=p$nplats, na.rm=TRUE) )

# finished
