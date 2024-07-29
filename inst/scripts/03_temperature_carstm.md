
<!-- NOTE 
  This doc is markdown formatted and can/should be viewed through a viewer:
  - a web browser with view ability (Firefox with Markdown Viewer Webext extension)
  - vscode with built-in "preview" or "Markdown Editor" extension
  - Zettlr, Quarto, Rstudio, etc
 -->

# Spatiotemporal model of bottom temperatures


## Purpose

Data collected (marine ocean bottom temperatures) from miscellaneous sources, opportunistically and from surveys with different rationales result in spatial and temporal aliasing/bias. To help reduce some of this selection bias, the data are subjected to a spatiotemporal model using [carstm](https://github.com/jae0/carstm), a simple front-end to [INLA](https://www.r-inla.org/), to perform Conditional autocorrelation (BYM) and temporal (AR1) models. 

## Prepare parameter settings

Here we prepare the parameter setting to control this modelling and prediction.

```r

require(aegis.temperature)
# loadfunctions("aegis.temperature")

year_assessment = 2023
year_start = 1999    
year_start_carstm_inputs = year_start - 5 # add 5 years prior of data to improve stability of parameters

# any changes here should be moved to temperature_parameters.R ... to find data lookup table simpler
p = temperature_parameters( 
  project_class="carstm", 
  carstm_model_label="default",
  carstm_input_time_limit = year_start_carstm_inputs, # to reduce file size /improve speed .. if 1950: ~ 4 days of compute
  yrs=year_start:year_assessment,
  spbuffer=9, # these are domain boundary options for areal_units ( search radius for aggregation)
  lenprob=0.95,   # these are domain boundary options for areal_units ( boundary determination)
  n_iter_drop=0, # no. of additional repasses in tesselation phase
  sa_threshold_km2=16, # min sa of AU
  areal_units_constraint_ntarget=12, # target no data points in each AU
  areal_units_constraint_nmin=1   # min no data points in each AU 
)


```



# Prepare data
  
```r    
  # to recreate the underlying data
  xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed

  xydata=temperature_db(p=p, DS="areal_units_input")  # redo if input data has changed
  # note learn from all available data 
  # xydata = xydata[ which(xydata$yr %in% p$yrs), ]
  xydata = xydata[ which(xydata$yr >= p$carstm_input_time_limit), ]

  # if sppoly options need to change, do so at parameter-level such that they are consistent  (though, not necessary if sppoly is passed directly to carstm)
  sppoly = areal_units( p=p, xydata=xydata,  redo=TRUE, verbose=TRUE )  # to force create : 1000-2000 units seems optimal .. this is higher to resolve space a bit better
  
  sppoly = areal_units( p=p  )  # reload


  plot( sppoly[ "AUID" ] ) 
  
  # or using carstm_map: 
  # carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
  # carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot

  M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE )  # must  redo if sppoly has changed or new data
  # M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly  ) 
  M = NULL
 

```


# Model via carstm


```r

sppoly = areal_units( p=p  )  # reload polygons
M = ' temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly )  '

# dimensionality and labels:
p$space_name = sppoly$AUID 
p$space_id = 1:nrow(sppoly)

p$time_name = as.character(p$yrs)
p$time_id =  1:p$ny

p$cyclic_name = as.character(p$cyclic_levels)
p$cyclic_id = 1:p$nw

gc()

# !!! WARNING: this uses a lot of RAM  
carstm_model( 
    p=p, 
    data =M,  
    sppoly=sppoly,
    # nposteriors=1000,
    toget = c("summary", "random_spatial", "predictions"),
    # posterior_simulations_to_retain = c("predictions"),  # not used at the moment
    family = "gaussian",
    theta=c( 0.1877, 0.7413, 1.1496, 0.9075, 2.1036, -3.6689, -2.1444, -1.4800, -0.7984, 2.5011, 0.3563, -0.9661, 0.4787 
    ),
    # control.inla = list( strategy="laplace", optimiser="gsl", restart=1 ),  # gsl = gsl::bfgs2 (gsl seems less memory demanding)
    # control.inla = list( strategy='auto'),
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" ),
    # control.inla = list( strategy='laplace'  ),
    # redo_fit=FALSE,
    # debug ="predictions",
    # debug="extract",
    # debug = "random_spatiotemporal", 
    # improve.hyperparam.estimates=TRUE,
    verbose=TRUE, 
    # compress=FALSE,  ## large file size makes compression/decompression too slow
    num.threads="2:2"  # safer .. 2:2 works on linux adjust for your machine; # 2023: 86GB RAM for fit with num.threads="3:2" ; reduce as required
  )    


    if (0) {
      # some random effects and checks
      # fit = carstm_model( p=p, DS="modelled_fit"  )  # extract currently saved model fit
      # summary(fit)  # inla object
        
      # plot(fit)
      # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

      # posterior predictive check
      M = temperature_db( p=p, DS='carstm_inputs', sppoly=sppoly  )
      carstm_posterior_predictive_check(p=p, M=M  )
  
      # EXAMINE POSTERIORS AND PRIORS
      res = carstm_model(  p=p, DS="carstm_summary" )  # parameters in p and summary
  
      names(res$hypers)
      for (i in 1:length(names(res$hypers)) ){
        o = carstm_prior_posterior_compare( hypers=res$hypers, all.hypers=res$all.hypers, vn=names(res$hypers)[i] )  
        dev.new(); print(o)
      } 
      
    }


  ## Some results and maps
 
  oeffdir = file.path(p$modeldir, p$carstm_model_label, "figures")
    fn_root_prefix = "Temperature bottom"
    carstm_plot_marginaleffects( p=p, outputdir=oeffdir, fn_root_prefix=fn_root_prefix ) 
 
  # maps of some of the results

  additional_features = features_to_add( 
    p=p, 
#      area_lines="cfa.regions",
    isobaths=c( 100, 200, 300, 400, 500  ), 
    xlim=c(-80,-40), 
    ylim=c(38, 60) # ,redo=TRUE 
  )

  # map all bottom temps: 

  outputdir = file.path(p$modeldir, p$carstm_model_label, "maps" )

  fn_root_prefix = "Bottom temperature (Celcius)"
    
  carstm_plot_map( p=p, outputdir=outputdir, 
    additional_features=additional_features, 
    toplot="random_spatial", probs=c(0.025, 0.975),  
    colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")) ) 

  carstm_plot_map( p=p, outputdir=outputdir, additional_features=additional_features, 
    toplot="predictions", colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
    brks=seq(0, 10, 2 ) )



```

# End


