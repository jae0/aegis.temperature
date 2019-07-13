
temperature.map = function( p=NULL, DS="all", yr=NULL, do.seasonal=FALSE ) {

  if (is.null(yr)) yr = p$yrs

  if ( DS=="all" ) {
    # run everything below
    p$yrs = yr # in case yr was specified
    allgrids = unique(c( p$spatial.domain.subareas, p$spatial.domain) )
    for ( gr in allgrids ) {
      print (gr)
      p1 = spatial_parameters(  p=p, spatial.domain=gr )
      temperature.map( p=p1, DS="stmv.stats" ) # no parallel option .. just a few
      temperature.map( p=p1, DS="climatology" ) # no parallel option .. just a few
      if (do.seasonal) temperature.map( p=p1, DS="seasonal" ) # all seasonal predicted means
      temperature.map( p=p1, DS="annual" ) # bottom.statistics.annual
    }
  }

  # -----------------------

  if ( DS=="seasonal" ) {

    if (exists( "libs", p)) RLibrary( p$libs )

    bottomdir.maps = project.datadirectory("aegis", "temperature", "maps", p$spatial.domain, "bottom.predictions", "seasonal" )
    dir.create( bottomdir.maps, recursive=T, showWarnings=F )
    loc = bathymetry.db(p=p, DS="baseline" )
    datarange = seq(-1, 12, length.out=100)
    cols = color.code( "blue.black", datarange )
    annot.cex=2

    for ( y in yr ) {
      H = temperature.db( p=p, DS="predictions", yr=y, ret="mean"  )
      if (is.null(H)) next ()
      for (w in 1:p$nw ) {
        wchar = paste( "0", w, sep="" )
        wchar = substr( wchar, nchar(wchar)-1, nchar(wchar) )
        outfn = paste( "temperatures.bottom", y, wchar, sep=".")
        annot = paste("Temperature\n", y, " - ", w, sep="")
        dir.create (bottomdir.maps, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( bottomdir.maps, paste(outfn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=cbind(loc, H[,w]), depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex,
          at=datarange, col.regions=cols, corners=p$corners, spatial.domain=p$spatial.domain )
        print(lp)
        dev.off()
      }
    }
    return( "Completed maps")
  }

  # ---------------------------

  if ( DS %in% c("annual" ) ) {

    if (exists( "libs", p)) RLibrary( p$libs )
    bottomdir.maps = project.datadirectory( "aegis", "temperature", "maps", p$spatial.domain, "bottom.predictions", "annual" )
    dir.create( bottomdir.maps, recursive=T, showWarnings=F )
    loc = bathymetry.db(p=p, DS="baseline" )
    annot.cex=3

    for ( vname in p$bstats ) {
      H = temperature.db( p=p, DS="bottom.statistics.annual", ret=vname )
      if (is.null(H)) next ()

      for ( iy in 1:length(yr) ) {
        y = yr[iy]

        if (vname %in% c("tmean") ) {
          datarange = seq(-0.5, 10, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom", y, sep=".")
          annot = y
        }

        if (vname %in% c("tsd") ) {
          datarange = seq(0.001, 2, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.sd", y, sep=".")
          annot = y
        }

        if (vname %in% c("tmin") ) {
          datarange = seq(-0.5, 10, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.min", y, sep=".")
          annot = y
        }

        if (vname %in% c("tmax") ) {
          datarange = seq(-0.5, 10, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.max", y, sep=".")
          annot = y
        }

        if (vname %in% c("tamplitude") ) {
          datarange = seq(0, 10, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.amplitude", y, sep=".")
          annot = y
        }

        if (vname %in% c("degreedays") ) {
          datarange = seq(-400, 6000, length.out=100)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.degreedays", y, sep=".")
          annot = y
        }

        dir.create (bottomdir.maps, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( bottomdir.maps, paste(outfn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=cbind(loc, H[,iy]), depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex,
          at=datarange ,col.regions=cols, corners=p$corners, spatial.domain=p$spatial.domain )
        print(lp)
        dev.off()
        print(fn)
      }
    }
    return( "Completed maps")
  }

  # ------------------------------

  if ( DS %in% c("climatology" ) ) {

    if (exists( "libs", p)) RLibrary( p$libs )

    bottomdir.maps = file.path( project.datadirectory("aegis", "temperature"), "maps", p$spatial.domain, "bottom.predictions", "climatology" )
    dir.create( bottomdir.maps, recursive=T, showWarnings=F )
    H = temperature.db( p=p, DS="bottom.statistics.climatology" )

    loc = bathymetry.db(p=p, DS="baseline" )
    annot.cex=0.9

    for (vname in p$bstats) {

      if (vname %in% c("tmean") ) {
        # datacols = c("plon", "plat", "tmean")
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom", sep=".")
        annot = paste("Temperature bottom climatology\n", sep="")
      }

      if (vname %in% c("tsd") ) {
        # datacols = c("plon", "plat", "tmean")
        datarange = seq(0.001, 2, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.sd", sep=".")
        annot = paste("Temperature bottom SD climatology\n", sep="")
      }

      if (vname %in% c("tmin") ) {
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.min", sep=".")
        annot = paste("Temperature bottom minimum climatology\n",  sep="")
      }

      if (vname %in% c("tmax") ) {
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.max", sep=".")
        annot = paste("Temperature bottom maximum climatology\n",  sep="")
     }

      if (vname %in% c("tamplitude") ) {
        datarange = seq(0, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.amplitude", sep=".")
        annot = paste("Temperature bottom amplitude climatology\n",  sep="")
      }

      fn = file.path( bottomdir.maps, paste(outfn, "png", sep="." ) )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
      lp = aegis_map( xyz=cbind(loc, H[,which( p$bstats==vname)]), depthcontours=TRUE, pts=NULL, annot=annot, annot.cex=annot.cex,
        at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain )
      print(lp)
      dev.off()

    }

  }


  # ------------------------------


  if ( DS %in% c("stmv.stats" ) ) {

    if (exists( "libs", p)) RLibrary( p$libs )

    H = temperature.db( p=p, DS="stmv.stats" )
    namesH = colnames(H)

    bottomdir.maps = file.path( project.datadirectory("aegis", "temperature"), "maps", p$spatial.domain, "bottom.predictions", "stmv.stats" )
    dir.create( bottomdir.maps, recursive=T, showWarnings=F )

    loc = bathymetry.db(p=p, DS="baseline" )
    annot.cex=0.9

    for (vname in namesH) {

      if (vname %in% c("sdTotal") ) {
        datarange = seq(0.01, 5, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "sdTotal", sep=".")
        annot = paste("Temperature bottom sdTotal\n", sep="")
        xyz=cbind(loc, H[,vname])
      }

      if (vname %in% c("rsquared") ) {
        datarange = seq(0.1, 0.99, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "rsquared", sep=".")
        annot = paste("Temperature bottom rsquared\n", sep="")
        xyz=cbind(loc, H[,vname])
      }

      if (vname %in% c("ndata") ) {
        datarange = seq(5, 8000, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "ndata", sep=".")
        annot = paste("Temperature bottom ndata\n",  sep="")
        xyz=cbind(loc, H[,vname])
      }

      if (vname %in% c("sdSpatial") ) {
        datarange = seq(0.01, 5, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "sdSpatial", sep=".")
        annot = paste("Temperature bottom sdSpatial\n",  sep="")
        xyz=cbind(loc, H[,vname])
     }

      if (vname %in% c("sdObs") ) {
        datarange = seq(0.01, 5, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "sdObs", sep=".")
        annot = paste("Temperature bottom sdObs\n",  sep="")
        xyz=cbind(loc, (H[,vname]))
      }

      if (vname %in% c("localrange") ) {
        datarange = seq( log(0.5), log(median(p$stmv_distance_scale)*3), length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "localrange", sep=".")
        annot = paste("Temperature bottom local range\n",  sep="")
        xyz=cbind(loc, log(H[,vname]))
        xyz = xyz[which( is.finite(rowSums(xyz))),]
      }

      if (vname %in% c("phi") ) {
        datarange = seq(log(0.15), log(median(p$stmv_distance_scale)*2), length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "phi", sep=".")
        annot = paste("Temperature bottom phi\n",  sep="")
        xyz=cbind(loc, log(H[,vname]))
        xyz = xyz[which( is.finite(rowSums(xyz))),]
      }

      if (vname %in% c("nu") ) {
        datarange = seq(0.1, 4, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "nu", sep=".")
        annot = paste("Temperature bottom nu\n",  sep="")
        xyz=cbind(loc, (H[,vname]))
      }

      if (vname %in% c("ar_timerange") ) {
        datarange = seq(0, 6, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "ar_timerange", sep=".")
        annot = paste("Temperature bottom ar_timerange\n",  sep="")
        xyz=cbind(loc, (H[,vname]))
      }

      if (vname %in% c("ar_1") ) {
        datarange = seq(0, 1, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "ar_1", sep=".")
        annot = paste("Temperature bottom ar_1\n",  sep="")
        xyz=cbind(loc, H[,vname])
      }

      dir.create (bottomdir.maps, showWarnings=FALSE, recursive =TRUE)
      fn = file.path( bottomdir.maps, paste(outfn, "png", sep="." ) )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
      lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
        annot=annot, annot.cex=annot.cex,
        at=datarange, col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain )
      print(lp)
      dev.off()
    }
    return (NULL)
  }

}
