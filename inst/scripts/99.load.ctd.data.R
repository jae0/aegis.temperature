
# one off loading of STD data for 2011 and 2012 CTD data for groundfish data ...

# saved to show loading of ODF formatted file method:


    data.dir = project.datadirectory("aegis", "temperature", "data", "tmp" )

    flist = list.files(path=data.dir, pattern="CTD.*\\.ODF$", full.names=T, recursive=FALSE)
    out = NULL

    for ( fn in flist ) {
      dat = parse.odf.file (fn )
      dat$fn = fn

      out= rbind( out, dat )
    }
