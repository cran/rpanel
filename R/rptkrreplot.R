.rptkrreplot <- function (lab, fun = lab$fun, hscale = lab$hscale, vscale = lab$vscale) 
{
  .my.tkdev(hscale, vscale)
  try(fun())
  .Tcl(paste("image create Rplot", lab$image))
  if (!is.null(lab$foreground))
  {
#    this may need commented out
    tkcreate(lab, 'image', 0, 0, image=lab$image, anchor="nw")
    tkcreate(lab, 'image', lab$margins[1], lab$margins[2], image=lab$foreground, anchor="nw")
  }
}

#function (lab, fun = lab$fun, hscale = lab$hscale, vscale = lab$vscale) 
#{
#    .my.tkdev(hscale, vscale)
#    try(fun())
#    .Tcl(paste("image create Rplot", lab$image))
#}

# Moved to rptkrreplot.r and code tidied 19/07/2006 by EC.

rp.tkrreplot<-function(panel, name) {
# some preparations 
  if (require(tkrplot)) {  # This uses Luke Tierney's tkrplot package

  id <- deparse(substitute(name))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# replot the graph. plotfun support removed - we always use the function already defined in rp.tkplot
  .rptkrreplot(.geval(panelname, "$", id))

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
  }
  else {
    warning("Package TkRplot is not installed.")
  }
}
