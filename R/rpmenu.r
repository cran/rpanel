# Started 19/10/2006 by EC.

rp.menu <- function(panel, var, labels, initval = NULL, parent = window, action = I, ...) {
# some preparations
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }

# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval = "")
  
# create the callback
  callback <- function(mopt) {
    function(...) {
      .geval(panelname, "$", varname, " <- '", mopt, "'")
# call the action function
      panel <- action(.geval(panelname))
# has the panel been passed back?
      if (!is.null(panel$intname)) {      
# assign the returned value back to the .rpenv - replaces rp.return
        .gassign(panel,panelname)
      }
      else {
# no intname? no panel! Stop and complain.
        stop("The panel was not passed back from the action function.")
      }
    }        
  }
  
# create the menu
  topMenu <- tkmenu(panel$window)
  tkconfigure(panel$window, menu=topMenu)
  for (i in (1:length(labels)))
  {
    submenu <- unlist(labels[i])
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    for (j in (2:length(submenu)))
    {
# The eval is absolutely necessary as submenu[j] will have the last value assigned to it when the command is called
# so we must preserve the value it had in the loop. Very similar to the doublebutton problem.
      tkadd(fileMenu, "command", label=submenu[j], command=eval(parse(text=paste("callback('", submenu[j], "')", sep=""))))
    }
    tkadd(topMenu,"cascade",label=submenu[1],menu=fileMenu)
  }
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
