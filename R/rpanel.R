# RPANEL package which provides simple routines for using interactive widgets.
# Version 1.0-4 (October 2006)
# Authors of rpanel include Adrian Bowman, Gavin Alexander, Ewan Crawford and Richard Bowman with comment
# from Brian Ripley and Simon Urbanek.

# -------------------------------------------------------------------------------------------------------
# Private functions and initialisations
# -------------------------------------------------------------------------------------------------------

.rpenv <- new.env()

.onAttach <- function(library, pkg) {
# First function run on opening the package
  cat("Package `rpanel', version 1.0-4\n")
  cat("type help(rpanel) for summary information\n")
  invisible()
}

.geval <- function(...) {
# evaluate the parameters within the panel's environment environment.
  expression = paste(..., sep="")
# not that this will return the result of the evaluation as well as carry it out  
  invisible(eval(parse(text = expression), envir = .rpenv))
}

.gassign <- function(value, ...) {
# do an assignment within the panel's environment environment.
  expression = paste(..., sep = "")
# note that this will return the created variable as well as create it
  invisible(assign(expression, value, envir = .rpenv))
}

.passign <- function(value, panelname, elementname){
# assign something to a variable within a panel, given the name of the panel and the
# element of the panel to which we're assigning, and value, which is assigned to it.
    tmppanel <- rp.panel(panelname)
    eval(parse(text=paste('tmppanel$',elementname," <- value",sep="")))
    .gassign(tmppanel,panelname)
} 

.rp.initialise <- function(panelname, varname, initval = NULL, default = NULL){
# setup the property panel$varname
  if (is.null(.geval(panelname, "$", varname)))
  {
    if (!is.null(initval))
    {
      if (is.character(initval)) { .geval(panelname, "$", varname, " <- '", initval, "'") }
      else { .geval(panelname, "$", varname, " <- ", initval) }
    }
    else { if (!is.null(default)) .geval(panelname, "$", varname, " <- ", default) }
  }
# return the created varname
  invisible(.geval(panelname, "$", varname))
}

.rp.layout <- function(widget, pos, default=NULL) {
# place the widget in the position given
  if (is.null(pos) && !is.null(default)) pos <- default

  if (is.null(pos))
    tkpack(widget, expand = "true", fill = "both") 
  else if (pos[1] == "default_left_align")
    tkpack(widget, expand = "true", anchor = "w") 
  else if (pos[1] %in% c("left", "right", "top", "bottom") && length(pos) == 1)
    tkpack(widget, side = pos) 
  else
    tkplace(widget, x = pos[1], y = pos[2], w = pos[3], h = pos[4])
  invisible()
}

# -------------------------------------------------------------------------------------------------------
# Public functions
# -------------------------------------------------------------------------------------------------------

rp.pos <- function() {
# This simply runs the rp.pos demonstration.
  demo(rp.pos)
}  

rp.panelname <- function(new = TRUE) {
# return the name of the next panel. Used for "disposable" panels.
# disposable panels are those created in functions but not returned to the panel's environment
# level. They are often menus.
# return an unused panel name of the form .rpanel0.### where ### is a number.  If new is false, 
# return the name of the last panel created.
  if (new){
    testname <- paste('.rpanel', substr(as.character(runif(1)), 4, 11), sep="")     
# generate test name .rpanel########
    while (exists(testname, envir = .rpenv)) {
      testname <- paste('.rpanel', substr(as.character(runif(1)), 4, 11), sep="")
# the above tries random names until it finds one that's not in use
    }
    .gassign('.rp.last.panel.name', testname)
    testname
  }
  else {
    warning("retrieving the name of the last panel created does not always work!")
    if (exists('.rp.last.panel.name', env = .rpenv)) .geval('.rp.last.panel.name')
    else stop("there is no record of the last panel created.  Perhaps no panels have been created yet?")
  }
  
#this function will probably be used by the user, and as such it might be nice _not_ to use
#invisible() around the returned value.
  
}


rp.panel <- function(panelname = rp.panelname(new = FALSE)) {
# return a panel - with panelname return the named panel, and without the most recently worked with.
  invisible(.geval(panelname))
}

rp.do <- function(panel, action = I) {
# execute a function which an rpanel panel is associated with - used to call the function which sets
# up graphical displays.
# some preparations
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
# call the action function
  panel <- action(panel)
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

# -------------------------------------------------------------------------------------------------------
