# Started 19/10/2006 by EC.
# NOTE THE USE OF ButtonRelease-1 otherwise this returns the old value.

rp.listbox <- function(panel, var, vals, labels = vals, rows = length(vals), initval = vals[1], parent = window, pos = NULL, title = deparse(substitute(var)), action = I, ...) {
# some preparations
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }

# this is slightly awkward as ... is already used as parameters for the callback function

  pos = .newpos(pos, ...)

# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval = initval)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

# create the prompt and listBox
# create a frame to contain the radiogroup
  if (is.null(pos$grid)) {
    gd = panel$window
  }
  else {
    gd = .geval(panelname,"$",pos$grid)
  }

  newlistbox <- tkwidget(gd, "labelframe", text = title)
  
  if ((is.null(pos$row)) && (is.null(pos$column))) {
    .rp.layout(newlistbox, pos)
  }
  else {
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
    tkgrid(newlistbox, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)    
  }
  
  if (rows != length(vals))
  {
    scr <- tkscrollbar(newlistbox, repeatinterval=5, command=function(...) tkyview(listBox,...))
    if ((is.null(pos$width)) && (is.null(pos$height))) {
      listBox <- tklistbox(newlistbox, height=rows, selectmode="single", yscrollcommand=function(...) tkset(scr,...), background="white")
    } else {
      listBox <- tklistbox(newlistbox, height=rows, selectmode="single", yscrollcommand=function(...) tkset(scr,...), background="white", width=pos$width, height=pos$height)
    }  
  }
  else
  {
    if ((is.null(pos$width)) && (is.null(pos$height))) {
      listBox <- tklistbox(newlistbox, height=rows, selectmode="single", background="white")
    } else {
      listBox <- tklistbox(newlistbox, height=rows, selectmode="single", background="white", width=pos$width, height=pos$height)
    }
  }
  if (rows != length(vals))
  {
    tkgrid(listBox,scr)
    tkgrid.configure(scr,rowspan=length(vals),sticky="nsw")
  }
  else
  {
    tkgrid(listBox)
  }
  selected <- 0
  for (i in (1:length(vals)))
  {
    tkinsert(listBox, "end", labels[i])
    if (inittclvalue == vals[i]) selected <- i
  }  

# setup the list value and set to initial value
  tkselection.set(listBox, selected-1) 
  
# setup the callback
  tkbind(listBox, "<ButtonRelease-1>", function(...)
    {
      .geval(panelname, "$", varname, " <- '", vals[as.numeric(tkcurselection(listBox))+1], "'")
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
    })
  }
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
