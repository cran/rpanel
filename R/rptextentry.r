# 11/2007, 12/2007 by EC & AB.
# 07/2008 EC

rp.textentry <- function(panel, var, action = I, labels = NA, names = labels, title = NA, initval = NA,
 parent = window, pos = NULL, ...) {

# obtain the panel
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }

  pos = .newpos(pos, ...)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

# set initval (if not already set) to var
  varname <- deparse(substitute(var))
  if ((varname %in% names(panel)) && (all(is.na(initval)))) {
    initval <- .geval(panelname, "$", varname)
  }

# identify the number of boxes
  if ((length(initval) == 1) && (is.na(initval))) {
    if ((length(labels) == 1) && (is.na(labels))) {
       nboxes <- 1
       if (is.na(title)) title <- varname
       labels <- varname
       }
    else {
       nboxes  <- length(labels)
       if (is.na(title) & (nboxes == 1)) title <- labels
       }
    initval <- rep(NA, nboxes)
    }
  else {
    nboxes <- length(initval)
    if ((length(labels) == 1) && (is.na(labels)))
      if (nboxes != 1) {
       labels <- paste(varname, 1:nboxes, sep = "")
      } else {
       labels <- varname
      } 
    else
       if (length(labels) != nboxes)
          stop("lengths of labels and initval do not match.")
    }
  if ((nboxes == 1) & (!is.na(title)))
     labels <- title

# create var which will be initially a blank vector, it will be populated as tcls are created
  .geval(panelname, "$", varname, " <- vector(length=", nboxes, ")")

# create the title, if unset
  if (nboxes > 1)
    if (is.na(title)) title <- varname

# if there are several labels create a frame
  if ((!is.list(pos)) || (is.null(pos$grid))) {
    gd = panel$window
  }
  else {
    gd = .geval(panelname,"$",pos$grid)
  }

  if (nboxes > 1) {  
    frame <- tkwidget(gd, "labelframe", text = title, padx=2, pady=2) # frame was newgroup
  }
  else {
    frame <- tkframe(gd)
  }

  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
    .rp.layout(frame, pos) # lay it out
  }
  else {
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
    tkgrid(frame, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan);    
  }    

# add a function to each textbox
  for (i in 1:nboxes) {

# setup the value and set to initial value
# this will take this form: panelname$varnameX where X is the vertical numbered position
    if (is.na(initval[i])) initval[i] <- "NA"
    inittclvalue <- .rp.initialise(panelname, paste(varname,i,sep=""), initval[i])
    tclvariable <- .geval(panelname, "$", varname, i, ".tcl <- tclVar(", deparse(inittclvalue), ")")
    if (is.numeric(inittclvalue))
    { .geval(panelname, "$", varname, "[", i, "] <- deparse(", inittclvalue, ")") }
    else
    { .geval(panelname, "$", varname, "[", i, "] <- '", inittclvalue, "'") }

    if (!any(is.na(names)))
    { .geval("names(",panelname, "$", varname, ")[", i, "] <- '", names[i],"'") }


# create the call back function
    f <- function()
    {
      for (i in 1:nboxes)
      {
        .geval(panelname, "$", varname, "[", i, "] <- tclvalue(", panelname, "$", varname, i, ".tcl)")
        if (!any(is.na(names)))
        { .geval("names(",panelname, "$", varname, ")[", i, "] <- '", names[i],"'") }
      }

# call the action function
      panel <- action(.geval(panelname))

# has the panel been passed back?
      if (!is.null(panel$intname)) {     # assign the returned value back to the .rpenv - replaces rp.return
        .gassign(panel,panelname)
      }
      else {

# no intname? no panel! Stop and complain.
        stop("The panel was not passed back from the action function.")
      }
    }

    if (!any(is.na(names)))
    { .geval("names(",panelname, "$", varname, ")[", i, "] <- '", names[i],"'") }

# now create the label and textbox and then place them
    label <- tklabel(frame, text = labels[i], height="1")
    if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {  
      entry <- tkentry(frame, textvariable = tclvariable) #create the widget
    } else {
      entry <- tkentry(frame, textvariable = tclvariable, width=pos$width) # , height=pos$height) #create the widget
    }
 
    tkgrid(label, entry)
    tkgrid.configure(label, sticky="w")
    tkgrid.configure(entry, sticky="e")
#    tkgrid.rowconfigure(frame, i, weight=1) #, minsize=30)

# bind the keypress and enter events to the textbox
    tkbind(entry, "<Key-Return>", f) #run the function when enter is pressed
    # tkbind(entry, "<KeyRelease>", f) #sync the text whenever user types

  }
#  tkgrid.propagate(frame, TRUE)
#  tkpack(frame) #, expand=TRUE)

  }

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
