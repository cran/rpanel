# 12/11/2007 by EC.
# Pos Alterations 07/2008

rp.checkbox <- function(panel, var, action=I, labels=NA, names=labels, title=NA, initval=NA, parent=window, pos = NULL, doaction=FALSE, ...) {

# obtain the panel
 ischar <- is.character(panel)
 if (ischar) { panelname <- panel; panel <- .geval(panel) }
 else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }

  pos = .newpos(pos, ...)

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
    initval <- rep(FALSE, nboxes)
    }
  else {
    nboxes <- length(initval)
    if ((length(labels) == 1) && (is.na(labels)))
       labels <- paste(varname, 1:nboxes, sep = "")
    else
       if (length(labels) != nboxes)
          stop("lengths of labels and initval do not match.")
    }


  if ((nboxes == 1) & (!is.na(title)))
     labels <- title

# create var which will be initially a blank vector, it will be populated as tcls are created
  .geval(panelname, "$", varname, " <- vector(length=", nboxes, ")")

# create the title, if unset
    if (is.na(title)) title <- varname

# Start to lay out

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {
  
if (nboxes>1) {
# create a frame to contain the group

# create and layout the group
  if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height))))
  {
    newcheckgroup <- tkwidget(panel$window, "labelframe", text = title, padx=2, pady=2, borderwidth=2)
  }
  else
  {
    newcheckgroup <- tkwidget(panel$window, "labelframe", text = title, padx=2, pady=2, borderwidth=2, width=pos$width, height=pos$height)
  }
  
# This cannot be set as it puts Tcl/Tk into a permanent loop! Note that tkgrid(newcheckgroup ... sticky) also cannot be used.
#
#  tkgrid.configure(newcheckgroup, sticky="w")
#

  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column))))
  {
    .rp.layout(newcheckgroup, pos)
  }
  else
  {
    if (is.null(pos$grid)) {
      gd = panel$window
    }
    else {
      gd = .geval(panelname,"$",pos$grid)
    }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1 }
    if (is.null(pos$sticky)) { pos$sticky = "w" }
# Note - the in must be in quotes due to the fact that in is a reserved work in R. Daft this.
    tkgrid(newcheckgroup, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan);
  }

}
else
{
  newcheckgroup <- panel$window
}

# print("here 7")

# add a function to each groupbutton
 for (i in 1:nboxes)
 {
# setup the value and set to initial value
   if (initval[i]==TRUE) {
     tclvariable <- .geval(panelname, "$", varname, i, ".tcl <- tclVar('1')")
# lines stolen from below to ensure that the val within the panel is set before calling the action.
     if (nboxes==1)
     {
       .geval(panelname, "$", varname, " <- TRUE")
     }
     else
     {
       .geval(panelname, "$", varname, "[", i, "] <- TRUE")
       if (!any(is.na(names)))
       { .geval("names(",panelname, "$", varname, ")[", i, "] <- '", names[i],"'") }
     }
   }
   else
   {
     tclvariable <- .geval(panelname, "$", varname, i, ".tcl <- tclVar('0')")
# lines stolen from below to ensure that the val within the panel is set before calling the action.
     if (nboxes==1)
     {
       .geval(panelname, "$", varname, " <- FALSE")
     }
     else
     {
       .geval(panelname, "$", varname, "[", i, "] <- FALSE")
       if (!any(is.na(names)))
       { .geval("names(",panelname, "$", varname, ")[", i, "] <- '", names[i],"'") }
     }
   }

   f <- function()
   {
     for (j in 1:nboxes)
     {
       curstate <- .geval("tclvalue(", panelname, "$", varname, j, ".tcl)")
       if (curstate=='1') {
         .geval(panelname, "$", varname, "[", j, "] <- TRUE")
       }
       else {
         .geval(panelname, "$", varname, "[", j, "] <- FALSE")
       }
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
   
# print("here 8")   

# use the function

if (nboxes>1) {
   if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height))))
   {
     rb <- tkcheckbutton(newcheckgroup, command = f, text = labels[i], variable = tclvariable)
   }
   else
   {
     rb <- tkcheckbutton(newcheckgroup, command = f, text = labels[i], variable = tclvariable, width=pos$width, height=pos$height)
   }
   tkgrid(rb, sticky = "w")
   tkgrid.rowconfigure(newcheckgroup, i, weight=1) 
}
else {

  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column))))
  {
    if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height))))
    {
      rb <- tkcheckbutton(newcheckgroup, command = f, text = labels[i], variable = tclvariable)
    }
    else
    {
      rb <- tkcheckbutton(newcheckgroup, command = f, text = labels[i], variable = tclvariable, width=pos$width, height=pos$height)
    }
    if (is.null(pos)) pos <- "default_left_align"
    .rp.layout(rb,pos)
  }
  else
  {
    if (is.null(pos$grid))
    {
      gd = panel$window
    }
    else
    {
      p = .geval(panelname)
      gd = .geval(panelname,"$",pos$grid)
    }
    if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height))))
    {  
      rb <- tkcheckbutton(newcheckgroup, command = f, text = title, variable = tclvariable)
    } else {
      rb <- tkcheckbutton(newcheckgroup, command = f, text = title, variable = tclvariable, width=pos$width, height=pos$height)
    }
    if (!is.list(pos)) {
    { pos$rowspan = 1 }
    { pos$columnspan = 1 }
    { pos$sticky = "w" }
    }
    else
    {
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1 }
    if (is.null(pos$sticky)) { pos$sticky = "w" }
    }
# Note - the in must be in quotes due to the fact that in is a reserved work in R. Daft this.
    tkgrid(rb, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan);
  }

}

# key stage of attaching the panel variable to the button
   tkconfigure(rb, variable = tclvariable)
 }

# if requested, do the action directly after setting up the values and functions
  if (doaction==TRUE) {
    panel <- action(.geval(panelname))
  }

 } # if checkpos

 if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
