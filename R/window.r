w.window <- function(title = "", size = c(100, 100), background = NULL, ftype = "new", ...) {

  if (!is.null(size)) { 
    if (length(size) == 2)
       wm <- handshake(tktoplevel, height = size[2], width = size[1])
    else { 
       wm <- handshake(tktoplevel)
       geo <- paste(size[1], "x", size[2], "+", size[3], "+", size[4], sep = "")
       handshake(tkwm.geometry, wm, geo) 
    }  
  }
  else
     wm <- handshake(tktoplevel)
  handshake(tkwm.title, wm, title)
  w.setbackground(wm, background)
  if (ftype != "new")
    panel <- list(...) 
  else { 
    panel <- list()
    w.assign(...) 
  }
  panel$.handle <- wm
  panel$.type   <- "window"
  
  invisible(panel)
}

w.window.settitle <- function(panel, title)
{
  handshake(tkwm.title, panel$handle, title)
  panel$.title <- title
  invisible(panel)
}

w.window.dispose <- function(panel)
{
  try(handshake(tkdestroy, panel$.handle), silent = TRUE)
}

w.window.focus <- function(panel)
{
  handshake(tkfocus, panel$.handle)
}

rp.control <- function(title = "", size=c(100, 100), 
                       panelname = paste("window", .nc(), sep = ""), background = NULL, ...) {
  panel <- w.window(title, size, background, panelname, ftype = "old", ...)  
  panel$panelname <- panelname

  handshake(tkbind, panel$.handle, "<Destroy>", 
    function() 
    {  
# destroy the window
# this probably should be through 'handshake' but I feel there is a risk it may not work properly every time
      if (exists(paste(panelname,"$.handle", sep = ""), envir = .rpenv)) 
         eval(parse(text=paste("try(tkdestroy(", panelname, "$.handle))", sep="")), envir=.rpenv)

# find other examples and set them to NULL
      listing <- ls(.rpenv)
      for(i in 1:length(listing)) {
        objname <- listing[i]
        if (nchar(objname) < 50) {
          obj <- eval(parse(text=objname), envir=.rpenv)
          if (is.list(obj)) {
            if (!is.null(obj$.type)) {
              if (obj$.type == "window") {
                if (as.character(obj$.handle$ID) == as.character(panel$.handle$ID)) {
                  eval(parse(text = paste("remove(", objname, ")", sep = "")), envir = .rpenv)
                }
              }
            }
          }
        }
      }

      if (exists(panelname, envir = .rpenv)) 
         eval(parse(text=paste(panelname, " <- NULL", sep="")), envir=.rpenv)
    }
  )    
  
  assign(panelname, panel, .rpenv)

  invisible(panel)
}

# I'm not sure this function is ever used.  It doesn't look right anyway.  name is never used.
rp.control.dispose <- function(panel, name)
{
  try({
    panelname <- deparse(substitute(panel))
    pwidget   <- eval(parse(text = panelname), envir=.rpenv)
    w.window.dispose(pwidget)
  })
}




rp.panel <- function(panelname) {
# return a panel - with panelname return the named panel, and without the most recently worked with.
  invisible(rp.control.get(panelname))
}

rp.control.resize <- function(panel, width, height)
{
  wm <- panel$.handle
  geo <- paste(width, "x", height, "+", 0, "+", 0, sep="")
  handshake(tkwm.geometry, wm, geo) 
}
