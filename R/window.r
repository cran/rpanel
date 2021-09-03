rp.control <- function(title = "", size = c(100, 100),
                  panelname = paste("window", .nc(), sep = ""),
                  background = NULL, ...) {

   panel           <- w.window(title, size, background, panelname, ftype = "old", ...)  
   panel$panelname <- panelname

   handshake(tkbind, panel$.handle, "<Destroy>", 
      function() {
         if (!exists(panelname, envir = .rpenv)) return(invisible())
         rp.control.dispose(panel)               
      }
   )   

   assign(panelname, panel, .rpenv)
  
   invisible(panel)
}

rp.control.dispose <- function(panel) {
   panelname <- panel$panelname
   pwidget   <- eval(parse(text = panelname), envir = .rpenv)
   try(tkdestroy(pwidget$.handle))
   listing <- ls(.rpenv)
   np      <- nchar(panelname)
   for (i in 1:length(listing)) {
      if ((listing[i] == panelname) | 
          ((substr(listing[i], 1, np) == panelname) & 
          (substr(listing[i], np + 1, np + 1) %in% c("$", "."))))
         eval(parse(text = paste("rm('", listing[i], "')", sep = "")),
                    envir = .rpenv)
   }
      
   # find other examples and set them to NULL
   listing <- ls(.rpenv)
   for (i in 1:length(listing)) {
      objname <- listing[i]
      if ((nchar(objname) < 50)) {
         obj <- eval(parse(text = objname), envir = .rpenv)
         if (is.list(obj) && !is.null(obj$.type) && (obj$.type == "window") &&
            as.character(obj$.handle$ID) == as.character(panel$.handle$ID))
            eval(parse(text = paste("rm(", objname, ")", sep = "")),
                 envir = .rpenv)
      }
   }
   # if (exists(panel$panelname, envir= .rpenv)) {
      # print("panel exists")
      # eval(parse(text = paste("rm(", panel$panelname, ")", sep = "")), envir = .rpenv)
   # }
   invisible()

}

# w.window.dispose <- function(panel) try(tkdestroy(panel$.handle))

rp.panel <- function(panelname)
   invisible(rp.control.get(panelname))

rp.control.resize <- function(panel, width, height) {
   wm <- panel$.handle
   geo <- paste(width, "x", height, "+", 0, "+", 0, sep="")
   handshake(tkwm.geometry, wm, geo) 
}

w.window <- function(title = "", size = c(100, 100), background = NULL,
                     ftype = "new", ...) {

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

w.window.settitle <- function(panel, title)  {
   handshake(tkwm.title, panel$handle, title)
   panel$.title <- title
   invisible(panel)
}

w.window.focus <- function(panel)
   handshake(tkfocus, panel$.handle)
