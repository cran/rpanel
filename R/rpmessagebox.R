# Moved to rpmessagebox.r and code tidied 19/07/2006 by EC.

rp.messagebox <- function(..., title = "rpanel Message") {
  tcl("tk_messageBox", message = paste(...), title = title)
}
