# rpanel 1.1-5

* A check is now made on whether the BWidget Tcl/Tk module is available.  This allows the package to be installed under Macos.  If BWidget is not available then three functions (rp.combo, rp.notebook, rp.notebook.raise) are disabled and advisory messges printed if these functions are called.
