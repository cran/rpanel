\name{rp.pos}

\alias{rp.pos}

\title{Positioning controls in an rpanel}

\description{This function provides demonstrations of the use of the \code{pos} argument in functions to create controls.}

\usage{rp.pos(layout="default")}

\arguments{
\item{layout}{the type of panel layout to be demonstrated. Valid options are "default", "pack", "place" and "grid".}
}

\details{
The various functions to create controls accept a parameter called \code{pos} 
which can be used to specify the layout of the controls. It has various modes of 
operation and the mode is determined from the type of information provided in the 
\code{pos} argument. The different modes are outlined below.

\itemize{
\item{default}{
If \code{pos} is not specified, controls are arranged in a column with the most recent added to the bottom. Each control is aligned to the left hand side.
}
\item{pack}{  
if \code{pos} is set to \code{"left"}, \code{"right"}, \code{"top"} or \code{"bottom"}, then the control is set to the left, right, top or bottom edge of the panel.  If there is already a control in that position, the new control is placed beside that control, closer to the centre.  (This uses Tk's "pack" layout manager.)
}
\item{place}{
If \code{pos} is set to a vector of four integer values, these are interpreted as \code{(x, y, width, height)} where all dimensions are in pixels.  \code{x} and \code{y} define the  co-ordinates in from the left hand side and down from the top respectively.  When using this mode of laying out objects, it usually helps to define the size of the panel in \code{rp.control}.  (This uses Tk's "place" layout manager.)
}
\item{grid}{
This mode provides greater flexibility in layout. The following arguments can be passed to \code{pos} in any of the function calls to create controls.  Alternatively, \code{pos} can be passed a list which has these named components.

  \itemize{	
  \item{column}{An integer which specifies the column number. Columns count from 0. This is a mandatory field for grids.}
  \item{row}{An integer which specifies the row number. Rows count from 0. This is a mandatory field for grids.}
  \item{grid}{A string which gives the name of the grid the control has to be placed in. This field is optional. If omitted the default grid belonging to the panel is used.}
  \item{columnspan}{An integer which specifies how many columns the control should span. Columns are counted to the right from the start column specified by \code{column}. This field is optional. If omitted one column is assumed.}
  \item{rowspan}{An integer which specifies how many rows the control should span. Rows are counted down from the start row specified by \emph{row}. This field is optional. If omitted one row is assumed.}
  \item{width}{An integer which specifies the width of the control. For controls with writing (buttons, listboxes etc) this is in characters and for images this is in pixels. This field is optional. If omitted the control is sized horizontally to fill the cell the control is placed within.}
  \item{height}{An integer which specifies the height of the control. For controls with writing (buttons, listboxes etc) this is in characters and for images this is in pixels. This field is optional. If omitted the control is sized vertically to fill the cell the control is placed within.}
  \item{sticky}{An string which specifies how the control expands to fill the cell. This is a string with any combination of 'n', 'e', 'w', 's', representing north/east/west/south expansions. An empty string assignment ('') will centre the control. If the argument is not assigned a value then the control is 'w' (west) aligned by default.}
  \item{background}{Specifies the background colour of the grid. If left blank this defaults to the operating system's standard background colour.}
  }

  (This uses Tk's "grid" layout manager.)

  The "grid" mode of layout should not be mixed with the other modes. 
  }
}

The example below illustrates the use of \code{pos}. Try resizing the windows to explore the behaviour.
}
