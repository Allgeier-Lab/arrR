#' as_data_table_ras
#'
#' @description Convert raster to data.table
#'
#' @param x  Raster* object.
#' @param ...	 Additional arguments passed to \code{`as.data.frame`} or
#' \code{`getValues`}.
#'
#' @details
#' Internal function to convert Raster* to data.table.
#'
#' @return data.table
#'
#' @references
#' Based on code by Etienne B. Racine with contribution of Lyndon Estes taken
#' from https://gist.github.com/etiennebr/9515738.
#'
#' @keywords internal
#'
#' @export
int_as_data_table_ras <- function(x, xy = TRUE,
                                  in_memory = raster::canProcessInMemory(x), ...) {

  # get col names
  col_names <- names(x)

  # able to handle data in memory
  if (in_memory) {

    # convert to data.table
    result <- data.table::as.data.table(raster::as.data.frame(x, xy = xy, ...))

    # add col names of x,y col are present
    if (xy) {

      col_names <- c("x", "y", col_names)

    }

    # rename object
    data.table::setnames(result, col_names)

  # not able to handle data in memory
  } else {

    # get block size that memory can handle
    block_size <- raster::blockSize(x)

    # loop through blocks
    result <- lapply(1:block_size$n, function(i) {

      data_table <- data.table::as.data.table(as.data.frame(
        raster::getValues(x, row = block_size$row[i],
                          nrows = block_size$nrows[i]), ...))


      # # rename object
      data.table::setnames(data_table, names(data_table), col_names)

      # if xy col is present
      if (xy == TRUE) {

        # get cell ids of current block
        cells <- raster::cellFromRowCol(x, row = c(block_size$row[i],
                                                   block_size$row[i] + block_size$nrows[i] - 1),
                                        col = c(1, ncol(x)))

        # get coords of current cell ids
        coords <- data.frame(raster::xyFromCell(x, cell = cells[1]:cells[2]))

        # add coords to data.table
        data_table[, c("x", "y") := coords]
      }

      return(data_table)
    })

    # rbind list to data.table
    result <- data.table::rbindlist(result)

    # add col names of x,y col are present
    if (xy) {

      col_names <- c("x", "y", col_names)

      data.table::setcolorder(result, col_names)

    }
  }

  return(result)
}
