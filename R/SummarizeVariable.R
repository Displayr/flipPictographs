#' Summarizes a numeric or categorical variable for plotting
#' @param x The input variable to summarize
#' @param type The type of summary statistic to output. This should be one of 'Average', 'Sum' or 'Percentage'.
#' @param subset Option logical vector of the same length as \code{x} indicating whether or not to include
#'   an observation in \code{x}.
#' @param weights Sampling or replication weights. This is a numeric vector of the same length as \code{x}.
#' @param category A comma-seperated list of the name or indices of the categories to include for 'Percentage'.
#' @importFrom flipStatistics Mean Sum WeightedTable
#' @importFrom flipTables SelectEntry
#' @export
SummarizeVariable <- function(x, type = c("Average", "Sum", "Percentage")[1], weights = NULL, subset = NULL, category = NULL)
{
    if (!is.null(weights) && length(weights) > 1 && length(weights) != length(x))
        stop("Weights should be the same length as the input variable")

    if (!is.null(subset) && length(subset) > 1)
    {
        if (length(subset) != length(x))
           stop("Filters should have the same length as the input variable")
        x <- x[subset]
        if (!is.null(weights))
            weights <- weights[subset]
    }

    if (grepl("Average", type))
        return(Mean(x, weights = weights))
    if (grepl("Sum", type))
        return(Sum(x, weights = weights))

    if (is.null(weights))
        weights <- rep(1, length(x))
    total <- sum(weights, na.rm = TRUE)

    # Pick any questions are encoded as 0 and 1s
    if (is.numeric(x) && all(x %in% c(TRUE, FALSE)))
    {
        if (sum(nchar(category), na.rm = TRUE))
            warning("Showing percentage selected (ignoring Category '",
                    category, "').")
        return(sum(weights * x)/total)
    }

    # Categorical variable
    counts <- WeightedTable(x, weights = weights)
    if (sum(nchar(category), na.rm = TRUE) == 0)
        stop("Select categories from '", paste(names(counts), collapse="', '"), "'.")

    return(sum(SelectEntry(counts, row=category, column = 1))/total)
}
