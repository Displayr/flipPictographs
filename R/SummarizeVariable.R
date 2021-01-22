#' Summarizes a numeric or categorical variable for plotting
#' @param x The input variable to summarize
#' @param type The type of summary statistic to output. This should be one of 'Average', 'Sum', 'Most frequent' or 'Percentage'.
#' @param subset Option logical vector of the same length as \code{x} indicating whether or not to include
#'   an observation in \code{x}.
#' @param weights Sampling or replication weights. This is a numeric vector of the same length as \code{x}.
#' @param category A comma-seperated list of the name or indices of the categories to include for 'Percentage'.
#' @importFrom flipStatistics Mean Sum WeightedTable
#' @importFrom flipTransformations AsNumeric TextAsVector
#' @importFrom flipU ConvertCommaSeparatedStringToVector
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

    if (grepl("Average", type) || grepl("Mean", type))
        return(Mean(AsNumeric(x, binary = FALSE), weights = weights))
    if (grepl("Sum", type))
        return(Sum(AsNumeric(x, binary = FALSE), weights = weights))

    # Convert QDate to Factors (Dates do not give sensible result for Average or Sum either way)
    if (!is.null(attr(x, "QDate")))
        x <- attr(x, "QDate")

    if (grepl("Most frequent", type))
    {
        counts <- sort(WeightedTable(x, weights = weights), decreasing = TRUE)
        tmp <- names(counts)[1]
        if (is.numeric(x))
            return(as.numeric(tmp))
        else
            return(x[x == tmp][1])
    }

    # Compute "Percentage" selected
    # for binary variable
	if (isTRUE(attr(x, "questiontype") == "PickAny") ||
	    isTRUE(attr(x, "questiontype") == "PickAnyGrid") || is.logical(x))
    {
        if (sum(nchar(category), na.rm = TRUE))
            warning("Showing percentage selected (ignoring Category '",
                    category, "').")
        return(as_pct(Mean(x, weights = weights)))
    }

    # "Percentage" for numeric variable
	if (is.numeric(x) && sum(nchar(category) > 0, na.rm = TRUE))
	{
		if (grepl("\\d+\\s*-\\s*\\d+", category))
		{
			cat.range <- as.numeric(trimws(strsplit(category, split = "-")[[1]]))
			if (length(cat.range) == 2 && all(!is.na(cat.range)))
			{
				in.range <- x >= min(cat.range) & x <= max(cat.range)
				return(as_pct(Mean(in.range, weights = weights)))
			}
		}
		else if (is.numeric(x) && any(x != round(x, 0)))
			warning("A numeric variable was supplied. Specify a range as the category (e.g. '1-5') or round variables to integers")
	}

    category.names <- levels(as.factor(x)) # only interested in labels so don't need to worry about values
    if (sum(nchar(category), na.rm = TRUE) == 0)
        stop("Select one or more categories from \"", paste(category.names, collapse = "\", \""), "\".")
    category.selected <- ConvertCommaSeparatedStringToVector(as.character(category), text.qualifier = "\"")
    ind.not.selected <- which(!category.selected %in% category.names)
    if (length(ind.not.selected) > 0 && any(grepl(",", category.names, fixed = TRUE)))
        warning("Categories \"", paste(category.selected[ind.not.selected], collapse = "\", \""),
                 "\" not found. Use double-quotes to surround category names containing commas.")

    # "Percentage" for categorical variable (the section above is just to get warning)
    is.selected <- x %in% category.selected
    is.selected[is.na(x)] <- NA     # '%in%' converts NA to FALSE
	return(as_pct(Mean(is.selected, weights = weights)))
}

as_pct <- function (x)
{
    x <- x * 100
    attr(x, "statistic") <- "%"
    return(x)
}
