
## ------------------------------------------------------------------------
## utility functions exposed to the users
## ------------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    is(x, "db.data.frame")
}

## ------------------------------------------------------------------------

## Grab a preview of the data
preview <- function(x, nrows = 100, interactive = TRUE)
{
    if (! is(x, "db.data.frame"))
        stop(deparse(substitute(x)), " must be a db.data.frame object!")

    if (is(x, "db.view") && interactive) {
        cat(deparse(substitute(x)),
            "points to a view in the database",
            dbname(conn.id(x)),
            "and it takes time to evaluate and extract a preview of it !\n")
        go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                          c("yes", "y", "no", "n"))
        if (go == "no" || go == "n") return
    }

    if (nrows == "all")
        .db.getQuery(paste("select * from", content(x)), conn.id(x))
    else if (is.numeric(nrows)) {
        res <- try(.db.getQuery(paste("select * from", content(x),
                                      "limit", nrows),
                                conn.id(x)), silent = TRUE)
        if (inherits(res, .err.class))
            stop("Unable to extract portion of data!")
        return (res)
    }
}

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
## Sampling function
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

setGeneric ("sample")

setMethod(
    "sample",
    signature(x = "db.table"),
    function (x, size, replace = FALSE, tbl.name = .unique.string(),
              is.temp = FALSE, n.sample = 1) {
        if (! replace)
        {
            
        }
        if (!identical(x@.id.col, character(0))) # has a valid id.col
        {
            
        }
        cat("To be implemented !\n")
        return (0)
    },
    valueClass = "db.table")

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.view"),
    function (x, size, replace = FALSE, tbl.name = .unique.string(),
              is.temp = FALSE, n.sample = 1) {
        tbl.source <- .unique.string()
        .db.getQuery(paste("create temp table ", tbl.source,
                           " as (select row_number() over () * from ",
                           content(x),
                           ")", sep = ""), conn.id = conn.id(x))
        dat <- db.data.frame(tbl.source, conn.id(x), x@.id.col)
        res <- try(sample(dat, size = size, replace = replace,
                      tbl.name = tbl.name, is.temp = is.temp,
                      n.sample = n.sample), silent = TRUE)
        .db.removeTable(tbl.source, conn.id(x))
        if (inherits(res, .err.class))
            cat("\n********** Sample failed! **********\n")
        return (res)
    },
    valueClass = "db.table")

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.Rquery"),
    function (x, size, replace = FALSE, tbl.name = .unique.string(),
              is.temp = FALSE, n.sample = n) {
        tbl.source <- .unique.string()
        dat <- as.db.data.frame(x, tbl.source, is.temp = TRUE)
        res <- try(sample(dat, size = size, replace = replace,
                      tbl.name = tbl.name, is.temp = is.temp,
                      n.sample = n.sample), silent = TRUE)
        .db.removeTable(tbl.source, conn.id(x))
        if (inherits(res, .err.class))
            cat("\n********** Sample failed! **********\n")
        return (res)
    },
    valueClass = "db.table")
