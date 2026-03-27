###################################
########## ELO Functions ##########
###################################
#
#
#
#
#





#' Elo functions
#'
#' Calculate post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.model.frame
#' @param elo.A,elo.B Numeric vectors of elo scores.
#' @param wins.A Numeric vector of wins by team A.
#' @param ... Other arguments (not in use at this time).
#' @param adjust.A,adjust.B Numeric vectors to adjust \code{elo.A} and \code{elo.B} by.
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.update}},
#'   \code{elo.model.frame}
#' @return A data.frame with two columns, giving the new Elo values after each update.
#' @examples
#' elo.calc(c(1, 0), c(1500, 1500), c(1500, 1600), k = 20)
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.calc(wins.A ~ elo.A + elo.B + k(k), data = dat)
#' @name elo.calc
NULL
#> NULL

#' @rdname elo.calc
#' @export
elo.calc <- function(wins.A, ...)
{
  UseMethod("elo.calc")
}

#' @rdname elo.calc
#' @export
elo.calc.default <- function(wins.A, elo.A, elo.B, k, ..., adjust.A = 0, adjust.B = 0)
{
  validate_score(wins.A)
  elo.up <- elo.update(wins.A = wins.A, elo.A = elo.A, elo.B = elo.B, k = k, ...,
                       adjust.A = adjust.A, adjust.B = adjust.B)
  if(NCOL(elo.up) == 1) elo.up <- matrix(c(elo.up, elo.up), ncol = 2)
  data.frame(elo.A = elo.A + elo.up[, 1], elo.B = elo.B - elo.up[, 2])
}

#' @rdname elo.calc
#' @export
elo.calc.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k")
  mf <- eval(Call, parent.frame())
  elo.calc(mf$wins.A, mf$elo.A, mf$elo.B, k = mf$k, ...,
           adjust.A = mf$adj.A, adjust.B = mf$adj.B)
}




#' Elo functions
#'
#' Calculate the probability that team A beats team B. This is vectorized.
#'
#' @inheritParams elo.calc
#' @param elo.A,elo.B Numeric vectors of elo scores, or else vectors of teams.
#' @param elos An optional named vector containing Elo ratings for all teams in \code{formula}
#'   or \code{elo.A} and \code{elo.B}.
#' @return A vector of Elo probabilities.
#' @details
#'   Note that \code{formula} can be missing the \code{wins.A} component. If
#'   present, it's ignored by \code{\link{elo.model.frame}}.
#' @seealso \code{\link{elo.update}}, \code{\link{elo.calc}},
#'   \code{elo.model.frame}
#' @examples
#' elo.prob(1500, 1500)
#' elo.prob(c(1500, 1500), c(1500, 1600))
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.prob(~ elo.A + elo.B, data = dat)
#'
#' ## Also works to include the wins and k:
#' elo.prob(wins.A ~ elo.A + elo.B + k(k), data = dat)
#'
#' ## Also allows teams
#' elo.prob(c("A", "B"), c("C", "C"), elos = c(A = 1500, B = 1600, C = 1500))
#'
#' @name elo.prob
NULL
#> NULL

#' @rdname elo.prob
#' @export
elo.prob <- function(elo.A, ...)
{
  UseMethod("elo.prob")
}

#' @rdname elo.prob
#' @export
elo.prob.default <- function(elo.A, elo.B, ..., elos = NULL, adjust.A = 0, adjust.B = 0)
{
  if(!is.numeric(elo.A) || !is.numeric(elo.B))
  {
    all.teams <- character(0)
    if(!is.numeric(elo.A))
    {
      if(!is.players(elo.A)) elo.A <- players(elo.A)
      if(anyNA(elo.A)) stop("NAs were found in elo.A; check that it can be coerced to character.")
      all.teams <- as.character(elo.A)
    }
    if(!is.numeric(elo.B))
    {
      if(!is.players(elo.B)) elo.B <- players(elo.B)
      if(anyNA(elo.B)) stop("NAs were found in elo.B; check that it can be coerced to character.")
      all.teams <- c(all.teams, as.character(elo.B))
    }
    
    all.teams <- sort(unique(all.teams))
    elos <- check_named_elos(elos, all.teams)
    
    if(!is.numeric(elo.A)) elo.A <- rowSums(matrix(elos[elo.A], nrow = nrow(elo.A)))
    if(!is.numeric(elo.B)) elo.B <- rowSums(matrix(elos[elo.B], nrow = nrow(elo.B)))
  }
  
  unname(1/(1 + 10^(((elo.B + adjust.B) - (elo.A + adjust.A))/400.0)))
}

#' @rdname elo.prob
#' @export
elo.prob.formula <- function(formula, data, na.action, subset, ..., elos = NULL)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  mf <- eval(Call, parent.frame())
  
  elo.prob(mf$elo.A, mf$elo.B, ..., adjust.A = mf$adj.A, adjust.B = mf$adj.B, elos = elos)
}




#' Create a 1/0/0.5 win "indicator"
#'
#' Create a 1/0/0.5 win "indicator" based on two teams' scores, and test for "score-ness".
#'
#' @param score.A Numeric; the score of the first team (whose wins are to be denoted by 1).
#' @param score.B Numeric; the score of the second team (whose wins are to be denoted by 0).
#' @param x An R object.
#' @return For \code{score}, a vector containing 0, 1, and 0.5 (for ties). For
#'   \code{is.score}, \code{TRUE} or \code{FALSE} depending on whether all values of
#'   \code{x} are between 0 and 1 (inclusive).
#' @seealso \code{\link{score}}
#' @examples
#' score(12, 10)
#' score(10, 10)
#' score(10, 12)
#' @name score
NULL
#> NULL

#' @rdname score
#' @export
score <- function(score.A, score.B)
{
  (score.A > score.B) + 0.5*(score.A == score.B)
}

#' @rdname score
#' @export
is.score <- function(x)
{
  is.numeric(x) && !anyNA(x) && all(0 <= x & x <= 1)
}

validate_score <- function(x)
{
  if(!is.score(x)) stop("The wins should be between 0 and 1 (inclusive).")
  invisible(x)
}



#' Elo functions
#'
#' Calculate the update value for a given Elo matchup. This is used in
#' \code{\link{elo.calc}}, which reports the post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.calc
#' @return A vector of Elo updates.
#' @examples
#' elo.update(c(1, 0), c(1500, 1500), c(1500, 1600), k = 20)
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.update(wins.A ~ elo.A + elo.B + k(k), data = dat)
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.calc}},
#'   \code{elo.model.frame}
#' @name elo.update
NULL
#> NULL

#' @rdname elo.update
#' @export
elo.update <- function(wins.A, ...)
{
  UseMethod("elo.update")
}

#' @rdname elo.update
#' @export
elo.update.default <- function(wins.A, elo.A, elo.B, k, ..., adjust.A = 0, adjust.B = 0)
{
  validate_score(wins.A)
  remove_elo_k(k)*(wins.A - elo.prob(elo.A, elo.B, ..., adjust.A = adjust.A, adjust.B = adjust.B))
}

#' @rdname elo.update
#' @export
elo.update.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k")
  mf <- eval(Call, parent.frame())
  elo.update(mf$wins.A, mf$elo.A, mf$elo.B, k = mf$k, ...,
             adjust.A = mf$adj.A, adjust.B = mf$adj.B)
}




#' Helper functions for \code{elo.run}
#'
#' \code{as.matrix} converts an Elo object into a matrix of running Elos.
#'
#' \code{as.data.frame} converts the \code{"elos"} component of an object
#'   from \code{\link{elo.run}} into a data.frame.
#'
#' \code{final.elos} is a generic function to extract the last Elo per team.
#'
#' @param x An object of class \code{"elo.run"} or class \code{"elo.run.regressed"}.
#' @param ... Other arguments (Not in use at this time).
#' @param group A grouping vector, telling which rows to output in the matrix.
#' @param regressed Logical, denoting whether to use the post-regressed (\code{TRUE}) or
#'   pre-regressed (\code{FALSE}) final Elos. Note that \code{TRUE} only makes sense when the
#'   final Elos were regressed one last time (i.e., if the last element of the \code{regress()})
#'   vector yields \code{TRUE}).
#' @return A matrix, a data.frame, or a named vector.
#' @examples
#' e <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week),
#'              data = tournament, k = 20)
#' head(as.matrix(e))
#' str(as.data.frame(e))
#' final.elos(e)
#' @seealso \code{\link{elo.run}}
#' @name elo.run.helpers
NULL
#> NULL

#' @rdname elo.run.helpers
#' @export
as.matrix.elo.run <- function(x, ..., group = x$group)
{
  group <- check_as_matrix(x, group)
  out <- eloRunAsMatrix(x$elos, x$initial.elos, group)
  colnames(out) <- x$teams
  out
}

#' @rdname elo.run.helpers
#' @export
as.matrix.elo.run.regressed <- function(x, ..., group = x$group)
{
  group <- check_as_matrix(x, group, regr = TRUE)
  out <- eloRunRegressedAsMatrix(x$elos, x$initial.elos, x$elos.regressed,
                                 check_group_regress(x$regress),
                                 group)
  colnames(out) <- x$teams
  out
}

#' @rdname elo.run.helpers
#' @export
as.data.frame.elo.run <- function(x, ...)
{
  out <- as.data.frame(x$elos)
  colnames(out) <- c("team.A", "team.B", "p.A", "wins.A", "update.A", "update.B", "elo.A", "elo.B")
  out$team.A <- factor(out$team.A, levels = seq_along(x$teams), labels = x$teams)
  out$team.B <- factor(out$team.B, levels = seq_along(x$teams), labels = x$teams)
  out
}

#' @rdname elo.run.helpers
#' @export
final.elos <- function(x, ...)
{
  UseMethod("final.elos")
}

#' @rdname elo.run.helpers
#' @export
final.elos.elo.run <- function(x, ...)
{
  check_final_elos(x, length(x$teams))
  out <- finalElos(x$elos, length(x$teams))
  names(out) <- x$teams
  out
}

#' @rdname elo.run.helpers
#' @export
final.elos.elo.run.regressed <- function(x, regressed = FALSE, ...)
{
  if(regressed && !utils::tail(check_group_regress(x$regress), 1))
    warning("'regressed = TRUE' only makes sense if the final Elos are regressed after the final game.")
  
  if(!regressed) return(NextMethod())
  
  out <- x$elos.regressed[nrow(x$elos.regressed), ]
  names(out) <- x$teams
  out
}


check_elo_run_vars <- function(mf, initial.elos = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B
  
  if(is.numeric(t1)) stop("team.A shouldn't be numeric (team.B can be, though!)")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  all.teams <- as.character(t1)
  wts1 <- weights(t1)
  
  flag <- 2L*is.numeric(t2) # now either 2 or 0
  if(!is.numeric(t2))
  {
    if(!is.players(t2)) t2 <- players(t2)
    if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
    all.teams <- c(all.teams, as.character(t2))
    wts2 <- weights(t2)
  } else
  {
    t2 <- matrix(t2, ncol = 1)
    wts2 <- 1
  }
  
  all.teams <- sort(unique(all.teams))
  initial.elos <- check_named_elos(initial.elos, all.teams)
  
  regress <- check_group_regress(mf$regress)
  to <- check_named_elos(attr(mf$regress, "to"), all.teams)
  
  tmp <- stats::setNames(seq_along(initial.elos) - 1L, names(initial.elos))
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  if(flag != 2) t2 <- matrix(tmp[t2], nrow = nrow(t2))
  
  list(winsA = mf$wins.A, teamA = t1, teamB = t2, weightsA = wts1, weightsB = wts2,
       k = mf$k, adjTeamA = mf$adj.A, adjTeamB = mf$adj.B, regress = regress,
       to = to, by = attr(mf$regress, "by"),
       regressUnused = attr(mf$regress, "regress.unused"),
       initialElos = initial.elos, flag = flag)
}

check_named_elos <- function(init.elos = NULL, teams)
{
  sing <- length(init.elos) == 1 && is.null(names(init.elos))
  if(is.null(init.elos) || sing)
  {
    init.elos <- rep(if(sing) init.elos else 1500, times = length(teams))
    names(init.elos) <- teams
  }
  
  if(!is.numeric(init.elos)) stop("Supplied Elos should be numeric.")
  if(is.null(names(init.elos)) || anyDuplicated(names(init.elos)))
    stop("Supplied Elos should have (unique) names!")
  if(any(!(teams %in% names(init.elos))))
    stop("Some teams were found without supplied Elos.")
  
  init.elos[teams]
}

check_group_regress <- function(x, gt.zero = FALSE)
{
  if(anyNA(x)) stop("NAs found in group or regress columns.")
  if(!is.logical(x))
  {
    x <- !duplicated(x, fromLast = TRUE)
  }
  if(gt.zero)
  {
    if(sum(x) == 0) stop("At least one entry in group column must be TRUE.")
  }
  x
}

check_as_matrix <- function(x, group, regr = FALSE)
{
  stopifnot(is.matrix(x$elos), is.numeric(x$elos))
  stopifnot(is.numeric(x$initial.elos))
  stopifnot(length(x$teams) == length(x$initial.elos))
  
  if(regr)
  {
    stopifnot(is.matrix(x$elos.regressed), is.numeric(x$elos.regressed))
    stopifnot(length(x$teams) == ncol(x$elos.regressed))
  }
  
  group <- check_group_regress(group, gt.zero = TRUE)
  stopifnot(length(group) == nrow(x$elos))
  invisible(group) # to avoid checking it again later
}

check_final_elos <- function(x, len)
{
  stopifnot(is.matrix(x$elos), is.numeric(x$elos))
  stopifnot(length(x$teams) == max(x$elos[, 1:sum(x$n.players)]))
}

null_or_length0 <- function(x) is.null(x) || length(x) == 0

clean_elo_formula <- function(Terms, drop.neutral = TRUE)
{
  k.col <- attr(Terms, "specials")$k
  grp.col <- attr(Terms, "specials")$group
  reg.col <- attr(Terms, "specials")$regress
  neu.col <- attr(Terms, "specials")$neutral
  
  if(!null_or_length0(cols <- c(k.col, grp.col, reg.col, if(drop.neutral) neu.col)))
  {
    Terms <- stats::drop.terms(Terms, dropx = cols - 1, keep.response = TRUE)
  }
  stats::formula(stats::delete.response(Terms))
}



mf_to_wide <- function(mf, teams = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B
  
  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  
  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))
  if(!is.null(teams))
  {
    if(!all(all.teams %in% teams)) stop("Unknown teams: ", paste0(unique(all.teams[!(all.teams %in% teams)]), collapse = ", "))
    all.teams <- teams
  }
  
  dat <- lapply(all.teams, function(tm) (rowSums(t1 == tm) > 0) - (rowSums(t2 == tm) > 0))
  names(dat) <- all.teams
  dat$home.field <- mf$home.field
  dat <- dat[c("home.field", all.teams)] # rearrange to put home field first
  dat$adj.A <- mf$adj.A
  dat$adj.B <- mf$adj.B
  structure(dat, class = "data.frame", row.names = c(NA_integer_, nrow(mf)), all.teams = all.teams)
}

check_elo_markovchain_vars <- function(mf)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B
  
  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  
  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))
  
  tmp <- stats::setNames(seq_along(all.teams) - 1L, all.teams)
  wts1 <- weights(t1)
  wts2 <- weights(t2)
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  t2 <- matrix(tmp[t2], nrow = nrow(t2))
  
  if(!all(mf$weights > 0)) stop("Weights should be positive numbers")
  
  if(!all(0 <= mf$k & mf$k <= 1)) stop("'k' should be between 0 and 1 (inclusive)")
  winsA <- if(attr(mf, "outcome") == "mov") score(mf$wins.A, 0) else mf$wins.A
  structure(list(winsA = winsA, k = mf$k, weights = mf$weights, teamA = t1, teamB = t2,
                 weightsA = wts1, weightsB = wts2, nTeams = length(all.teams)), teams = all.teams)
}

group_to_int <- function(grp, skip)
{
  grp2 <- check_group_regress(grp, gt.zero = FALSE)
  grp2 <- rev(cumsum(rev(grp2)))
  mx <- max(grp2)
  if(skip > mx || skip < 0) stop("skip must be between 0 and ", mx, " (inclusive)")
  mx + 1 - grp2 # from mx : 1 to 1 : mx
}



check_elo_winpct_vars <- function(mf)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B
  
  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  
  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))
  
  tmp <- stats::setNames(seq_along(all.teams) - 1L, all.teams)
  wts1 <- weights(t1)
  wts2 <- weights(t2)
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  t2 <- matrix(tmp[t2], nrow = nrow(t2))
  
  if(!all(mf$weights > 0)) stop("Weights should be positive numbers")
  
  winsA <- if(attr(mf, "outcome") == "mov") score(mf$wins.A, 0) else mf$wins.A
  out <- list(winsA = winsA, weights = mf$weights, teamA = t1, teamB = t2,
              weightsA = wts1, weightsB = wts2, nTeams = length(all.teams))
  attr(out, "teams") <- all.teams
  out
}

mean_vec_subset_matrix <- function(vec, mat)
{
  rowMeans(matrix(vec[mat], nrow = nrow(mat)))
}



#' Details on \code{elo} formulas and the specials therein
#'
#' Details on \code{elo} functions and the special functions allowed in them to change functions' behaviors.
#'
#' @param x,y A vector.
#' @param adjustment A single value or a vector of the same length as \code{x}: how much to adjust the Elos in \code{x}.
#' @param to Numeric: what Elo to regress to. Can be a single value or named vector the same length
#'   as the number of teams.
#' @param by Numeric: by how much should Elos be regressed toward \code{to}.
#' @param regress.unused Logical: whether to continue regressing teams which have stopped playing.
#' @param ... Vectors to be coerced to character, which comprise of the players of a team.
#' @param weights A vector giving the weights of Elo updates for the players in \code{...}. Ignored for
#'   \code{\link{elo.glm}}.
#' @details
#' In the functions in this package, \code{formula} is usually of the form \code{wins.A ~ elo.A + elo.B},
#'   where \code{elo.A} and \code{elo.B} are vectors of Elos, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A (Elo A) won or lost (or something between). \code{elo.prob} also allows
#'   \code{elo.A} and \code{elo.B} to be character or factors, denoting which team(s) played. \code{elo.run}
#'   requires \code{elo.A} to be a vector of teams or a players matrix from \code{players()}
#'   (sometimes denoted by \code{"team.A"}), but \code{elo.B} can be either a vector of teams or
#'   players matrix (\code{"team.B"}) or else a numeric column (denoting a fixed-Elo opponent).
#'   \code{elo.glm} requires both to be a vector of teams or players matrix. \code{\link{elo.markovchain}}
#'   requires both to be a vector of teams.
#'
#' \code{formula} accepts six special functions in it:
#'
#' \code{k()} allows for complicated Elo updates. For
#'   constant Elo updates, use the \code{k = } argument instead of this special function.
#'   Note that \code{\link{elo.markovchain}} uses this function (or argument) as a convenient
#'   way of specifying transition probabilities. \code{\link{elo.colley}} uses this to indicate
#'   the fraction of a win to be assigned to the winning team.
#'
#' \code{adjust()} allows for Elos to be adjusted for, e.g., home-field advantage. The second argument
#'   to this function can be a scalar or vector of appropriate length. This can also be used in
#'   \code{\link{elo.glm}} and \code{\link{elo.markovchain}} as an adjuster to the logistic regressions.
#'
#' \code{regress()} can be used to regress Elos back to a fixed value
#'   after certain matches. Giving a logical vector identifies these matches after which to
#'   regress back to the mean. Giving any other kind of vector regresses after the appropriate
#'   groupings (see, e.g., \code{\link{duplicated}(..., fromLast = TRUE)}). The other three arguments determine
#'   what Elo to regress to (\code{to = }), by how much to regress toward that value
#'   (\code{by = }), and whether to continue regressing teams which have stopped playing (\code{regress.unused},
#'   default = \code{TRUE}).
#'
#' \code{group()} is used to group matches (by, e.g., week). It is fed to \code{\link{as.matrix.elo.run}}
#'   to produce only certain rows of matrix output. It also determines how many models to run (and on what data)
#'   for \code{\link{elo.glm}} and \code{\link{elo.markovchain}} when \code{running=TRUE}.
#'
#' \code{neutral()} is used in \code{\link{elo.glm}} and \code{\link{elo.markovchain}} to determine the intercept.
#'   In short, the intercept is \code{1 - neutral()}, denoting home-field advantage. Therefore, the column
#'   passed should be 0 (denoting home-field advatange) or 1 (denoting a neutral game). If omitted, all matches
#'   are assumed to have home field advantage.
#'
#' \code{players()} is used for multiple players on a team contributing to an overall Elo. The Elo updates
#'   are then assigned based on the specified weights. The weights are ignored in \code{\link{elo.glm}}.
#' @name formula.specials
NULL
#> NULL

#' @rdname formula.specials
#' @export
k <- function(x, y = NULL)
{
  if(!is.null(y)) x <- matrix(c(x, y), ncol = 2)
  structure(x, class = c("elo.k", class(x)))
}

remove_elo_k <- function(x)
{
  class(x) <- class(x)[!(class(x) %in% "elo.k")]
  x
}

#' @rdname formula.specials
#' @export
adjust <- function(x, adjustment) {
  if(!(length(adjustment) %in% c(1, length(x))))
    stop("The second argument to 'adjust()' needs to be length 1 or the same length as the first argument.")
  
  attr(x, "adjust") <- if(length(adjustment) == 1) rep(adjustment, times = length(x)) else adjustment
  class(x) <- c("elo.adjust", class(x))
  x
}

fix_adjust <- function(x, na.action)
{
  # why do we need this? Well, model.frame conveniently assigns the original attributes back onto vectors after na.action
  if(!is.null(na.action))
  {
    attr(x, "adjust") <- attr(x, "adjust")[-na.action]
  }
  x
}

#' @export
"[.elo.adjust" <- function(x, i, j, drop = FALSE)
{
  out <- NextMethod()
  adjust(out, attr(x, "adjust")[i])
}

#' @export
is.na.elo.adjust <- function(x)
{
  out <- NextMethod()
  out | is.na(attr(x, "adjust"))
}

remove_elo_adjust <- function(x)
{
  class(x) <- class(x)[!(class(x) %in% "elo.adjust")]
  attr(x, "adjust") <- NULL
  x
}

#' @rdname formula.specials
#' @export
regress <- function(x, to, by, regress.unused = TRUE) {
  if(!is.numeric(to) || anyNA(to)) stop("regress: 'to' must be numeric.")
  if(!is.numeric(by) || length(by) != 1 || anyNA(by) || by > 1 || by < 0)
    stop("regress: 'by' must be 0 <= by <= 1")
  if(!is.logical(regress.unused) || length(regress.unused) != 1 || anyNA(regress.unused))
    stop("regress: 'regress.unused' must be a single logical value.")
  attr(x, "to") <- to
  attr(x, "by") <- by
  attr(x, "regress.unused") <- regress.unused
  class(x) <- c("elo.regress", class(x))
  x
}

#' @export
"[.elo.regress" <- function(x, i)
{
  out <- NextMethod()
  regress(out, attr(x, "to"), attr(x, "by"), attr(x, "regress.unused"))
}

#' @rdname formula.specials
#' @export
group <- function(x) structure(x, class = c("elo.group", class(x)))

#' @rdname formula.specials
#' @export
neutral <- function(x)
{
  if(!all(x %in% c(0:1, NA))) warning("Some 'neutral()' values aren't 0 or 1.")
  structure(x, class = c("elo.neutral", class(x)))
}















































































