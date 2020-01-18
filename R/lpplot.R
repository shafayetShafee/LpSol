

#' Linear Programming Solutions
#' `lpplot()` gives the minimum/maximised value and feasible region for LP problems
#'
#' @param vec
#' @param obj
#' @param crit
#'
#' @return
#' @export
#'
#' @examples
lpplot <- function(vec, obj, crit = "max") {
  mat <- matrix(vec, byrow = TRUE, ncol = 3)

  sol <- lpSolve::lp(direction = crit, objective.in = obj, const.mat = mat[,c(1,2)],
                     const.dir = rep("<=", nrow(mat)),const.rhs = mat[,3])
  soln <- sol$solution
  objval <- sol$objval

  if(crit == "min") {
    message(sprintf("The objective function z = %dx + %dy is minimised at x = %.2f and y = %.2f", obj[1], obj[2], soln[1], soln[2]))
    message(sprintf("And the maximum value is %.2f", objval))
  }
  else if(crit == "max") {
    message(sprintf("The objective function z = %dx + %dy is maximised at x = %.2f and y = %.2f", obj[1], obj[2], soln[1], soln[2]))
    message(sprintf("And the maximum value is %.2f", objval))
  }

  gMOIP::plotPolytope(
    mat[,c(1,2)],
    mat[,3],
    crit = crit,
    obj,
    plotFeasible = TRUE,
    plotOptimum = TRUE,
    labels = "coord"
  )+
    ggplot2::theme_bw()+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(color = "grey75")
    )+ ggplot2::xlab("X") + ggplot2::ylab("Y")

}

