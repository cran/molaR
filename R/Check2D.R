#' Visual check of 2D footprint triangles (HTML widget; no Quartz)
#'
#' Plot the 2D footprint triangles and their vertices used in the RFI 2D area
#' calculation, as a visual QA step to detect spurious triangles inside the
#' footprint. The result is an HTML widget (no Quartz/X11 window) with an
#' optional legend, suitable for RStudio Viewer, Quarto/HTML, and browsers.
#'
#' @param RFI_Output An object that stores the output of `RFI()`
#' @param FootColor Character; fill color for the footprint triangles (default `"red"`)
#' @param TriPointsColor Character; point color for footprint triangle vertices (default `"black"`)
#' @param main Character; plot title (default `""`)
#' @param cex Numeric; relative size multiplier for legend text (default `1`)
#' @param legend Logical; show a right-hand legend (default `TRUE`)
#' @param widget_size_px Integer; square size of the widget in pixels (default `768`)
#' @param scene_zoom Numeric; initial zoom of the 3D scene (default `1.5`)
#' @param leftOffset Numeric (-1..1); small horizontal camera nudge (default `0`)
#' @param fieldofview Numeric; field of view in degrees; 0 = isometric (default `0`)
#' @param title_font_size_px Integer; title font size in pixels (default `30`)
#' @param legend_magnify Numeric; additional legend scale factor (default `1`)
#' @param point_size Numeric; size for triangle vertex points (default `5`)
#' @param alpha Numeric between 0 and 1; transparency for the footprint fill (default `0.35`)
#'
#' @details
#' This reproduces the same intent as the classic `Check2D()`: it draws the triangles
#' from `RFI_Output$Footprint_Triangles` and the corresponding 2D coordinates in
#' `RFI_Output$Flattened_Pts`. The triangles are plotted in the **XY plane** (z = 0).
#' If you see points/triangles **inside** the footprint that shouldn’t be there,
#' it typically indicates an **alpha** value that is too small in the RFI step, leading
#' to an inflated 2D footprint.
#'
#' The function uses a headless rgl device and returns an `rglwidget` embedded in an
#' HTML layout (title atop, legend on the right)—matching the approach used in `DNE3d()`.
#'
#' @return An htmltools-browsable object containing the title, 3D widget, and legend.
#'
#' @examples
#' # RFI_out <- RFI(Tooth, alpha = 0.5)
#' # Check2D(RFI_out, FootColor = "tomato", TriPointsColor = "black",
#' # main = "RFI 2D Footprint QA")
#'
#' @import htmltools grDevices utils
#' @export
Check2D <- function(
  RFI_Output,
  FootColor = "red",
  TriPointsColor = "black",
  main = "",
  cex = 1,
  legend = TRUE,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  legend_magnify = 1,
  point_size = 5,
  alpha = 0.35
) {
  old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)
  if (!is.list(RFI_Output)) stop("RFI_Output must be a list (output of RFI()).")
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric length 1.")
  if (!is.numeric(point_size) || point_size <= 0) stop("point_size must be > 0.")
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) stop("alpha must be within [0, 1].")
  tris <- RFI_Output$Footprint_Triangles
  flat <- RFI_Output$Flattened_Pts
  if (is.null(tris) || is.null(dim(tris)) || ncol(tris) != 3)
    stop("RFI_Output$Footprint_Triangles must be an integer matrix with 3 columns.")
  if (is.null(flat) || ncol(flat) < 2)
    stop("RFI_Output$Flattened_Pts must be a 2-column matrix/data.frame with XY coordinates.")
  tris <- as.matrix(tris)
  flat <- as.matrix(flat)
  v1 <- cbind(flat[tris[, 1], 1], flat[tris[, 1], 2], 0)
  v2 <- cbind(flat[tris[, 2], 1], flat[tris[, 2], 2], 0)
  v3 <- cbind(flat[tris[, 3], 1], flat[tris[, 3], 2], 0)
  tri_vertices <- rbind(v1, v2, v3)
  pts3d <- cbind(flat[, 1], flat[, 2], 0)
  if (interactive()) {
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::triangles3d(tri_vertices, color = FootColor, alpha = alpha, specular = "black")
    rgl::lines3d(rbind(v1, v2, matrix(NA, nrow = 1, ncol = 3)),
                 color = grDevices::adjustcolor(FootColor, alpha.f = 0.9), lwd = 1)
    rgl::lines3d(rbind(v2, v3, matrix(NA, nrow = 1, ncol = 3)),
                 color = grDevices::adjustcolor(FootColor, alpha.f = 0.9), lwd = 1)
    rgl::lines3d(rbind(v3, v1, matrix(NA, nrow = 1, ncol = 3)),
                 color = grDevices::adjustcolor(FootColor, alpha.f = 0.9), lwd = 1)
    rgl::points3d(pts3d, col = TriPointsColor, size = point_size)
    rgl::view3d(fov = fieldofview)
    rgl::aspect3d("iso")
    rgl::par3d(zoom = scene_zoom)
    rgl::bg3d(col = "white")
    if (!isTRUE(all.equal(leftOffset, 0))) {
      ZView <- rgl::par3d("observer")[3]
      XView <- leftOffset * ZView * 0.05
      rgl::observer3d(XView, 0, ZView)
    }
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)
    legend_panel_px <- max(160, round(120 * cex * legend_magnify))
    legend_block <- function() {
      item <- function(color, label, border = "#333") {
        htmltools::div(
          style = "display:flex; align-items:center; gap:8px; margin-bottom:4px;",
          htmltools::tags$span(
            style = paste0(
              "display:inline-block;width:14px;height:14px;",
              "background:", color, ";border:1px solid ", border, ";"
            )
          ),
          htmltools::tags$span(
            style = paste0(
              "font-size:", round(12 * cex * legend_magnify), "px;",
              "color:#111;"
            ),
            label
          )
        )
      }
      htmltools::div(
        style = "display:flex; flex-direction:column; align-items:center;",
        htmltools::div(
          style = paste0(
            "font-weight:600;",
            "font-size:", round(12 * cex * legend_magnify), "px;",
            "color:#111; line-height:1.2; margin:0 0 6px 0; text-align:center;"
          ),
          "2D Footprint Check"
        ),
        item(grDevices::adjustcolor(FootColor, alpha.f = 0.7), "Footprint triangles", border = FootColor),
        item(TriPointsColor, "Triangle vertices")
      )
    }
    title_div <- if (nzchar(main)) {
      htmltools::div(
        style = paste0(
          "font-weight:700;",
          "font-size:", title_font_size_px, "px;",
          "line-height:1.25;",
          "margin:0 0 10px 0;",
          "text-align:center;"
        ),
        main
      )
    } else NULL
    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:row; align-items:center; gap:12px;",
          "width:", widget_size_px + legend_panel_px, "px; margin:0 auto;"
        ),
        htmltools::div(
          style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
          w
        ),
        htmltools::div(
          style = paste0("width:", legend_panel_px, "px;"),
          legend_block()
        )
      )
    } else {
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; margin:0 auto;"),
        w
      )
    }
    container <- htmltools::div(style = "width:100%;", title_div, row_div)
    htmltools::browsable(container)
  }
}
