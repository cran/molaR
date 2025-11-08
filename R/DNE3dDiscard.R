#' Plot discarded-face results of a DNE analysis (HTML widget; no Quartz)
#'
#' Produces a three-dimensional rendering that highlights the faces discarded from
#' the DNE calculation---boundary faces and statistical outliers---along with optional
#' concavity marking. The plot is returned as an HTML widget with a simple, readable
#' legend panel to the right.
#'
#' Run `DNE()` first to compute per-face values and discard sets.
#'
#' @param DNE_File An object that stores the output of `DNE()`
#' @param baseCol Base color for typical faces (default `"gray"`)
#' @param boundCol Color for boundary faces discarded from DNE (default `"red"`)
#' @param outlierCol Color for faces discarded as outliers (default `"lawngreen"`)
#' @param concaveCol Color for concave faces; when left equal to `baseCol` the
#' concave category is not distinguished or shown in the legend (default = `baseCol`)
#' @param main Character; plot title (default `""`)
#' @param cex Numeric; relative size multiplier for legend text (default `1`)
#' @param legend Logical; show legend panel (default `TRUE`)
#' @param widget_size_px Integer; square size of the 3D widget in pixels (default `768`)
#' @param scene_zoom Numeric; initial zoom on the 3D scene (default `1.5`)
#' @param leftOffset Numeric (-1..1); small horizontal camera nudge (default `0`)
#' @param fieldofview Numeric; field of view in degrees; 0 = isometric (default `0`)
#' @param title_font_size_px Integer; title font size in pixels (default `30`)
#' @param legend_magnify Numeric; additional legend scale factor (default `1`)
#' @param legendTextCol Color for legend text (default `"black"`)
#' @param legendLineCol Color for legend swatch borders/lines (default `"black"`)
#' @param fileName Character or `NA`; if non-`NA`, write a colorized *.ply to disk (default `NA`)
#' @param binary Logical; write PLY as binary (default `FALSE`, i.e., ascii)
#'
#' @details
#' **What this shows**
#' * *Boundary faces* (e.g., with >=1 boundary vertex) and *outlier faces* (top extreme
#' tail) that were excluded from DNE are colored distinctly; all other faces get
#' `baseCol`. If `concaveCol` differs from `baseCol`, concave regions are tinted
#' accordingly and labeled in the legend.
#'
#' **HTML widget**
#' * Uses a headless rgl device and returns an `rglwidget` embedded beside a simple HTML
#' legend (no Quartz window). Layout and centering mirror the updated `DNE3d`.
#'
#' **Saving a PLY**
#' * If `fileName` is provided, a colorized PLY is written. For ascii PLY, a comment
#' line noting the generator/version is inserted (as in the updated `DNE3d`).
#'
#' @return An htmltools-browsable object containing the title, 3D widget, and legend.
#'
#' @examples
#' # DNE_out <- DNE(Tooth)
#' if(interactive()){DNE3dDiscard(DNE_out, main = "DNE -- Discarded Faces")}
#'
#' @import htmltools grDevices utils
#' @importFrom Rvcg vcgPlyWrite
#' @export
DNE3dDiscard <- function(
  DNE_File,
  baseCol = "gray",
  boundCol = "red",
  outlierCol = "lawngreen",
  concaveCol = baseCol,
  main = "",
  cex = 1,
  legend = TRUE,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  legend_magnify = 1,
  legendTextCol = "black",
  legendLineCol = "black",
  fileName = NA,
  binary = FALSE
) {
  options(rgl.useNULL = TRUE)
  old <- options(rgl.printRglwidget = FALSE)
  on.exit(options(old), add = TRUE)
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric length 1.")
  if (!is.logical(legend) || length(legend) != 1L) stop("legend must be TRUE/FALSE.")
  plyFile <- DNE_File$plyFile
  if (!inherits(plyFile, "mesh3d")) stop("DNE_File$plyFile must be an rgl mesh3d.")
  k <- DNE_File$Kappa
  n_faces <- length(DNE_File$Face_Values$Face_Areas)
  DNE_colors <- rep(baseCol, length.out = n_faces)
  inflection <- which(DNE_File$Face_Values$Kappa_Values <= k)
  if (length(inflection)) DNE_colors[inflection] <- concaveCol
  edges <- as.numeric(rownames(DNE_File$Boundary_Values))
  if (length(edges)) DNE_colors[edges] <- boundCol
  outliers <- as.numeric(rownames(DNE_File$Outliers))
  if (length(outliers)) DNE_colors[outliers] <- outlierCol
  vb <- plyFile$vb
  mesh_centered <- plyFile
  xr <- range(vb[1, ], na.rm = TRUE)
  zr <- range(vb[3, ], na.rm = TRUE)
  T_center_xz <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)
  {
    LEFT_BIAS_FRAC <- 0.08
    vb2 <- mesh_centered$vb
    xr2 <- range(vb2[1, ], na.rm = TRUE)
    x_width <- diff(xr2)
    x_left <- -LEFT_BIAS_FRAC * x_width
    T_left <- rgl::translationMatrix(x_left, 0, 0)
    mesh_centered <- rgl::transform3d(mesh_centered, T_left)
  }
  vb3 <- mesh_centered$vb
  yr <- range(vb3[2, ], na.rm = TRUE)
  T_center_y <- rgl::translationMatrix(0, -mean(yr), 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)
  if (interactive()) {
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, color = DNE_colors, meshColor = "faces", shininess = 110)
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
    legend_html <- function(
      baseCol, boundCol, outlierCol, concaveCol, show_concave,
      cex = 1, magnify = 1, text_col = "black", line_col = "black",
      legend_title = "DNE Discarded Faces"
    ) {
      item <- function(color, label) {
        htmltools::div(
          style = "display:flex; align-items:center; gap:8px;",
          htmltools::tags$span(
            style = paste0(
              "display:inline-block;width:14px;height:14px;",
              "background:", color, ";border:1px solid ", line_col, ";"
            )
          ),
          htmltools::tags$span(
            style = paste0(
              "font-size:", round(12 * cex * magnify), "px;",
              "color:", text_col, ";"
            ),
            label
          )
        )
      }
      htmltools::div(
        style = "display:flex; flex-direction:column; align-items:center; gap:8px;",
        htmltools::div(
          style = paste0(
            "font-weight:600;",
            "font-size:", round(12 * cex * magnify), "px;",
            "color:", text_col, ";",
            "line-height:1.2; margin:0 0 4px 0; text-align:center;"
          ),
          legend_title
        ),
        item(baseCol, "Base"),
        item(boundCol, "Boundary (discarded)"),
        item(outlierCol, "Outlier (discarded)"),
        if (isTRUE(show_concave)) item(concaveCol, "Concave")
      )
    }
    show_concave <- !isTRUE(all.equal(concaveCol, baseCol))
    legend_panel_px <- max(160, round(120 * cex * legend_magnify))
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
          legend_html(
            baseCol, boundCol, outlierCol, concaveCol, show_concave,
            cex = cex, magnify = legend_magnify,
            text_col = legendTextCol, line_col = legendLineCol
          )
        )
      )
    } else {
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; margin:0 auto;"),
        w
      )
    }
    container <- htmltools::div(style = "width:100%;", title_div, row_div)
    if (!is.na(fileName)) {
      if (!is.character(fileName) || length(fileName) != 1L)
        stop("Enter a single character string for fileName.")
      if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply")
        fileName <- paste0(fileName, ".ply")
      plyFile$material$color <- DNE_colors
      Rvcg::vcgPlyWrite(mesh = plyFile, filename = fileName, binary = binary)
      if (!binary) {
        path <- file.path(getwd(), fileName)
        txt <- readLines(con = path, warn = FALSE)
        com <- paste(
          "comment DNE plot generated in molaR",
          utils::packageVersion("molaR"), "for", R.version.string
        )
        com <- unlist(strsplit(com, split = "\n"))
        out <- c(txt[1:3], com, txt[4:length(txt)])
        writeLines(out, con = path)
      }
    }
    htmltools::browsable(container)
  }
}
