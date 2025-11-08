#' Plot 3D and 2D areas of a mesh used to calculate relief index (HTML widget)
#'
#' Renders a three-dimensional model of the mesh surface and a footprint of the
#' two-dimensional area side-by-side for visual comparison. The `RFI()` function
#' must be performed prior to using `RFI3d()`.
#'
#' @param RFI_File An object that stores the output of the RFI function
#' @param displacement Numeric that moves the surface footprint some proportion
#'   of the height of the mesh. 0 is vertical center; negative values displace
#'   the footprint downward. Default -1.9.
#' @param SurfaceColor Color for the 3D surface mesh
#' @param FootColor Color for the 2D surface footprint
#' @param FootPts Logical; if TRUE, plot the flattened points used for the footprint
#' @param FootPtsColor Color of plotted footprint points when `FootPts = TRUE`
#' @param Opacity Numeric from 0 to 1 controlling the opacity of the 3D surface
#' @param legend Logical; draw a legend (right side of the widget)
#' @param main Plot title
#' @param cex Base scaling for legend/ticks (relative multiplier)
#' @param widget_size_px Square widget size in pixels (default 768)
#' @param scene_zoom Initial zoom for the 3d scene (default 1.5)
#' @param leftOffset Horizontal camera nudge (-1..1 recommended)
#' @param fieldofview Field of view in degrees (0 = isometric)
#' @param title_font_size_px Title font size in pixels (default 30)
#' @param title_font_family CSS font-family list for the title
#' @param legend_magnify Additional scale factor for the legend text
#' @param fileName Optional file base name to save a colored *.ply (no legend)
#' @param binary Binary PLY if TRUE (smaller files)
#'
#' @details
#'
#' This function helps visualize the 3D surface area (numerator) and the 2D
#' projected area (denominator) that comprise the relief index, by displaying
#' both at once. Adjust `Opacity` to make the footprint more visible through
#' the surface. The footprint plane can be moved along Z via `displacement`.
#'
#' The plotting window is an HTML widget. The legend is rendered in HTML/CSS
#' and aligned vertically at the right of the scene.
#'
#' @import grDevices graphics utils
#' @import htmltools
#' @export
#' @examples
#' if(interactive()){
#'   rfi <- RFI(Tooth, alpha = 0.5)
#'   RFI3d(rfi)
#' }
RFI3d <- function(
  RFI_File,
  displacement = -1.9,
  SurfaceColor = "gray",
  FootColor = "red",
  FootPts = FALSE,
  FootPtsColor = "black",
  Opacity = 1,
  legend = TRUE,
  main = "",
  cex = 1,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  title_font_family = "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif",
  legend_magnify = 1,
  fileName = NA,
  binary = FALSE
){
  # ---- inputs & validation ---------------------------------------------------
  plyFile <- RFI_File$plyFile
  if (!inherits(plyFile, "mesh3d"))
    stop("RFI_File$plyFile must be an rgl 'mesh3d'.")
  if (!is.numeric(displacement) || length(displacement) != 1L)
    stop("displacement must be numeric length 1.")
  if (!is.numeric(Opacity) || length(Opacity) != 1L || Opacity < 0 || Opacity > 1)
    stop("Opacity must be a number in [0, 1].")
  if (!is.logical(FootPts) || length(FootPts) != 1L)
    stop("FootPts must be TRUE/FALSE.")
  if (!is.logical(legend) || length(legend) != 1L)
    stop("legend must be TRUE/FALSE.")
  if (!is.numeric(cex) || cex <= 0)
    stop("cex must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0)
    stop("scene_zoom must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0)
    stop("widget_size_px must be > 0.")
  if (!is.numeric(title_font_size_px) || title_font_size_px <= 0)
    stop("title_font_size_px must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0)
    stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L)
    stop("leftOffset must be numeric length 1.")

  # ---- center mesh & small left bias to accommodate legend ------------------
  vb <- plyFile$vb
  mesh_centered <- plyFile
  xr <- range(vb[1, ], na.rm = TRUE)
  zr <- range(vb[3, ], na.rm = TRUE)
  T_center_xz   <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)

  LEFT_BIAS_FRAC <- 0.08
  vb2    <- mesh_centered$vb
  xr2    <- range(vb2[1, ], na.rm = TRUE)
  x_width <- diff(xr2)
  x_left  <- -LEFT_BIAS_FRAC * x_width
  T_left  <- rgl::translationMatrix(x_left, 0, 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_left)

  # center in y
  vb3 <- mesh_centered$vb
  yr  <- range(vb3[2, ], na.rm = TRUE)
  T_center_y   <- rgl::translationMatrix(0, -mean(yr), 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)

  # ---- footprint geometry ----------------------------------------------------
  FootprintPts <- RFI_File$Flattened_Pts
  MeshHeight   <- abs(max(plyFile$vb[3, ]) - min(plyFile$vb[3, ]))
  displaceDist <- displacement * 0.5 * MeshHeight
  zpts <- FootprintPts[, 3] + displaceDist
  xyz  <- cbind(FootprintPts[, 1:2], zpts)
  FootprintVertices <- t(cbind(xyz, rep(1, length(zpts))))
  triangles <- t(RFI_File$Footprint_Triangles)
  Footprint <- list(vb = FootprintVertices, it = triangles,
                    primitivetype = "triangle", material = NULL)
  class(Footprint) <- c("mesh3d", "shape3d")

  # ---- optional: save colored PLY (surface only; no legend) -----------------
  if (!is.na(fileName)) {
    if (!is.character(fileName) || length(fileName) != 1L)
      stop("Enter a single character string for fileName.")
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply") {
      fileName <- paste0(fileName, ".ply")
    }
    # Save the surface with its uniform color
    outMesh <- mesh_centered
    outMesh$material$color <- SurfaceColor
    Rvcg::vcgPlyWrite(mesh = outMesh, filename = fileName, binary = binary)
    if (!binary) {
      path <- file.path(getwd(), fileName)
      txt  <- readLines(con = path, warn = FALSE)
      com  <- paste("comment RFI plot generated in molaR",
                    utils::packageVersion("molaR"), "for", R.version.string)
      com  <- unlist(strsplit(com, split = "\n"))
      out  <- c(txt[1:3], com, txt[4:length(txt)])
      writeLines(out, con = path)
    }
  }

  # ============================ INTERACTIVE ONLY ==============================
  if (interactive()) {
    # rgl device setup (headless): minimal, scoped, auto-restore
    old_opts <- options(c("rgl.useNULL", "rgl.printRglwidget"))
    options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
    on.exit(options(old_opts), add = TRUE)

    # --- scene ----------------------------------------------------------------
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, meshColor = "faces",
                 color = SurfaceColor, alpha = Opacity, shininess = 110)
    rgl::shade3d(Footprint, meshColor = "faces", color = FootColor)
    if (isTRUE(FootPts)) rgl::points3d(xyz, color = FootPtsColor)
    rgl::view3d(fov = fieldofview)
    rgl::aspect3d("iso")
    rgl::par3d(zoom = scene_zoom)
    rgl::bg3d(col = "white")

    # Optional small horizontal camera nudge
    if (!isTRUE(all.equal(leftOffset, 0))) {
      ZView <- rgl::par3d("observer")[3]
      XView <- leftOffset * ZView * 0.05
      rgl::observer3d(XView, 0, ZView)
    }

    # rglwidget
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)

    # HTML legend (vertical, right)
    rfi_legend_html <- function(surfCol, footCol, footPts, footPtsCol,
                                cex = 1, magnify = 1,
                                font_family = title_font_family,
                                box_px = 14,
                                spacing_px = 6) {
      font_px <- 12 * cex * magnify
      swatch <- function(color) {
        htmltools::tags$span(
          style = paste0(
            "display:inline-block;width:", box_px, "px;height:", box_px, "px;",
            "background:", color, ";border:1px solid #999;margin-right:8px;"
          )
        )
      }
      pt_swatch <- function(color) {
        htmltools::tags$span(
          style = paste0(
            "display:inline-block;width:", box_px, "px;height:", box_px, "px;",
            "border-radius:", floor(box_px/2), "px;background:", color, ";",
            "border:1px solid #999;margin-right:8px;"
          )
        )
      }
      rows <- list(
        htmltools::div(
          style = paste0("display:flex;align-items:center;gap:8px;margin-bottom:", spacing_px, "px;"),
          swatch(surfCol), htmltools::tags$span("Surface")
        ),
        htmltools::div(
          style = paste0("display:flex;align-items:center;gap:8px;margin-bottom:", spacing_px, "px;"),
          swatch(footCol), htmltools::tags$span("Footprint")
        )
      )
      if (isTRUE(footPts)) {
        rows <- c(rows, list(
          htmltools::div(
            style = "display:flex;align-items:center;gap:8px;",
            pt_swatch(footPtsCol), htmltools::tags$span("Footprint points")
          )
        ))
      }
      htmltools::div(
        style = paste0(
          "display:flex;flex-direction:column;gap:", spacing_px + 2, "px;",
          "font-family:", font_family, ";font-size:", round(font_px), "px;color:#222;"
        ),
        htmltools::div(
          style = paste0(
            "font-weight:600;font-size:", round(font_px * 1.05), "px;",
            "line-height:1.2;text-align:center;margin-bottom:", spacing_px, "px;"
          ),
          "Layers"
        ),
        rows,
        htmltools::div(
          style = paste0("margin-top:", spacing_px + 2, "px;color:#444;"),
          paste0("Opacity (surface): ", format(Opacity, trim = TRUE))
        )
      )
    }

    # Title (optional)
    title_div <- if (nzchar(main)) {
      htmltools::div(
        style = paste0(
          "font-weight:700;",
          "font-size:", title_font_size_px, "px;",
          "font-family:", title_font_family, ";",
          "line-height:1.25;",
          "margin:0 0 10px 0;",
          "text-align:center;"
        ),
        main
      )
    } else NULL

    # Right-column legend width
    legend_panel_px <- max(140, round(70 * cex * legend_magnify))

    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:row; align-items:center; gap:12px; ",
          "width:", widget_size_px + legend_panel_px, "px; margin:0 auto;"
        ),
        # Left: 3D scene
        htmltools::div(
          style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
          w
        ),
        # Right: vertical legend
        htmltools::div(
          style = paste0("width:", legend_panel_px, "px;"),
          rfi_legend_html(SurfaceColor, FootColor, FootPts, FootPtsColor,
                          cex = cex, magnify = legend_magnify,
                          font_family = title_font_family)
        )
      )
    } else {
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; margin:0 auto;"),
        w
      )
    }

    container <- htmltools::div(
      style = "width:100%;",
      title_div,
      row_div
    )

    return(htmltools::browsable(container))
  }

  # ---------------------- NON-INTERACTIVE RETURN ------------------------------
  # No HTML or widget created; return a lightweight spec
  out <- structure(
    list(
      title            = main,
      surface_color    = SurfaceColor,
      foot_color       = FootColor,
      foot_points      = isTRUE(FootPts),
      foot_points_color= FootPtsColor,
      opacity          = Opacity,
      displacement     = displacement,
      legend           = isTRUE(legend),
      widget_size_px   = widget_size_px,
      scene_zoom       = scene_zoom,
      fieldofview      = fieldofview
    ),
    class = "RFI3d_spec"
  )
  invisible(out)
}
