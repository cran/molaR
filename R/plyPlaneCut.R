#' Cut a PLY Mesh Along a Specified Plane (HTML widget; no Quartz)
#'
#' Cuts a mesh by a plane and returns either one side or both sides of the cut.
#' The result is also previewed as an HTML widget (no Quartz/X11) with the kept
#' side in solid color, the other side semi-transparent, and the cutting plane shown.
#'
#' @param plyFile An object of class 'mesh3d'.
#' @param axis Character, one of "X","Y","Z"; used when deriving the plane from a
#'   vertex index or a numeric cut value (ignored when 'plane' is supplied). Default "Z".
#' @param vertIndex Integer index of a mesh vertex used to place an orthogonal plane
#'   through that vertex along 'axis'. Ignored if 'plane' or 'cut_value' is supplied.
#' @param cut_value Numeric coordinate along 'axis' at which to place the orthogonal
#'   plane. Ignored if 'plane' is supplied. (HTML-friendly alternative to interactive pick)
#' @param keepBoth Logical; if TRUE and the plane intersects the mesh, return both
#'   sides as a list with elements 'meshA' and 'meshB'. Default FALSE.
#' @param plane Numeric vector c(a,b,c,d) giving a plane in ax + by + cz + d = 0 form.
#'   If provided, overrides 'axis', 'vertIndex', and 'cut_value'.
#' @param flipAxis Logical; reverse the plane normal direction used to decide which
#'   side is kept when keepBoth = FALSE. Default FALSE.
#' @param display Logical; if TRUE, render an HTML widget preview. Default TRUE.
#'
#' @param kept_col Color for the kept portion in the preview (default "#3C8DFF").
#' @param other_col Color for the other portion in the preview (default "gray70").
#' @param plane_col Color for the plane polygon in the preview (default "firebrick").
#' @param plane_alpha Alpha for the plane polygon (default 0.35).
#'
#' @param main Plot title (default "")
#' @param widget_size_px Square widget size in pixels (default 768)
#' @param scene_zoom Initial zoom for the 3D scene (default 1.5)
#' @param leftOffset Horizontal camera nudge (-1..1 recommended) (default 0)
#' @param fieldofview Field of view in degrees (0 = isometric) (default 0)
#' @param title_font_size_px Title font size in pixels (default 30)
#' @param title_font_family CSS font-family list for the title (default system UI stack)
#'
#' @return If keepBoth = FALSE, a 'mesh3d' for the retained side.
#'   If keepBoth = TRUE, a list with elements 'meshA' and 'meshB'.
#'   The returned object also carries an attribute 'widget' with the
#'   `htmltools::browsable` preview when `display = TRUE`.
#'
#' @details
#' This HTML-widget version avoids opening a native rgl window and mirrors the
#' device-and-layout pattern you used in your updated `DNE3d`. Interactive vertex
#' picking (`select3d`) isn’t supported in widget mode; supply the cut as `plane`,
#' `vertIndex` + `axis`, or `cut_value` + `axis`. The kept side selection is
#' controlled by the normal direction and `flipAxis`.
#'
#' @import htmltools
#' @export
plyPlaneCut <- function(
  plyFile,
  axis = "Z",
  vertIndex = NA,
  cut_value = NA,
  keepBoth = FALSE,
  plane = NA,
  flipAxis = FALSE,
  display = TRUE,
  kept_col = "#3C8DFF",
  other_col = "gray70",
  plane_col = "firebrick",
  plane_alpha = 0.35,
  main = "",
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  title_font_family = "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif"
) {
  # ---- rgl device setup (headless): minimal, scoped, auto-restore ------------
  old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)

  # ---- validation & helpers ---------------------------------------------------
  if (!inherits(plyFile, "mesh3d"))
    stop("plyFile must be an rgl 'mesh3d' object.")

  axis_to_dim <- function(ax) {
    ax <- toupper(trimws(ax))
    if (ax %in% c("X","Y","Z")) return(match(ax, c("X","Y","Z")))
    stop("axis must be one of 'X','Y','Z'.")
  }
  dim_idx <- axis_to_dim(axis)

  vb    <- plyFile$vb
  verts <- t(vb[1:3, , drop = FALSE])

  # Determine plane coefficients (a,b,c,d)
  use_plane <- !(length(plane) == 1 && is.na(plane))
  if (use_plane) {
    if (!is.numeric(plane) || length(plane) != 4)
      stop("plane must be numeric c(a,b,c,d) for ax + by + cz + d = 0")
    abc <- plane[1:3]
    d   <- plane[4]
    if (isTRUE(flipAxis)) abc <- -abc
    plane_abcd <- c(abc, d)
  } else {
    # derive orthogonal plane from vertIndex or cut_value along 'axis'
    if (!is.na(vertIndex)) {
      if (vertIndex < 1 || vertIndex > nrow(verts))
        stop("vertIndex out of range for vertices.")
      cut_at <- verts[vertIndex, dim_idx]
    } else if (!is.na(cut_value)) {
      if (!is.numeric(cut_value) || length(cut_value) != 1L)
        stop("cut_value must be a single numeric coordinate.")
      cut_at <- cut_value
    } else {
      stop("Provide either 'plane', 'vertIndex', or 'cut_value' (HTML mode does not support interactive selection).")
    }
    nrm <- c(0,0,0); nrm[dim_idx] <- if (isTRUE(flipAxis)) -1 else 1
    # ax+by+cz+d=0; through coordinate 'cut_at' on chosen axis -> d = -cut_at * nrm[dim_idx]
    d <- -cut_at * nrm[dim_idx]
    plane_abcd <- c(nrm, d)
  }

  # ---- perform the clip (meshClip expected to return list(meshA, meshB)) -----
  # Note: mirrors original logic; we don't open an interactive display
  newMesh <- meshClip(plyFile, plane_abcd[1], plane_abcd[2], plane_abcd[3], plane_abcd[4])
  if (is.null(newMesh$meshA)) warning("Entire mesh is below plane")
  if (is.null(newMesh$meshB)) warning("Entire mesh is above plane")

  if (isTRUE(keepBoth)) {
    out <- newMesh
  } else {
    # take first non-NULL side
    out <- if (is.null(newMesh[[1]])) newMesh[[2]] else newMesh[[1]]
  }

  # ---- HTML preview (no Quartz) ----------------------------------------------
  widget_obj <- NULL
  if (isTRUE(display)) {
    mesh_center <- function(mesh) {
      if (is.null(mesh)) return(NULL)
      m  <- mesh
      xr <- range(m$vb[1, ], na.rm = TRUE)
      zr <- range(m$vb[3, ], na.rm = TRUE)
      T_center_xz <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
      m  <- rgl::transform3d(m, T_center_xz)
      LEFT_BIAS_FRAC <- 0.08
      xr2    <- range(m$vb[1, ], na.rm = TRUE)
      x_left <- -LEFT_BIAS_FRAC * diff(xr2)
      m  <- rgl::transform3d(m, rgl::translationMatrix(x_left, 0, 0))
      yr <- range(m$vb[2, ], na.rm = TRUE)
      T_center_y <- rgl::translationMatrix(0, -mean(yr), 0)
      rgl::transform3d(m, T_center_y)
    }

    A <- mesh_center(newMesh$meshA)
    B <- mesh_center(newMesh$meshB)

    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)

    if (!is.null(A)) rgl::shade3d(A, color = kept_col, meshColor = "faces", shininess = 110)
    if (!is.null(B)) rgl::shade3d(B, color = other_col, meshColor = "faces", alpha = 0.33, shininess = 60)

    # plane visual cue (finite display grid generated by rgl)
    rgl::planes3d(plane_abcd[1], plane_abcd[2], plane_abcd[3], plane_abcd[4],
                  col = plane_col, alpha = plane_alpha)

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

    row_div <- htmltools::div(
      style = paste0(
        "display:flex; flex-direction:row; align-items:center; justify-content:center;",
        "width:", widget_size_px, "px; margin:0 auto;"
      ),
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
        w
      )
    )

    container  <- htmltools::div(style = "width:100%;", title_div, row_div)
    widget_obj <- htmltools::browsable(container)
    if (interactive()) print(widget_obj)
  }



  attr(out, "widget") <- widget_obj
  return(out)
}
