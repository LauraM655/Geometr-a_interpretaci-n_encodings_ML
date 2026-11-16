# Carga el nucleo desde la raiz del proyecto (test_dir fija el wd en esta carpeta)
for (f in c("core.R", "geometry.R", "geometry_plots.R")) {
  source(file.path("..", "..", "R", f), encoding = "UTF-8")
}