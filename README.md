# energy-performance-haringey


To render the shiny live app. 
execute 
shinylive::export(appdir = "myapp", destdir = "docs")
then 
httpuv::runStaticServer("docs/", port=8008)
