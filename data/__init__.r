# export all submodules using their module names
.lsdir = list.dirs(module_file(), recursive=FALSE)
for (dir in .lsdir[file.info(.lsdir)$isdir])
    assign(basename(dir), import(paste("./", basename(dir), sep="")))
