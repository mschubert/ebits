.b = import('../base')
.io = import('../io')

path = function(mod_str=NULL) {
    getOption(paste("data.dir", mod_str, sep=".")) %or%
        .io$file_path(getOption("data.dir"), mod_str) %or%
        .io$file_path(module_file(), mod_str)
}

file = function(mod_str, file, ...) {
    .io$file_path(path(mod_str), file, ...)
}

load = function(mod_str, file) {
    .io$load(file(mod_str, file))
}

read = function(mod_str, file, ...) {
    .io$read_table(file(mod_str, file), ...)
}

exists = function(mod_str, file, ...) {
    file.exists(file(mod_str, file, ...))
}
