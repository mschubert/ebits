#!/bin/sh
scripts=$(find . -name "*.r" -o -name "*.R")
content=$(echo $scripts | xargs cat)

regex=(
    "(?<=library\()[a-zA-Z0-9.]+|"
    "(?<=require\()[a-zA-Z0-9.]+|"
    "(?<=requireNamespace\()[a-zA-Z0-9.]+|"
    "(?<=import_package\()[a-zA-Z0-9.]+|"
    "[a-zA-Z0-9.]+(?=:::?)"
)
combined=$(printf "%s" "${regex[@]}")

echo $content | tr -d "'\"" | grep -o -P "$combined" | sort | uniq
