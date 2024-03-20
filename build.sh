#!/bin/bash
#building for osx x64

packages=("http-easy" "text-table" "json-format")

# Default target
TARGET=""

# Parse command line arguments for the target
for arg in "$@"
do
    case $arg in
        --target=*)
        TARGET="${arg#*=}"
        shift
        ;;
        *)
        ;;
    esac
done

# Check if target is specified
if [ -z "$TARGET" ]; then
    echo "Target not specified. Use --target=x86_64-macosx or --target=x86_64-linux."
    exit 1
fi

# Loop through the packages
for pkg in "${packages[@]}"; do
    output=$(raco cross --target "$TARGET" pkg show "$pkg")

    # Check if package is installed
    if echo "$output" | grep -q -v "\[none\]" && echo "$output" | grep -q "Package"; then
        echo "$pkg is already installed in $TARGET environment."
    else
        echo "$pkg is not installed on $TARGET."
        while true; do
            read -p "Do you wish to install this program? (y/n) " yn
            case $yn in
                [Yy]* ) raco cross --target "$TARGET" pkg install "$pkg"; break;;
                [Nn]* ) exit;;
                * ) echo "Please answer yes or no.";;
            esac
        done
    fi
done

raco cross --target "$TARGET" exe -o nm-cli nm-cli.rkt 
raco cross --target "$TARGET" distribute nm-cli-release-"$TARGET" nm-cli
tar -czvf nm-cli-release-"$TARGET".tar-gz nm-cli-release-"$TARGET"

rm -r nm-cli-release-"$TARGET"







