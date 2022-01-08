#!/bin/bash
set -euo pipefail

mkdir "day$1"

cd "day$1"

dotnet new console -lang "F#" 

dotnet new tool-manifest
dotnet tool install --local fantomas-tool --version 4.6.0-alpha-010

touch test.txt
touch input.txt