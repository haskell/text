#!/bin/bash
set -eu
cd "${BASH_SOURCE%/*}/"
wget http://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
runghc-9.2 CaseMapping
rm SpecialCasing.txt CaseFolding.txt
cd -
