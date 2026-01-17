#!/bin/bash
set -exu
cd "${BASH_SOURCE%/*}/"
wget http://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
runghc CaseMapping
rm SpecialCasing.txt CaseFolding.txt UnicodeData.txt
cd -
