#    <vscr - Verilog converter to abc format.>
#    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

echo open String > exprkind.ml
echo let getstr tok = match tok with >> exprkind.ml
grep '\ [A-Z][A-Z_]' < $1 | cut -d\( -f1 | tr '\011' ' ' |\
grep -v _TYPE |\
sed -e 's=[|\ ]*\([A-Z][A-Z0-9_\ o]*\)=|\ Libstp.\1 -> lowercase(\"\1\") \;=' -e 's= o= _=' -e 's= o==' | cut -d\; -f1 | sort >> exprkind.ml
