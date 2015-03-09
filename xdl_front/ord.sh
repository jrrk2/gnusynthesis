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

echo open String > ord.ml
echo let getstr tok = match tok with >> ord.ml
sed '/and/,$d' xdl_parser.mli | grep -v '^val' | grep '\ [A-Z][A-Za-z_]' | grep -v exception | grep -v menhir | cut -d\( -f1 | tr '\011' ' ' |\
sed -e 's=[|\ ]*\([A-Z][A-Za-z0-9_\ o]*\)=|\ Xdl_parser.\1 -> \"\1\" \;=' -e 's= of [A-Za-z0-9\ ]*= _=' -e 's= of[A-Za-z0-9\ ]*==' | cut -d\; -f1 | sort >> ord.ml
