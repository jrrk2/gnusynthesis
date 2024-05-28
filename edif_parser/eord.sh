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

echo open String > eord.ml
echo let esymbols = Hashtbl.create 256 >> eord.ml
echo 'let _ = List.iter (fun (str,key) -> if str <> "" then Hashtbl.add esymbols (lowercase_ascii str) key) [' >> eord.ml
grep \| edif2.mli | tr '\011|' ' ' | sed -e 's=[\ ]*\([A-Z][A-Za-z0-9_]*\)=("\1", Edif2.\1=' -e 's= of (unit)= ()=' -e 's=$=);=' | grep -v ' of ' >> eord.ml
echo '("", Edif2.EMPTY)]' >> eord.ml
echo let getstr tok = match tok with >> eord.ml
grep \| edif2.mli | cut -d\( -f1 | tr '\011' ' ' |\
sed -e 's=|[\ ]*\([A-Z][A-Za-z0-9_\ of]*\)=|\ Edif2.\1 -> (\"\1\")=' -e 's= of= arg=' -e 's= of ==' >> eord.ml
