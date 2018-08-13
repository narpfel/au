`Au` -- Monadic Parser Combinators
==================================

`Au` is a small clone of the `parsec` parser combinator library, written to learn
about mondic parser combinators.

The `Au.Impl` module contains applications of `Au` to parse real world examples.


TODO
----

* Error handling: Currently, the parser just returns `None` on a failed parse.
* Generating parsers and parse tree data structures based on a textual description
  of the grammar.


License
-------

`Au` is licensed under version 3 (or any later version) of the GPL:

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
