;;;
;;; Copyright (c) 1998-2002 Peter Norvig
;;; Copyright (c) 2009 Roberto Corradini

;;; This file is part of the reversi program
;;; http://github.com/rcrr/reversi

;;; This pogram takes origin from the code
;;; described in the Peter Norvig's PAIP book (Paradigms of AI Programming).
;;; The internet location of the book is "http://norvig.com/paip.html"
;;; The page where to download the source code is "http://norvig.com/paip/README.html"
;;; The licence agreement page is located at "http://norvig.com/license.html"
;;; 
;;; Here the licence is reported:
;;; Copyright © 1998-2002 by Peter Norvig.

;;; Permission is granted to anyone to use this software, in source or object code form,
;;; on any computer system, and to modify, compile, decompile, run, and redistribute
;;; it to anyone else, subject to the following restrictions:
;;;   1. The author makes no warranty of any kind, either expressed or implied,
;;;      about the suitability of this software for any purpose.
;;;   2. The author accepts no liability of any kind for damages or other
;;;      consequences of the use of this software, even if they arise from
;;;      defects in the software.
;;;   3. The origin of this software must not be misrepresented,
;;;      either by explicit claim or by omission.
;;;   4. Altered versions must be plainly marked as such,
;;;      and must not be misrepresented as being the original software.
;;;      Altered versions may be distributed in packages
;;;      under other licenses (such as the GNU license). 
;;; If you find this software useful, it would be nice if you let me (peter@norvig.com)
;;; know about it, and nicer still if you send me modifications that you
;;; are willing to share. However, you are not required to do so.

;;; As stated in the original license, the modified version, this file, is
;;; distributed under the GNU GPL v3 license.

;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

(in-package :reversi)

;;; As stated into the SBCL manual (ref. 2.3.4 Defining Constants)
;;; the compiler is quite strict in enforcing at compile time
;;; that the new value assigned to the constant has to be eql to
;;; the old one.
;;; The macro definition is taken from the manual itself
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;; PAIP 18.2 - Representation Choices (p. 601)
(define-constant all-directions '(-11 -10 -9 -1 1 9 10 11))

(define-constant empty 0 "An empty square")
(define-constant black 1 "A black piece")
(define-constant white 2 "A white piece")
(define-constant outer 3 "Marks squares outside the board")

;;; PAIP 18.2 - Representation Choices (p. 603)
(define-constant all-squares
    (loop for i from 11 to 88
       when (<= 1 (mod i 10) 8) collect i))

;;; PAIP 18.4 - Searching Ahead: Minimax (p. 613)
(define-constant winning-value most-positive-fixnum)
(define-constant losing-value most-negative-fixnum)

(define-constant edge-and-x-lists
    '((22 11 12 13 14 15 16 17 18 27)
      (72 81 82 83 84 85 86 87 88 77)
      (22 11 21 31 41 51 61 71 81 72)
      (27 18 28 38 48 58 68 78 88 77))
  "The four edges (with their X-squares)")

(define-constant top-edge (first edge-and-x-lists))