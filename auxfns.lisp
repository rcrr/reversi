;;;
;;; Copyright (c) 1991 Peter Norvig
;;; Copyright (c) 2009 Roberto Corradini

;;; This file is part of the reversi program

;;; This pogram takes origin from code
;;; found in the famous PAIP book.
;;; Code from Paradigms of AI Programming

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

(in-package #:reversi)

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

