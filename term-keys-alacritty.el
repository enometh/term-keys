;;; term-keys-alacritty.el --- term-keys support for Alacritty

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains supplementary code for aiding in the
;; configuration of the Alacritty terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defgroup term-keys/alacritty nil
  "`term-keys' options for ALACRITTY-based terminal emulators."
  :group 'term-keys)

(define-widget 'term-keys/alacritty-modifier 'lazy
  "Choice for Alacritty key modifier state flags."
  :type '(choice (const "Command")
		 (const "Control")
		 (const "Option")
		 (const "Super")
		 (const "Shift")
		 (const "Alt")
		 (const :tag "(none)" nil)))


(defcustom term-keys/alacritty-modifier-map ["Shift" "Control" "Alt" "Super" "Command" "Option"]
  "Map of Alacritty modifiers to Emacs modifiers.

This should be a vector of 6 elements, with each element being a
string indicating the name of the Alacritty modifier name
corresponding to the Emacs modifiers Shift, Control, Meta, Super,
Hyper and Alt respectively.  nil indicates that there is no
mapping for this modifier."
  :type '(vector
	  (term-keys/alacritty-modifier :tag "Shift")
	  (term-keys/alacritty-modifier :tag "Control")
	  (term-keys/alacritty-modifier :tag "Meta")
	  (term-keys/alacritty-modifier :tag "Super")
	  (term-keys/alacritty-modifier :tag "Hyper")
	  (term-keys/alacritty-modifier :tag "Alt"))
  :group 'term-keys/alacritty)


(defun term-keys/alacritty-mods-representable (mods)
  "Return non-nil if the given MODS vector is representable in ALACRITTY."
  (cl-reduce (lambda (x y) (and x y)) ; all
	     (mapcar (lambda (n)
		       (or (not (elt mods n)) ; inactive modifier
			   (elt term-keys/alacritty-modifier-map n))) ; mapped
		     (number-sequence 0 (1- (length mods)))))) ; 0..5

(defvar term-keys/alacritty-config-format 'toml
  "If not `toml', output YAML (alacritty versions before 13.0), otherwise
output TOML.")

(defun term-keys/alacritty-format-mods (mods)
  "Format MODS in Alacritty syntax."
  (if (cl-reduce (lambda (x y) (or x y)) mods)
      (concat
       (if (eql term-keys/alacritty-config-format 'toml)
	   ""				; format "mods =" in caller
	 ", mods: ")
       (mapconcat
	(lambda (n)
	  (elt term-keys/alacritty-modifier-map n))
	(cl-remove-if-not (lambda (n) (elt mods n))
			  (number-sequence 0 (1- (length mods))))
	"|"))
    ""))


;; event names removed in winit-29 in winit commit 918430979f8219
;; (https://github.com/rust-windowing/winit) on 03-28-2023. Alacritty
;; 0.13.1 prints [ERROR] [alacritty_config_derive] Config error:
;; unknown variant `Apostrophe`, expected one of `Alt`,[...].

(defvar $term-keys/alacritty-missing-names
  '("Apostrophe" "Apps" "B" "Back" "Backslash" "C" "Capital" "Comma" "D" "Down" "E" "Equals" "F" "G" "Grave" "H" "I" "J" "K" "Key0" "Key1" "Key2" "Key3" "Key4" "Key5" "Key6" "Key7" "Key8" "Key9" "L" "LAlt" "LBracket" "LControl" "LShift" "LWin" "Left" "M" "Minus" "N" "O" "P" "Period" "Q" "R" "RAlt" "RBracket" "RControl" "RShift" "RWin" "Return" "Right" "S" "Scroll" "Semicolon" "Slash" "Snapshot" "T" "U" "Up" "V" "W" "X" "Y" "Z")
  "These key names are gone in winit 29")

(defun term-keys/alacritty-format-key (_index keymap _mods)
  (let ((name (elt keymap 8)))
    (if (and (eql term-keys/alacritty-config-format 'toml) ; XXX assume new winit
	     (member name $term-keys/alacritty-missing-names))
	(cond ((equal name "Back") "Backspace")
	      ((equal name "Backslash") "\\\\")
	      ((equal name "Return") "Enter")
	      ((member name $term-keys/alacritty-missing-names)
	       (elt keymap 0)))
      name)))

(defun term-keys/alacritty-config ()
  "Construct Alacritty configuration (alacritty.yml fragment).

This function returns, as a string, an alacritty.yml fragment
necessary to configure Alacritty to encode term-keys key
sequences, according to the term-keys configuration."
  (apply #'concat
	 (if (not  (eql term-keys/alacritty-config-format 'toml))
	     "key_bindings:\n")
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (let ((out-key (term-keys/alacritty-format-key index keymap mods))
		  (out-mods (term-keys/alacritty-format-mods mods))
		  (out-chars
		   (mapconcat
		    (lambda (c)
		      (format (if (eql term-keys/alacritty-config-format 'toml)
				  "\\u%04X" "\\x%02x")
			      c))
		    (append
		     term-keys/prefix
		     (term-keys/encode-key index mods)
		     term-keys/suffix
		     nil)
		    "")))
	      (if (eql term-keys/alacritty-config-format 'toml)
		  (format "[[keyboard.bindings]]\nchars = \"%s\"\nkey = \"%s\"\nmods = \"%s\"\n\n"
			  out-chars out-key out-mods)
		(format "- { key: %s%s, chars: \"%s\" }\n"
			out-key out-mods out-chars)))))))


(provide 'term-keys-alacritty)
;;; term-keys-alacritty.el ends here
