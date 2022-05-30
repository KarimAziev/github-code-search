;;; github-code-search.el --- Configure code search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ohai-emacs
;; Keywords: hypermedia
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with code search

;; Commands

;; M-x `github-code-search'
;;      Search code on github for a given language and filename.

;; Customization

;; `github-code-search-word-at-point-major-modes-chars'
;;      Alist of major modes and chars to get word at point.
;;
;;      Chars is like the inside of a [...] in a regular expression
;;      except that ] is never special and quotes ^, - or (but
;;       not at the end of a range; quoting is never needed there).

;; `github-code-search-word-at-point-default-chars'
;;      Default value of chars to get word at point.
;;
;;      Chars is like the inside of a [...] in a regular expression
;;      except that ] is never special and quotes ^, - or (but
;;       not at the end of a range; quoting is never needed there).

;; `github-code-search-word-browse-fn'
;;      Function to browse results of code search.

;;; Code:

(defvar github-code-search-langs
  '("Emacs Lisp" "JavaScript" "TypeScript" "Org" "JSON" "NPM Config" "Shell"
    "ShellSession" "Git Attributes" "Git Config" "C#" "C++" "CoffeeScript" "CSS"
    "Dart" "DM" "Elixir" "Go" "Groovy" "HTML" "Java" "Kotlin" "Objective-C" "Perl"
    "PHP" "PowerShell" "Python" "Ruby" "Rust" "Scala" "Swift" "1C Enterprise" "4D"
    "ABAP" "ABAP CDS" "ABNF" "ActionScript" "Ada" "Adobe Font Metrics" "Agda" "AGS
  Script" "AIDL" "AL" "Alloy" "Alpine Abuild" "Altium Designer" "AMPL"
    "AngelScript" "Ant Build System" "ANTLR" "ApacheConf" "Apex" "API Blueprint"
    "APL" "Apollo Guidance Computer" "AppleScript" "Arc" "AsciiDoc" "ASL" "ASN.1"
    "ASP.NET" "AspectJ" "Assembly" "Astro" "Asymptote" "ATS" "Augeas" "AutoHotkey"
    "AutoIt" "Avro IDL" "Awk" "Ballerina" "BASIC" "Batchfile" "Beef" "Befunge"
    "BibTeX" "Bicep" "Bison" "BitBake" "Blade" "BlitzBasic" "BlitzMax" "Bluespec"
    "Boo" "Boogie" "Brainfuck" "Brightscript" "Browserslist" "C-ObjDump" "C2hs
  Haskell" "Cabal Config" "Cap'n Proto" "CartoCSS" "Ceylon" "Chapel" "Charity"
    "ChucK" "CIL" "Cirru" "Clarion" "Classic ASP" "Clean" "Click" "CLIPS"
    "Clojure" "Closure Templates" "Cloud Firestore Security Rules" "CMake" "COBOL"
    "CODEOWNERS" "CodeQL" "ColdFusion" "ColdFusion CFC" "COLLADA" "Common Lisp"
    "Common Workflow Language" "Component Pascal" "CoNLL-U" "Cool" "Coq"
    "Cpp-ObjDump" "Creole" "Crystal" "CSON" "Csound" "Csound Document" "Csound
  Score" "CSV" "Cuda" "CUE" "Cue Sheet" "cURL Config" "CWeb" "Cycript" "Cython"
    "D" "D-ObjDump" "Dafny" "Darcs Patch" "DataWeave" "desktop" "Dhall" "Diff"
    "DIGITAL Command Language" "dircolors" "DirectX 3D File" "DNS Zone"
    "Dockerfile" "Dogescript" "DTrace" "Dylan" "E" "E-mail" "Eagle" "Easybuild"
    "EBNF" "eC" "Ecere Projects" "ECL" "ECLiPSe" "EditorConfig" "Edje Data
  Collection" "edn" "Eiffel" "EJS" "Elm" "EmberScript" "EQ" "Erlang" "F#" "F*"
    "Factor" "Fancy" "Fantom" "Faust" "Fennel" "FIGlet Font" "Filebench WML"
    "Filterscript" "fish" "Fluent" "FLUX" "Formatted" "Forth" "Fortran" "Fortran
  Free Form" "FreeBasic" "FreeMarker" "Frege" "Futhark" "G-code" "Game Maker
  Language" "GAML" "GAMS" "GAP" "GCC Machine Description" "GDB" "GDScript"
    "GEDCOM" "Gemfile.lock" "Genie" "Genshi" "Gentoo Ebuild" "Gentoo Eclass"
    "Gerber Image" "Gettext Catalog" "Gherkin" "GLSL" "Glyph" "Glyph Bitmap
  Distribution Format" "GN" "Gnuplot" "Golo" "Gosu" "Grace" "Gradle"
    "Grammatical Framework" "Graph Modeling Language" "GraphQL" "Graphviz (DOT)"
    "Groovy Server Pages" "Hack" "Haml" "Handlebars" "HAProxy" "Harbour" "Haskell"
    "Haxe" "HCL" "HiveQL" "HLSL" "HolyC" "HTML+ECR" "HTML+EEX" "HTML+ERB"
    "HTML+PHP" "HTML+Razor" "HTTP" "HXML" "Hy" "HyPhy" "IDL" "Idris" "Ignore List"
    "IGOR Pro" "ImageJ Macro" "Inform 7" "INI" "Inno Setup" "Io" "Ioke" "IRC log"
    "Isabelle" "Isabelle ROOT" "J" "Jasmin" "Java Properties" "Java Server Pages"
    "JavaScript+ERB" "JFlex" "Jinja" "Jison" "Jison Lex" "Jolie" "jq" "JSON with
  Comments" "JSON5" "JSONiq" "JSONLD" "Jsonnet" "Julia" "Jupyter Notebook"
    "Kaitai Struct" "KakouneScript" "KiCad Layout" "KiCad Legacy Layout" "KiCad
  Schematic" "Kit" "KRL" "Kusto" "LabVIEW" "Lark" "Lasso" "Latte" "Lean" "Less"
    "Lex" "LFE" "LilyPond" "Limbo" "Linker Script" "Linux Kernel Module" "Liquid"
    "Literate Agda" "Literate CoffeeScript" "Literate Haskell" "LiveScript" "LLVM"
    "Logos" "Logtalk" "LOLCODE" "LookML" "LoomScript" "LSL" "LTspice Symbol" "Lua"
    "M" "M4" "M4Sugar" "Macaulay2" "Makefile" "Mako" "Markdown" "Marko" "Mask"
    "Mathematica" "MATLAB" "Maven POM" "Max" "MAXScript" "mcfunction" "Mercury"
    "Meson" "Metal" "Microsoft Developer Studio Project" "Microsoft Visual Studio
  Solution" "MiniD" "Mirah" "mIRC Script" "MLIR" "Modelica" "Modula-2"
    "Modula-3" "Module Management System" "Monkey" "Moocode" "MoonScript"
    "Motorola 68K Assembly" "MQL4" "MQL5" "MTML" "MUF" "mupad" "Muse" "Mustache"
    "Myghty" "nanorc" "NASL" "NCL" "Nearley" "Nemerle" "NEON" "nesC" "NetLinx"
    "NetLinx+ERB" "NetLogo" "NewLisp" "Nextflow" "Nginx" "Nim" "Ninja" "Nit" "Nix"
    "NL" "NSIS" "Nu" "NumPy" "Nunjucks" "NWScript" "ObjDump" "Object Data Instance
  Notation" "Objective-C++" "Objective-J" "ObjectScript" "OCaml" "Odin"
    "Omgrofl" "ooc" "Opa" "Opal" "Open Policy Agent" "OpenCL" "OpenEdge ABL"
    "OpenQASM" "OpenRC runscript" "OpenSCAD" "OpenStep Property List" "OpenType
  Feature File" "Ox" "Oxygene" "Oz" "P4" "Pan" "Papyrus" "Parrot" "Parrot
  Assembly" "Parrot Internal Representation" "Pascal" "Pawn" "PEG.js" "Pep8"
    "Pic" "Pickle" "PicoLisp" "PigLatin" "Pike" "PlantUML" "PLpgSQL" "PLSQL" "Pod"
    "Pod 6" "PogoScript" "Pony" "PostCSS" "PostScript" "POV-Ray SDL"
    "PowerBuilder" "Prisma" "Processing" "Proguard" "Prolog" "Propeller Spin"
    "Protocol Buffer" "Public Key" "Pug" "Puppet" "Pure Data" "PureBasic"
    "PureScript" "Python console" "Python traceback" "q" "Q#" "QMake" "QML" "Qt
  Script" "Quake" "R" "Racket" "Ragel" "Raku" "RAML" "Rascal" "Raw token data"
    "RDoc" "Readline Config" "REALbasic" "Reason" "Rebol" "Record Jar" "Red"
    "Redcode" "Redirect Rules" "Regular Expression" "Ren'Py" "RenderScript"
    "ReScript" "reStructuredText" "REXX" "Rich Text Format" "Ring" "Riot"
    "RMarkdown" "RobotFramework" "robots.txt" "Roff" "Roff Manpage" "Rouge" "RPC"
    "RPM Spec" "RUNOFF" "Sage" "SaltStack" "SAS" "Sass" "Scaml" "Scheme" "Scilab"
    "SCSS" "sed" "Self" "SELinux Policy" "ShaderLab" "Shen" "Sieve" "Singularity"
    "Slash" "Slice" "Slim" "Smali" "Smalltalk" "Smarty" "SmPL" "SMT" "Solidity"
    "Soong" "SourcePawn" "SPARQL" "Spline Font Database" "SQF" "SQL" "SQLPL"
    "Squirrel" "SRecode Template" "SSH Config" "Stan" "Standard ML" "Starlark"
    "Stata" "STON" "StringTemplate" "Stylus" "SubRip Text" "SugarSS"
    "SuperCollider" "Svelte" "SVG" "SWIG" "SystemVerilog" "Tcl" "Tcsh" "Tea"
    "Terra" "TeX" "Texinfo" "Text" "Textile" "TextMate Properties" "Thrift" "TI
  Program" "TLA" "TOML" "TSQL" "TSV" "TSX" "Turing" "Turtle" "Twig" "TXL" "Type
  Language" "Unified Parallel C" "Unity3D Asset" "Unix Assembly" "Uno"
    "UnrealScript" "UrWeb" "V" "Vala" "Valve Data Format" "VBA" "VBScript" "VCL"
    "Verilog" "VHDL" "Vim Help File" "Vim script" "Vim Snippet" "Visual Basic
  .NET" "Volt" "Vue" "Wavefront Material" "Wavefront Object" "wdl" "Web Ontology
  Language" "WebAssembly" "WebIDL" "WebVTT" "Wget Config" "Wikitext" "Windows
  Registry Entries" "wisp" "Wollok" "World of Warcraft Addon Data" "X BitMap" "X
  Font Directory Index" "X PixMap" "X10" "xBase" "XC" "XCompose" "XML" "XML
  Property List" "Xojo" "Xonsh" "XPages" "XProc" "XQuery" "XS" "XSLT" "Xtend"
    "Yacc" "YAML" "YANG" "YARA" "YASnippet" "ZAP" "Zeek" "ZenScript" "Zephir"
    "Zig" "ZIL" "Zimpl"))

(defcustom github-code-search-word-browse-fn 'browse-url
  "Function to browse results of code search."
  :type 'function
  :group 'github-code-search)

(defcustom github-code-search-word-at-point-default-chars "-'*\"_~$A-Za-z0-9:.#\\+"
  "Default value of chars to get word at point.

Chars is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  :type 'string
  :group 'github-code-search)

(defcustom github-code-search-word-at-point-major-modes-chars
  '((typescript-mode . "-*\"_~$A-Za-z0-9.#\\+")
    (js-mode . "-*\"_~$A-Za-z0-9.")
    (emacs-lisp-mode . "-*_~$A-Za-z0-9:.#\\+"))
  "Alist of major modes and chars to get word at point.

Chars is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (string :tag "Chars"))
  :group 'github-code-search)

(defun github-code-search-bounds-by-chars (chars)
  "Return bounds of thing at point if it is match CHARS.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun github-code-search-bounds-of-region-or-chars (chars)
  "Return bounds of active region or bounds of thing at point that match CHARS.

CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (github-code-search-bounds-by-chars chars)))

(defun github-code-search-get-bounds (&optional chars)
  "Return bounds of active region or bounds of thing at point that match CHARS.

CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (unless chars
    (setq chars (or chars
                    (cdr (assoc
                          major-mode
                          github-code-search-word-at-point-major-modes-chars))
                    github-code-search-word-at-point-default-chars)))
  (github-code-search-bounds-of-region-or-chars chars))

(defun github-code-search-region ()
  "Return current active region as string or nil."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defun github-code-search-word (&optional chars)
  "Get thing at point matching CHARS.
Optional argument CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)"
  (when-let ((bounds (github-code-search-get-bounds chars)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun github-code-search-word-or-region ()
  "Get current word or region."
  (or (github-code-search-region)
      (github-code-search-word)))

(defun github-code-search-query-from-alist (query-alist &optional keep-empty
                                                           semicolons)
  "Build a query-string from QUERY-ALIST.
If KEEP-EMPTY is non nil, include queries without values, else remove them.
When SEMICOLONS is given, the separator will be \";\"."
  (let ((filtered-list (delete
                        nil
                        (seq-filter
                         (lambda
                           (&rest args)
                           (funcall
                            (apply-partially '<= (if keep-empty 1 2))
                            (apply 'length args)))
                         (mapcar (apply-partially 'delete nil)
                                 query-alist)))))
    (url-build-query-string
     filtered-list semicolons keep-empty)))

(defun github-code-search-read-language ()
  "Read github language in minibuffer with completions."
  (let* ((modes-alist `((emacs-lisp-mode . "Emacs Lisp")
                        (typescript-mode . "Typescript")
                        (js-mode . "Javascript")
                        (org-mode . "Org")
                        (python-mode . "Python")
                        (clojurescript-mode . "Clojure")
                        (sh-mode . "Shell")
                        (json-mode . "JSON")))
         (lang (completing-read
                "Language: "
                (append (list "None") github-code-search-langs)
                nil t nil nil (cdr (assoc major-mode modes-alist)))))
    (when (and lang
               (not (string-empty-p lang))
               (not (string= lang "None")))
      lang)))

(defun github-code-search-read-filename ()
  "Read filename for github code search."
  (let ((filename (read-string
                   "Filename (empty if none): "
                   (when buffer-file-name
                     (file-name-nondirectory buffer-file-name)))))
    (when (and filename
               (not (string-empty-p filename)))
      filename)))

;;;###autoload
(defun github-code-search ()
  "Search code on github for a given language and filename."
  (interactive)
  (let ((filename (github-code-search-read-filename))
        (lang (github-code-search-read-language))
        (code (read-string "Code: " (github-code-search-word-or-region)))
        (query)
        (url))
    (setq query (github-code-search-query-from-alist
                 (cond ((and filename (string-empty-p code))
                        `(("type" "filename")
                          ("l" ,lang)
                          ("q" ,filename)))
                       (t `(("q" ,(if filename
                                      (concat "filename:" filename " " code)
                                    code))
                            ("l" ,lang)
                            ("type" "Code"))))))
    (setq url (concat "https://github.com/search?" query))
    (funcall github-code-search-word-browse-fn url)))

(provide 'github-code-search)
;;; github-code-search.el ends here