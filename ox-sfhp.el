;;; ox-sfhp.el - export from org-mode to a single file HTML presentation

;; Author: DoMiNeLa10 (https://github.com/DoMiNeLa10)

;;; license: GPLv3 or newer

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Dependencies:
(require 'ox)
;; this exporter can use web-mode if it's installed to indent documents

;;; Variables and constants:

;;; Constants with code
(defconst org-sfhp-style-common
  "<style type=\"text/css\">
 /* common style, no colors should go in here */
 body {
   margin: 0;
   font-family: sans, arial, helvetica;
 }
 #slides {
   width: 100%;
   height: 100%;
   top: 0;
 }
 #slides div {
   margin-top: 1em;
   margin-bottom: 5em;
   width: 100%;
   text-align: center;
   position: relative;
 }
 h1 {
   text-align: center;
   margin-top: 0;
 }
 dl, p, blockquote {
   text-align: left;
   text-indent: 1em;
   margin: 0.5em 2em;
 }
 blockquote:before {
   content: \"“\";
 }
 blockquote:after {
   content: \"”\";
 }
 pre, .monospace {
   padding: 0.4em;
   border-radius: 0.2em; /* CSS 3 */
   text-align: left;
 }
 .monospace {
   white-space: pre-line;
   font-family: monospace;
   line-height: 2;
 }
 pre {
   margin: 0.5em 2em;
   word-wrap: break-word; /* CSS 3 and IE 5.5+ */
   white-space: pre-wrap;
 }
 dt {
   font-weight: bold;
 }
 dt:after { /* CSS 2.1 syntax should be more compatible */
   content: \":\";
 }
 table {
   margin: 1em auto;
 }
 td {
   padding: 0.3em 1em;
   border-width: 1px;
   border-style: solid;
   text-align: center;
 }
 tr:first-of-type td { /* CSS 3 */
   border-bottom-width: 3px;
   border-style: double;
 }
 ol, ul {
   text-align: left;
   margin: 0.5em 2em;
   padding-left: 2.5em;
 }
 ol *, ul * {
   margin: 0;
 }
 ol ol, ol ul, ul ol, ul ul {
   padding-left: 1.5em;
 }
 #buttons {
   position: fixed;
   height: 4em;
   width: 100%;
   bottom: 0;
   opacity: 0.5; /* CSS 3 */
   filter: alpha(opacity=50); /* old versions of IE */
   text-align: center;
 }
 #counter {
   position: absolute;
   margin-top: 0.25em;
   width: 30%;
   left: 35%;
   font-size: x-large;
 }
 button {
   outline: none; /* don't show that buttons are focused */
   height: 100%;
   width: 3em;
   margin: 0;
   border: none;
   font-size: xx-large;
 }
 #left {
   float: left;
 }
 #right{
   float: right;
 }
 img {
   max-width: 90%;
   margin: 1em;
 }
</style>\n"
  "Common style for ox-sfhp")

(defvar org-sfhp-color-themes
  '(("dark" . "<style type=\"text/css\">
/* dark style */
body {
  background-color: #222;
  color: #AAA;
}
pre, .monospace {
  background-color: #333;
  color: #BBB;
}
hr {
  background-color: #AAA;
}
td {
  border-color: #AAA;
}
button {
  background-color: #444;
  color: #AAA;
}
.disabled {
  background-color: #111;
}
#buttons {
  background-color: #333;
}
</style>\n")
    ("light" . "<style type=\"text/css\">
/* light style */
body {
  background-color: #EEE;
  color: #111;
}
pre, .monospace {
  color: #000;
  background-color: #FFF;
}
hr {
  background-color: #111;
}
td {
  border-color: #000;
}
#counter {
  color: #000;
}
button {
  background-color: #AAA;
  color: #000;
}
.disabled {
  background-color: #333;
}
#buttons {
  background-color: #BBB;
}
</style>\n"))
  "Color themes for ox-sfhp.")

(defconst org-sfhp-style-hack-oldie
  "<!--[if lt IE 7]>
  <style type=\"text/css\">
  #buttons {
    position: absolute !important;
  }
  div {
    posititon: absolute;
  }
  </style>
<![endif]-->\n"
  "A hack that fixes this presentation in old versions of
  Internet Explorer that don't support position: fixed;")

(defconst org-sfhp-style-hack-polish-quotes
  "<style type=\"text/css\">
 /* polish quotes */
 blockquote:before {
   content: \"„\" !important;
 }
 blockquote:after {
   content: \"”\" !important;
 }
</style>\n"
  "A hack that overrides quotes with polish quotation marks")

(defconst org-sfhp-script
  "<script type=\"text/javascript\">
 /* @licstart The following is the entire license notice for the JavaScript
    code in this page.

    Copyright (C) 2016

    The JavaScript code in this page is free software: you can redistribute
    it and/or modify it under the terms of the GNU General Public License
    (GNU GPL) as published by the Free Software Foundation, either version 3
    of the License, or (at your option) any later version. The code is
    distributed WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU GPL for
    more details.

    As additional permission under GNU GPL version 3 section 7, you may
    distribute non-source (e.g., minimized or compacted) forms of that code
    without the copy of the GNU GPL normally required by section 4, provided
    you include this license notice and a URL through which recipients can
    access the Corresponding Source.

    @licend The above is the entire license notice for the JavaScript code
    in this page. */
 var currentSlide = 1, totalSlides, left, right, counter;
 /* change value of currentSlide to open up the presentation on that slide */
 /* revert to 1 later */
 function init () { /* init function to be run onload */
   totalSlides = document.getElementById(\"slides\").children.length;
   for (i = 1; i <= totalSlides; i++) /* hide all slides */
     hideSlide(i);
   showSlide(currentSlide);
   /* add buttons (they won't work without JavaScript anyway) */
   document.body.innerHTML += '<div id=\"buttons\"><div id=\"counter\"></div><button id=\"left\" title=\"previous slide\" onclick=\"previousSlide();\">&lt;</button><button id=\"right\" title=\"next slide\" autofocus=\"autofocus\" onclick=\"nextSlide();\">&gt;</button></div>';
   left = document.getElementById(\"left\");
   right = document.getElementById(\"right\");
   counter = document.getElementById(\"counter\");
   update();
 }
 function getSlide (n) {
   return document.getElementById(\"slide\" + n);
 }
 function showSlide (n) {
   getSlide(n).style.display = \"block\";
 }
 function hideSlide (n) {
   getSlide(n).style.display = \"none\";
 }
 function previousSlide () {
   if (currentSlide > 1) {
     hideSlide(currentSlide);
     showSlide(--currentSlide);
     update();
   }
 }
 function nextSlide () {
   if (currentSlide < totalSlides) {
     hideSlide(currentSlide);
     showSlide(++currentSlide);
     update();
   }
 }
 function update () { /* update the counter and \"disable\" buttons */
   if (currentSlide <= 1)
     left.className = \"disabled\"; /* classList doesn't work in IE 5 */
   else
     left.className = \"\";
   if (currentSlide >= totalSlides)
     right.className = \"disabled\";
   else
     right.className = \"\";
   counter.innerHTML = currentSlide + \"/\" + totalSlides;
 }
 function getKey(event) { /* bind keys to changing slides */
   switch(event.which){
     case 80: /* p */
     case 66: /* b */
     case 72: /* h */
     case 75: /* k */
     case 37: /* left arrow */
       previousSlide();
       break;
     case 70: /* f */
     case 78: /* n */
     case 74: /* j */
     case 76: /* l */
     case 39: /* right arrow */
       nextSlide();
       break;
   }
 }
 document.onkeydown = getKey;
</script>\n"
  "JavaScript code for one at a time display in ox-sfhp output")

(defconst org-sfhp-meta
  "<meta charset=\"utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
<meta name=\"generator\" content=\"Org-mode + ox-sfhp.el\" />\n"
  "Meta tags for ox-sfhp.")

;;; Other constants
(defconst org-sfhp-extension
  "-sfhp.html"
  "Extension for files from ox-sfhp.")

(defconst org-sfhp-protected-characters
  '(("&" . "&amp;")                  ;order here is important, & should be first
    ("<" . "&lt;")
    (">" . "&gt;"))
  "List of protected HTML characters and how they should be escaped.")

(defconst org-sfhp-list-types
  '((ordered . "ol")
    (unordered . "ul")
    (descriptive . "dl"))
  "List of types of lists and tags that contain them. Used by ox-sfhp.")

;;; Variables
(defvar org-sfhp-color-theme "dark"     ;change this value later or something
  "Color theme for ox-sfhp export. Can be light, dark or CSS code
  with a custom color theme.")

(defvar org-sfhp-indent-output (fboundp 'web-mode)
  "When non-nil, ox-sfhp's output is indented.")

(defvar org-sfhp-include-oldie-hacks t
  "When non-nil, inlude a CSS hack for old versions of Internet
Explorer in ox-sfhp output.")

;; backend
(org-export-define-backend 'sfhp
  '((bold . org-sfhp-bold)
    (center-block . org-sfhp-paragraph)
    ;; (clock . org-sfhp-ignore)
    (code . org-sfhp-monospace)
    ;; (drawer . org-sfhp-ignore)
    ;; (dynamic-block . org-sfhp-ignore)
    ;; (entity . org-sfhp-ignore)
    (example-block . org-sfhp-monospace-block)
    ;; (export-block . org-sfhp-ignore) ; maybe implement later
    ;; (export-snippet . org-sfhp-ignore) ; maybe implement later
    ;; (fixed-width . org-sfhp-ignore)
    (footnote-definition . org-sfhp-paragraph)
    (footnote-reference . org-sfhp-paragraph)
    (headline . org-sfhp-headline)
    (horizontal-rule . org-sfhp-horizontal-rule)
    (inline-src-block . org-sfhp-monospace)
    ;; (inlinetask . org-sfhp-ignore)
    ;; (inner-template . org-sfhp-ignore)
    (italic . org-sfhp-italic)
    (item . org-sfhp-item)
    ;; (keyword . org-sfhp-ignore)
    ;; (latex-environment . org-sfhp-ignore)
    (latex-fragment . org-sfhp-monospace-block)
    (line-break . org-sfhp-line-break)
    ;; (link . org-sfhp-ignore) ;add this, but only for pictures
    (paragraph . org-sfhp-paragraph)
    (plain-list . org-sfhp-plain-list)
    (plain-text . org-sfhp-plain-text)
    ;; (planning . org-sfhp-ignore)
    ;; (property-drawer . org-sfhp-ignore)
    (quote-block . org-sfhp-quote-block)
    (quote-section . org-sfhp-paragraph)
    ;; (radio-target . org-sfhp-ignore)
    (section . org-sfhp-section) ;
    (special-block . org-sfhp-paragraph)
    (src-block . org-sfhp-monospace-block) ; maybe change later
    ;; (statistics-cookie . org-sfhp-ignore)
    (strike-through . org-sfhp-strike-through)
    (subscript . org-sfhp-subscript)
    (superscript . org-sfhp-superscript)
    (table . org-sfhp-table)
    (table-cell . org-sfhp-table-cell)
    (table-row . org-sfhp-table-row)
    ;; (target . org-sfhp-ignore)
    (template . org-sfhp-template)
    ;; (timestamp . org-sfhp-ignore)
    (underline . org-sfhp-underline)
    (verbatim . org-sfhp-monospace)
    (verse-block . org-sfhp-quote-block)) ; maybe treat it like a block quote
  :export-block "SFHP"
  :filters-alist '(;; (:filter-options . org-html-infojs-install-script)
        	   (:filter-final-output . org-sfhp-final-filter))
  :menu-entry
  '(?p "Export to a single file HTML presentation"
       ((?b "As a buffer" org-sfhp-export-to-buffer)
        (?f "As a file" org-sfhp-export-to-file)
        ;; (?o "As a file and open"        ; will this even work?
        ;;     (lambda (a s v b)
        ;;       (if a (org-html-export-to-html t s v b)
        ;; 	(org-open-file (org-html-export-to-html nil s v b)))))

        ;; browse-url might be a bettter choice
        ))

  ;; change this later
  :options-alist
  '(
    ;; options will go here
    ))


;;; wrapping functions (or whatever)
;; standard emphasis
(defun org-sfhp-bold (type contents info)
  "Return content as bold text in HTML format."
  (format "<b>%s</b>" contents))

(defun org-sfhp-strike-through (type contents info)
  "Return content as strike-through text in HTML format."
  (format "<s>%s</s>" contents))

(defun org-sfhp-italic (type contents info)
  "Return content as italic text in HTML format."
  (format "<i>%s</i>" contents))

(defun org-sfhp-monospace (type contents info)
  "Return content as monospace text. Used for verbatim and code markup in org-mode."
  (format "<span class=\"monospace\">%s</span>"
          (org-sfhp-plain-text (org-element-property :value type) info)))

(defun org-sfhp-underline (type contents info)
  "Return content as underline text in HTML format."
  (format "<u>%s</u>" contents))

(defun org-sfhp-subscript (type contents info)
  "Return contents as subscript HTML."
  (format "<sub>%s</sub>" contents))

(defun org-sfhp-superscript (type contents info)
  "Return contents as subscript HTML."
  (format "<sup>%s</sup>" contents))

;; handlers for unsupported features
(defun org-sfhp-insert-raw (type contents info)
  "Return content from org-mode that isn't supported in a fancy way."
  contents)

(defun org-sfhp-ignore (&rest ARGS)
  ;; (type contents info)
  "Ommit content that makes no sense in a presentation."
  "")

;; horizontal rule and line break
(defun org-sfhp-horizontal-rule (type contents info)
  "Return a horizontal rule."
  "<hr/>\n")

(defun org-sfhp-line-break (type contents info)
  "Return a HTML line break."
  "<br/>\n")

;; paragraph
(defun org-sfhp-paragraph (type contents info)
  "Return a HTML paragraph."
  (cond ((eq (org-element-type (org-export-get-parent type)) 'item) contents)
        (t (format "<p>\n%s</p>" contents))))

;; section
(defun org-sfhp-section (type contents info)
  "Return a org-mode seciton."
  contents)

;; maybe htmlize contents if it's a code block (better do it in a separate function)
(defun org-sfhp-monospace-block (type contents info)
  "Return a <pre> block with contents."
  (format "<pre>%s</pre>" contents))

(defun org-sfhp-quote-block (type contents info)
  "Return a block quote in HTML."
  (format "<blockquote>%s</blockquote>" contents))

;; lists
(defun org-sfhp-plain-list (type contents info)
  (let ((tag (or
              (cdr (assoc (org-element-property :type type)
                          org-sfhp-list-types))
              "ul")))
    (format "<%s>\n%s</%s>" tag contents tag)))

(defun org-sfhp-item (type contents info)
  (if (eq
       (org-element-property :type
                             (org-export-get-parent type))
       'descriptive)                    ;when a list is descriptive
      (format "<dt>%s</dt>\n<dd>\n%s</dd>"
              (car (org-element-property :tag type)) ;returns a list, so I car it
              contents)
    (format "<li>\n%s</li>" contents)))

;;; table stuff
;; table
(defun org-sfhp-table (type contents info)
  "Return contents as a HTML table."
  (format "<table>\n%s</table>" contents))

;; table row
(defun org-sfhp-table-row (type contents info)
  "Return a HTML table row."
  (when (eq (org-element-property :type type) 'standard) ;ignore separators
    (format "<tr>%s\n</tr>" contents)))

;; table cell
(defun org-sfhp-table-cell (type contents info)
  "Return a HTML table cell."
  (format "\n<td>%s</td>" contents)) ;newlines at the end get removed for some reason

;; headline
(defun org-sfhp-headline (type contents info) ;might not work well
  "Return a headline."
  (format "<div id=\"slide%d\">\n<h1>%s</h1>\n%s</div>"
          (car (org-export-get-headline-number type info))
          (car (org-element-property :title type)) ; org-element-property returns a list
          contents))

;; template

(defun org-sfhp-template (contents info)
  "Returns the outer template of the HTML document."
  (let ((language (plist-get info :language)))
    (concat "<!DOCTYPE html>\n"
            (format "<html%s>\n" (if language
                                     (concat " lang=\"" language "\"")
                                   ""))
            "<head>\n"
            (format "<title>%s</title>\n"
                    (let ((title (org-export-data (plist-get info :title) info)))
                      (if (eq title "") ; title of a HTML document shouldn't be empty
                          "Untitled presentation"
                        title)))
            org-sfhp-meta
            org-sfhp-script
            org-sfhp-style-common
            ;; color theme
            (or (cdr (assoc org-sfhp-color-theme org-sfhp-color-themes))
                (concat "<style type=\"text/css\">\n"
                        org-sfhp-color-theme
                        "\n</style>"))   ;include the custom color theme

            ;;; hacks
            ;; CSS hack for old versions of Internet Explorer
            (if org-sfhp-include-oldie-hacks
                org-sfhp-style-hack-oldie
              "")
            ;; polish quotes
            (if (string-equal language "pl")
                org-sfhp-style-hack-polish-quotes
              "")
            "</head>\n"
            "<body onload=\"init();\">\n"
            "<div id=\"slides\">\n"
            contents
            "</div>\n</body>\n</html>")))

;; plain text
;; this function is pretty much like org-html-encode-plain-text in ox-html
(defun org-sfhp-plain-text (content info)
  "Escapes characters used by HTML. Used by ox-sfhp."
  (mapc (lambda (pair)
          (setq content (replace-regexp-in-string (car pair) (cdr pair) content t t)))
        org-sfhp-protected-characters)
  content)

;;; export functions

(defun org-sfhp-export-to-buffer
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a single file HTML presentation buffer."
  (interactive)
  (org-export-to-buffer 'sfhp "*Org SFHP Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (if (fboundp 'web-mode)
                   (web-mode)
                 (set-auto-mode t)))))

(defun org-sfhp-export-to-file
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a single file HTML presentation file."
  (interactive)
  (org-export-to-file 'sfhp (org-export-output-file-name org-sfhp-extension)
    async subtreep visible-only body-only ext-plist))

;;; filters

(defun org-sfhp-final-filter (contents backend info)
  "A final filter for ox-sfhp."
  (if org-sfhp-indent-output
      (org-sfhp-indent-filter contents backend info)
    contents))

(defun org-sfhp-indent-filter (contents backend info)
  "Intent filter for ox-sfhp."
  (with-temp-buffer
    (insert contents)
    (if (fboundp 'web-mode) ;web-mode is better at indenting multi-language HTML files
        (web-mode)
      (set-auto-mode t))
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))
