<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: eshell-autojump.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=eshell-autojump.el" />
<link type="text/css" rel="stylesheet" href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.0/css/bootstrap-combined.min.css" />
<link type="text/css" rel="stylesheet" href="/css/bootstrap.css" />
<meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: eshell-autojump.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=eshell-autojump.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for eshell-autojump.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=eshell-autojump.el" /><meta name="viewport" content="width=device-width" />
<script type="text/javascript" src="/outliner.0.5.0.62-toc.js"></script>
<script type="text/javascript">

  function addOnloadEvent(fnc) {
    if ( typeof window.addEventListener != "undefined" )
      window.addEventListener( "load", fnc, false );
    else if ( typeof window.attachEvent != "undefined" ) {
      window.attachEvent( "onload", fnc );
    }
    else {
      if ( window.onload != null ) {
	var oldOnload = window.onload;
	window.onload = function ( e ) {
	  oldOnload( e );
	  window[fnc]();
	};
      }
      else
	window.onload = fnc;
    }
  }

  var initToc=function() {

    var outline = HTML5Outline(document.body);
    if (outline.sections.length == 1) {
      outline.sections = outline.sections[0].sections;
    }

    if (outline.sections.length > 1
	|| outline.sections.length == 1
           && outline.sections[0].sections.length > 0) {

      var toc = document.getElementById('toc');

      if (!toc) {
	var divs = document.getElementsByTagName('div');
	for (var i = 0; i < divs.length; i++) {
	  if (divs[i].getAttribute('class') == 'toc') {
	    toc = divs[i];
	    break;
	  }
	}
      }

      if (!toc) {
	var h2 = document.getElementsByTagName('h2')[0];
	if (h2) {
	  toc = document.createElement('div');
	  toc.setAttribute('class', 'toc');
	  h2.parentNode.insertBefore(toc, h2);
	}
      }

      if (toc) {
        var html = outline.asHTML(true);
        toc.innerHTML = html;
      }
    }
  }

  addOnloadEvent(initToc);
  </script>

<script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
<script src="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.0/js/bootstrap.min.js"></script>
<script src="http://emacswiki.org/emacs/emacs-bootstrap.js"></script>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body class="http://www.emacswiki.org/emacs"><div class="header"><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><br /><span class="specialdays">Tonga, Emancipation Day</span><h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.emacswiki.org/emacs?search=%22eshell-autojump%5c.el%22">eshell-autojump.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="http://www.emacswiki.org/emacs/download/eshell-autojump.el">Download</a></p><pre class="code"><span class="linecomment">;;; eshell-autojump.el -- autojump command for Eshell</span>
<span class="linecomment">;; Copyright 2013  Alex Schroeder</span>

<span class="linecomment">;; This program is free software: you can redistribute it and/or modify it</span>
<span class="linecomment">;; under the terms of the GNU General Public License as published by the Free</span>
<span class="linecomment">;; Software Foundation, either version 3 of the License, or (at your option)</span>
<span class="linecomment">;; any later version.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This program is distributed in the hope that it will be useful, but WITHOUT</span>
<span class="linecomment">;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or</span>
<span class="linecomment">;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for</span>
<span class="linecomment">;; more details.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; You should have received a copy of the GNU General Public License along</span>
<span class="linecomment">;; with this program. If not, see &lt;http://www.gnu.org/licenses/&gt;.</span>

<span class="linecomment">;;; Commentary:</span>

<span class="linecomment">;; Use the command j to list common directories and to jump to them.</span>

<span class="linecomment">;;; Code:</span>

(require 'eshell)

(defcustom eshell-autojump-file
  (expand-file-name "<span class="quote">autojump</span>" eshell-directory-name)
  "<span class="quote">The name of the file to read/write the directories for autojumping.</span>"
  :type 'file
  :group 'eshell-dirs)

(defvar eshell-autojump-map nil
  "<span class="quote">Hash map with directories and how often they were called.</span>")

(defun eshell-autojump-load ()
  "<span class="quote">Read the initial value of `eshell-autojump-map' from `eshell-autojump-file'.
The file format is a simple alist.
Ignore non-directories.</span>"
  (let ((map (make-hash-table :test 'equal)))
    (when (file-exists-p eshell-autojump-file)
      (dolist (element (with-temp-buffer
			 (insert-file eshell-autojump-file)
			 (goto-char (point-min))
			 (read (current-buffer))))
	(when (file-directory-p (car element))
	  (puthash (car element) (cdr element) map))))
    (setq eshell-autojump-map map)))

(add-hook 'kill-emacs-hook 'eshell-autojump-save)

(defun eshell-autojump-save ()
  "<span class="quote">Save the value of `eshell-autojump-map' to `eshell-autojump-file'.
The file format is a simple alist.
Reduce values by 1% such that eventually unused items fall off the list
after not being used in a hundred sessions.</span>"
  (when (and eshell-autojump-file eshell-autojump-map)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
	(insert "<span class="quote">(</span>")
	(maphash (lambda (key value)
		   (when (&gt; value 0)
		     (insert "<span class="quote">(</span>")
		     (prin1 key)
		     (insert "<span class="quote"> . </span>")
		     (prin1 (- value 0.01))
		     (insert "<span class="quote">)\n</span>")))
		 eshell-autojump-map)
	  (delete-char -1)<span class="linecomment">; eat newline</span>
	  (insert "<span class="quote">)</span>"))
      (write-file eshell-autojump-file))))

(add-hook 'eshell-directory-change-hook
	  'eshell-autojump-record)

(defun eshell-autojump-record ()
  "<span class="quote">Record the current directory.
`curdir' is set by `eshell/cd'.</span>"
  (unless eshell-autojump-map
    (eshell-autojump-load))
  (if (gethash curdir eshell-autojump-map)
      (puthash curdir (1+ (gethash curdir eshell-autojump-map)) eshell-autojump-map)
    (puthash curdir 1 eshell-autojump-map)))

(defun eshell-autojump-candidates ()
  "<span class="quote">Return the most popular directories.
Return list of keys sorted by value, descending, from `eshell-autojump-map'.</span>"
  (unless eshell-autojump-map
    (eshell-autojump-load))
  (let (keys)
    (maphash (lambda (key value)
	       (setq keys (cons key keys)))
	     eshell-autojump-map)
    (sort keys (lambda (a b)
		 (&gt; (gethash a eshell-autojump-map)
		    (gethash b eshell-autojump-map))))))

(defun eshell/j (&rest args)           <span class="linecomment">; all but first ignored</span>
  "<span class="quote">Jump to a directory you often cd to.
This compares the argument with the list of directories you usually jump to.
Without an argument, list the ten most common directories.
With a positive integer argument, list the n most common directories.
Otherwise, call `eshell/cd' with the result.</span>"
  (setq args (eshell-flatten-list args))
  (let ((path (car args))
	(candidates (eshell-autojump-candidates))
	(case-fold-search (eshell-under-windows-p))
	result)
    (when (not path)
      (setq path 10))
    (if (and (integerp path) (&gt; path 0))
	(progn
	  (let ((n (nthcdr (1- path) candidates)))
	    (when n
	      (setcdr n nil)))
	  (eshell-lisp-command (mapconcat 'identity candidates "<span class="quote">\n</span>")))
      (while (and candidates (not result))
	(if (string-match path (car candidates))
	    (setq result (car candidates))
	  (setq candidates (cdr candidates))))
      (eshell/cd result))))

(provide 'eshell-autojump)
    
<span class="linecomment">;;; eshell-autojump.el ends here</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=eshell-autojump.el;missing=de_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="comment local" accesskey="c" href="http://www.emacswiki.org/emacs/Comments_on_eshell-autojump.el">Talk</a> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=eshell-autojump.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=eshell-autojump.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=eshell-autojump.el">Administration</a></span><span class="time"><br /> Last edited 2013-02-01 22:57 UTC by 178-83-213-96.dynamic.hispeed.ch <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=eshell-autojump.el">(diff)</a></span><form method="get" action="http://www.emacswiki.org/cgi-bin/emacs" enctype="multipart/form-data" accept-charset="utf-8" class="search">
<p><label for="search">Search:</label> <input type="text" name="search"  size="20" accesskey="f" id="search" /> <label for="searchlang">Language:</label> <input type="text" name="lang"  size="10" id="searchlang" /> <input type="submit" name="dosearch" value="Go!" /></p></form><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a class="licence" href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
