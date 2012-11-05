Zotelo helps you efficiently export and synchronize local databases (bib, rdf, html, json etc) and [Zotero](http://www.zotero.org) collections directly from emacs.

Zotelo can be used in conjunction with any emacs mode but is primarily intended for latex users of [RefTeX](http://staff.science.uva.nl/~dominik/Tools/reftex/reftex-nutshell.html).


Installation
===========

Download [zotelo.el](https://raw.github.com/vitoshka/zotelo/master/zotelo.el) and put it into your emacs path. You may want to use the [Melpa](http://melpa.milkbox.net/) package archive to install `zotelo` programmatically. `Zotelo` package is updated from the upstream branch daily at every commit. If you don't use `Melpa` and want to give it a try please consider reading the official [documentation](http://melpa.milkbox.net/#installing).

Install [MozRepl](https://addons.mozilla.org/en-US/firefox/addon/mozrepl/) extension for Firefox and start it (you can also configure it for auto-start).

Though `zotelo` doesn't depend on [RefTex](http://www.gnu.org/software/auctex/reftex.html), you would really want it when working with `BibTeX`. `RefTeX` comes with [AucTeX](http://www.gnu.org/s/auctex/).

Activate `zotelo-minor-mode` in `latex-mode`:

```lisp
(require 'zotelo)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
```

Similarly you can activate `zotelo` in org mode if you use it to draft your LaTeX papers.

Usage
=====

_*Key-map*_
```
C-c z c         zotelo-set-collection (also C-c z s)
C-c z u         zotelo-update-database
C-c z e         zotelo-export-secondary
C-c z r         zotelo-reset
C-c z t         zotelo-set-translator
```

If a file contains any of the following bibliography declarations:

```tex
\bibliography{file1, file2, ...}
\zotelo{file1, file2, ...}
\nobibliography{file1, file2, ...}
```
`zotelo` exports the associated Zotero collection as a `file1.XXX` file, otherwise it exports into `[current-file-name].XXX`. `XXX` extension depends on the current translator, which is set to `BibTeX` by default. Use `zotelo-set-translator` to choose the translator. To set the translator permanently customize `zotelo-default-translator` variable. 

To associate a zotero collection with the current buffer type `\C-c z c` (`zotelo-set-collection`). Select `*ALL*` to export
the whole Zotero library (but beware, if your library is big it might take awhile). Now you can use  `'reftex-citation` (`C-c [`) to insert citations into your file.

Zotelo uses [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings ) interface for the collection selection:

![set_collection](https://github.com/vitoshka/zotelo/raw/master/img/set_collection.png)

![zotero_collection](https://github.com/vitoshka/zotelo/raw/master/img/zotero_collection.png)


After modifying your zotero collection from the zotero interface, update the exported database with `C-c z u` (`zotelo-update-database`). This is the recommended way.  

Alternatively you can  mark the buffer for automatic update with `C-c z m` (zotelo-mark-for-auto-update). Due to zotero limitations not all changes to the collection are detected. This also doesn't seem to work on Windows (emacs hangs). zotelo auto-updates bibtex files only if `zotelo--auto-update-is-on` is non-nil (default is `nil`). You can always toggle it with `C-c z T`. The minor-mode indicator is *"zx"* if this variable is `nil` and *"ZX"* otherwise.

Multiple Databases and Collections
----------------------------------

You can list several files in `\thebibliography{...}` list. The first file is the primary database which you set with `C-c z s` and update with `C-c z u`. All others are secondary databases. 

Usually one database is enough, but for some projects you might want to use several zotero collections. Use `zotelo-export-secondary` (bound to `C-c z e`) to export any zotero collection into one of the secondary files.  You will be asked to select a file and a collection to export. This way you can have as many databases and zotero collections as you want. 

Troubleshooting
===============
If zotxo stoped working for some reason, try `C-c z r`. This will reset moz-repl connection. 

If you get errors or spurious message then switch to `*moz-command-output*` buffer to investigate. Normally you should see something like 

```
....> ":MozOK:"
repl> 
```

Also see the buffer `*zoteloMozRepl*`, this is a primary buffer where mozrepl outputs it's messages. Normally it should be clean and show only the startup message. 

To further investigate your problem. Toggle `M-x zotelo-verbose RET` and try the problematic `C-c z u`. If case of an error, go to *messages* buffer. You should see womething like this:

```javascript

zotelo message on [Mon Apr  9 18:44:53 2012]
Executing command: 

(moz-command (format zotelo--export-collection-js '/home/vitoshka/works/disposition_effect/disposition.bib' 119))

translated as:

var filename=('/home/vitoshka/works/disposition_effect/disposition.bib');
var id = 119;
var prefs = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefService).getBranch('extensions.zotero.');
var recColl = prefs.getBoolPref('recursiveCollections');
prefs.setBoolPref('recursiveCollections', true);
var file = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
file.initWithPath(filename);
var zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
var collection = true;
var translator = new zotero.Translate('export');
if (id != 0){ //not all collections
collection = zotero.Collections.get(id);
translator.setCollection(collection);
};
if(collection){
translator.setLocation(file);
translator.setTranslator('9cb70025-a888-4a29-a210-93ec52da40d4');
translator.translate();
out=':MozOK:';
}else{
out='Collection with the id ' + id + ' does not exist.';
};
prefs.setBoolPref('recursiveCollections', recColl);
out;
```

You can execute this javascript with [moz-repl.el](https://github.com/bard/mozrepl/wiki/Emacs-integration). Zotelo doesn't require _moz-repl_, but the idea is the same -- you must be able to send commands to moz-repl from emacs whatever way.


