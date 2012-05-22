This packages provides `zotexo-minor-mode` and is primarily intended for latex users of emacs and [RefTeX](http://staff.science.uva.nl/~dominik/Tools/reftex/reftex-nutshell.html). It helps you efficiently synchronize BibTeX file and [Zotero](http://www.zotero.org) collection associated with your file. 


Installation
===========

Download [zotexo.el](https://raw.github.com/vitoshka/zotexo/master/zotexo.el) and put it into your emacs path. Install [MozRepl](https://addons.mozilla.org/en-US/firefox/addon/mozrepl/) extension for Firefox and start it (you can also configure it for  auto-start).

You will need [RefTex](http://www.gnu.org/software/auctex/reftex.html), which comes with [AucTeX](http://www.gnu.org/s/auctex/). 

Finally, activate `zotexo-minor-mode` in `latex-mode`:

```lisp
(require 'zotexo)
(add-hook 'TeX-mode-hook 'zotexo-minor-mode)
```

You might also want to activate `zotexo` in org mode if you use it to draft your
LaTeX papers.

Usage
=====

_*Key-map*_
```
C-c z c         zotexo-set-collection (also C-c z s)
C-c z u         zotexo-update-database
C-c z U         zotexo-update-database-secondary
C-c z r         zotexo-reset
```
If a file contains a `BibTeX' bibliography declaration:
```tex
\bibliography{file_name}
```
`zotexo` exports the associated Zotero collection as a `file_name.bib` file, otherwise it exports into `[current-file-name]_zotexo_.bib`.

To associate a zotero collection with the current buffer type `\C-c z c` (`zotexo-set-collection`). Select `*ALL*` to export
the whole Zotero library (but beware, if your library is big it might take awhile). Now you can use  `'reftex-citation` command (`C-c [`) to insert the citations into your file.

Zotexo uses [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings ) interface for the collection selection:

![set_collection](https://github.com/vitoshka/zotexo/raw/master/img/set_collection.png)

![zotero_collection](https://github.com/vitoshka/zotexo/raw/master/img/zotero_collection.png)

 
After modifying your zotero collection,  update the bibtex file with `C-c z u` (`zotexo-update-database`). This is a recommended way.  

Alternatively you can  mark the buffer for automatic update with `C-c z m` (zotexo-mark-for-auto-update). Due to zotero limitations not all changes to the collection are detected. This also doesn't seem to work on Windows (emacs hangs). Zotexo auto-updates bibtex files only if `zotexo--auto-update-is-on` is non-nil (default is `nil`). You can always toggle it with `C-c z t`. The minor-mode indicator is *"zx"* if this variable is `nil` and *"ZX"* otherwise.

Multiple Databases and Collections
----------------------------------

You can list several files in `\thebibliography{...}` list. The first file is the primary database which you set and update with `C-c z s` and `C-c z u` respectively. All others are secondary databases. 

Usually one database is enough, but for some projects you might want to use several zotero collections. Use `zotexo-export-secondary` (bound to `C-c z e`) to export any zotero collection into one of the secondary files.  You will be asked to select a file and a collection to export. This way you can have as many databases and zotero collections as you want. 

Troubleshooting
===============

If you get errors or spurious message switch to `*moz-command-output*` buffer and see what it the message. Normally you should see something like 

```
....> ":MozOK:"
repl> 
```

Also see the buffer `*ZotexoMozRepl*`, this is a primary buffer where mozrepl outputs it's messages. Normally it should be clean, showing only the startup message. 

To further investigate your problem. Toggle `M-x zotexo-verbose RET` and try the problematic `C-c z u`. If case of an error, go to *messages* buffer. You should see womething like this:

```javascript

 zotexo message on [Mon Apr  9 18:44:53 2012]
 Executing command: 

 (moz-command (format zotexo--export-collection-js '/home/vitoshka/works/disposition_effect/disposition.bib' 119))

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

You should be able to execute this javascript with [moz-repl.el](https://github.com/bard/mozrepl/wiki/Emacs-integration). Zotexo doesn't require _moz-repl_, but the idea is the same -- you should be able to send commands to moz-repl from emacs whatever way.


