This packages provides `zotexo-minor-mode` and is primarly intended for latex users of emacs and [http://staff.science.uva.nl/~dominik/Tools/reftex/reftex-nutshell.html RefTeX]. It will synchronize on the fly BibTeX file and [http://www.zotero.org/ Zotero] collection associated with your file. 


== Instalation == 

Note: Zotexo doesn't seem to work properly on Windows, see this [http://code.google.com/p/zotexo/issues/detail?id=3 issue].

Download [https://zotexo.googlecode.com/svn/trunk/zotexo.el zotexo.el] and put it into your emacs path. Install [https://addons.mozilla.org/en-US/firefox/addon/mozrepl/ MozRepl] extension for Firefox and start it (you can also set it to auto start).

You will also need [http://www.gnu.org/software/auctex/reftex.html RefTex], which comes with [http://www.gnu.org/s/auctex/ AucTeX]. 

Activate `zotexo-minor-mode` in `latex-mode`:

{{{
(require 'zotexo)
(add-hook 'LaTeX-mode-hook 'zotexo-minor-mode)
}}}


== Usage ==

  _*Key-map*_
   {{{
C-c z c         zotexo-set-collection (also C-c z s)
C-c z u         zotexo-update-database
C-c z r         zotexo-reset

   }}}
If you have a BibTeX bibliography declaration somewhere in your file:
{{{
\bibliography{file_name}
}}}

`zotexo` will use it to export the Zotero collection as a `file_name.bib` file, otherwise it will export to `"[current-file-name]_zotexo_.bib"`.

To associate a zotero collection with the current buffer type `\C-c z c` or `\C-c z s` (
`zotexo-set-collection`. You can choose `*ALL*` to export
the whole Zotero library (but beware, if your library is big it might take some time). 

Zotexo uses [http://www.emacswiki.org/emacs/InteractivelyDoThings IDO] interface for the collection selection:

<img src="https://zotexo.googlecode.com/svn/trunk/set_collection.png" width=600>

<img src="https://zotexo.googlecode.com/svn/trunk/zotero_collection.png" width=600>

Now you can use  `'reftex-citation` command ("C-c ["`) to insert the citations into your file.

 
You can manually update the bibtex file with `C-c z u`
(`zotexo-update-database`) or mark the buffer for automatic update with `C-c z m` (zotexo-mark-for-auto-update). Due to zotero limitations not all changes to the collection are detected. 

Zotexo auto-updates bibtex files only if `zotexo--auto-update-is-on` is `t` (default is `nil`). You can always toggle it with `C-c z t`. The minor-mode indicator monitors this variable and is set to *"zx"* if `nil` or *"ZX"* if `t`. 

== Troubleshooting ==

If you get errors or spurious message switch to `*moz-command-output*` buffer and see what it the message. Normally you should see something like 

{{{
....> ":MozOK:"
repl> 
}}}

Also see the buffer `*ZotexoMozRepl*`, this is a primary buffer where mozrepl outputs it's messages. Normally it should be clean, showing only the startup message. 

To further investigate your problem. Toggle `M-x zotexo-verbose RET` and try the problematic `C-c z u`. If case of an error, go to *messages* buffer. You should see womething like this:

{{{

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
}}}

You should be able to execute this javascript with [https://github.com/bard/mozrepl/wiki/Emacs-integration moz-repl.el] Zotexo doesn't require moz-repl, but the idea is the same -- you should be able to send commands to moz-repl from emacs whatever way.


