;;; zotelo.el --- Manage Zotero collections from emacs
;;
;; Filename: zotelo.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2011-2012, Spinu Vitalie, all rights reserved.
;; Created: Oct 2 2011
;; Version: 1.2
;; URL: https://github.com/vitoshka/zotelo
;; Keywords: zotero, emacs, reftex, bibtex, MozRepl, bibliography manager
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Zotelo helps you efficiently export and synchronize local databases (bib,
;; rdf, html, json etc) and [Zotero](http://www.zotero.org) collections directly
;; from emacs.
;;
;; Zotelo can be used in conjunction with any emacs mode but is primarily
;; intended for bibtex and RefTeX users.
;;
;; zotelo-mode-map lives on  C-c z prefix.
;;
;;   *Instalation*
;;
;;   (add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;;
;;  See https://github.com/vitoshka/zotelo for more
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar zotelo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-czu" 'zotelo-update-database)
    (define-key map "\C-cze" 'zotelo-export-secondary)
    (define-key map "\C-czs" 'zotelo-set-collection)
    (define-key map "\C-czc" 'zotelo-set-collection)
    (define-key map "\C-czm" 'zotelo-mark-for-auto-update)
    (define-key map "\C-czr" 'zotelo-reset)
    (define-key map "\C-czt" 'zotelo-set-translator)
    (define-key map "\C-czT" 'zotelo-toggle-auto-update)
    map))

(defvar zotelo--check-timer nil
  "Global timer executed at `zotelo-check-interval' seconds. ")

(defvar zotelo-check-interval 5
  "Seconds between checks for zotero database changes.
Note that zotelo uses idle timer. Yeach time emacs is idle for
this number of seconds zotelo checks for an update.")

;; (defvar zotelo-use-ido t
;;   "If t will try to use ido interface")

(defvar zotelo-auto-update-all nil
  "If t zotelo checks for the change in zotero database
every `zotelo-check-interval' seconds and auto updates all
buffers with active `zotelo-minor-mode'.

If nil the only updated files are those with non-nil file local
variable `zotelo-auto-update'. See
`zotelo-mark-for-auto-update'. ")

(defgroup zotelo nil "Customization for zotelo"
  :group 'convenience)

;;;###autoload
(defcustom zotelo-default-translator 'BibTeX
  "The name of the default zotero-translator to use (a symbol).

Must correspond to one of the keys in `zotelo-translators' alist.

You can set this varialbe interactively with
`zotelo-set-translator'.
"
  :type 'symbol
  :group 'zotelo)

;;;###autoload
(defcustom zotelo-translators
  '((BibTeX "9cb70025-a888-4a29-a210-93ec52da40d4" "bib")
    (BibLaTeX "ba4cd274-f24e-42cf-8ff2-ccfc603aacf3" "bib")
    (BibLaTeX-cite "fe7a85a9-4cb5-4986-9cc3-e6b47d6660f7" "bib")
    (Zotero-RDF "14763d24-8ba0-45df-8f52-b8d1108e7ac9" "rdf")
    (Wikipedia "3f50aaac-7acc-4350-acd0-59cb77faf620" "txt")
    (CSL-JSON "bc03b4fe-436d-4a1f-ba59-de4d2d7a63f7" "json")
    (Bookmarks-HTML "4e7119e0-02be-4848-86ef-79a64185aad8" "html")
    (Refer/BibIX  "881f60f2-0802-411a-9228-ce5f47b64c7d" "txt")
    (RIS "32d59d2d-b65a-4da4-b0a3-bdd3cfb979e7" "ris")
    (MODS  "0e2235e7-babf-413c-9acf-f27cce5f059c" "xml")
    (Bibliontology-RDF "14763d25-8ba0-45df-8f52-b8d1108e7ac9" "rdf"))
  "An alist of zotero translators ids.
Each cell consists an user friendly key, and a value is a list of
an unique identifier used by zotero and an extension of the
target file.

Not all of the listed translatros are the default zotero
translators. You have to search and download them yourself.

Standard BibTeX (zotero): '9cb70025-a888-4a29-a210-93ec52da40d4'
BibLaTeX (downloaded from https://code.google.com/p/zotero-biblatex-export/): 'ba4cd274-f24e-42cf-8ff2-ccfc603aacf3'
"
  :group 'zotelo
  :type 'alist
  )

(defvar zotelo--get-zotero-database-js
  "var zotelo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
zotelo_zotero.getZoteroDatabase().path;")

(defvar zotelo--get-zotero-storage-js
  "var zotelo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
zotelo_zotero.getStorageDirectory().path;")

(defvar zotelo--auto-update-is-on nil
  "If t zotelo monitors changes in zotero database and reexports
  collections if needed.
  You can toggle it with  'C-c z T'
")

(defvar zotelo--ignore-files (list "_region_.tex"))


(defvar zotelo--verbose nil)
(defun zotelo-verbose ()
  "Toggle zotelo debug messages (all printed in *message* buffer)"
  (interactive)
  (message "zotelo verbose '%s'" (setq zotelo--verbose (not zotelo--verbose))))

(defun zotelo--message (str)
  (when zotelo--verbose
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (insert (format "\n zotelo message [%s]\n %s\n" (current-time-string) str)))))

(defvar zotelo--render-collection-js
  "var zotelo_render_collection = function() {
    var R=%s;
    var Zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
    var print_names = function(collections, prefix){
        for (c in collections) {
            var fullname = prefix + '/' + collections[c].name;
            R.print(collections[c].id + ' ' + fullname);
            if (collections[c].hasChildCollections) {
                var subcol = Zotero.getCollections(collections[c].id);
                print_names(subcol, fullname); 
            }}};
    print_names(Zotero.getCollections(), '');
    var groups = Zotero.Groups.getAll();        
    for (g in groups){
        print_names(groups[g].getCollections(), '/*groups*/'+groups[g].name);
    }};
"
  )


;;;; moz-repl splits long commands. Need to send it partially, but then errors
;;;; in first parts are not visible ... :(
;;;; todo: insert the check dirrectly in moz-command ??? 
(defvar zotelo--export-collection-js
  "
var zotelo_filename=('%s');
var zotelo_id = %s;
var zotelo_translator_id = '%s';
var zotelo_prefs = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefService).getBranch('extensions.zotero.');
var zotelo_file = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
var zotelo_recColl = zotelo_prefs.getBoolPref('recursiveCollections');
zotelo_file.initWithPath(zotelo_filename);
//split
var zotelo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
var zotelo_collection = true;
var zotelo_translator = new zotelo_zotero.Translate('export');
if (zotelo_id != 0){ //not all collections
    zotelo_collection = zotelo_zotero.Collections.get(zotelo_id);
    zotelo_translator.setCollection(zotelo_collection);
};
//split
if(zotelo_collection){
    zotelo_translator.setLocation(zotelo_file);
    zotelo_translator.setTranslator(zotelo_translator_id);
    zotelo_prefs.setBoolPref('recursiveCollections', true);
    zotelo_translator.translate();
    zotelo_prefs.setBoolPref('recursiveCollections', zotelo_recColl);
    zotelo_out=':MozOK:';
}else{
    zotelo_out='Collection with the id ' + zotelo_id + ' does not exist.';
};
//split
zotelo_out;
"
  "Command to be sent to zotero request export."
  )

(defvar zotelo--dateModified-js
  "var zotelo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
var zotelo_id = %s;
var zotelo_collection = zotelo_zotero.Collections.get(zotelo_id);
if(zotelo_collection){
   ':MozOK:' + zotelo_collection.dateModified;
}else{
   'Collection with the id ' + zotelo_id + ' does not exist.';
}"

  "Command to get last modification date of the collection.")

;;;###autoload
(define-minor-mode zotelo-minor-mode
  "zotelo minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, `zotelo-set-collection' prompts
for zotero collection and stores it as file local variable . To
manually update the BibTeX data base call
`zotelo-update-database'. The \"file_name.bib\" file will be
created with the exported zotero items. To specify the file_name
just insert insert \\bibliography{file_name} anywhere in the
buffer.

This mode is designed mainly for latex modes and works in
conjunction with RefTex, but it can be used in any other mode
such as org-mode.

The following keys are bound in this minor mode:

\\{zotelo-minor-mode-map}"
  nil
  (zotelo--auto-update-is-on " ZX" " zx")
  :keymap zotelo-minor-mode-map
  :group 'zotelo
  (if zotelo-minor-mode
      (progn
        (unless (timerp zotelo--check-timer)
          (setq zotelo--check-timer
                (run-with-idle-timer 5 zotelo-check-interval 'zotelo--check-and-update-all)))
        )
    (unless
        (loop for b in (buffer-list)
              for is-zotelo-mode = (buffer-local-value 'zotelo-minor-mode b)
              until is-zotelo-mode
              finally return is-zotelo-mode)
      ;; if no more active zotelo mode, cancel the timer and kill the process
      (when (timerp zotelo--check-timer)
        (cancel-timer zotelo--check-timer)
        (setq zotelo--check-timer nil)
        (delete-process (zotelo--moz-process))
        (kill-buffer zotelo--moz-buffer)
        )
      )
    )
  )


(defun zotelo--check-and-update-all ()
  "Function run with `zotelo--check-timer'."
  (when zotelo--auto-update-is-on
    (let ( out id any-z-buffer-p z-buffer-p)
      (zotelo--message  "zotelo checking for updates.")
      (dolist (b  (buffer-list)) ;iterate through zotelo buffers
        (setq z-buffer-p (buffer-local-value 'zotelo-minor-mode b))
        (when z-buffer-p
          (setq any-z-buffer-p t))
        (when (and
               ;; zotelo buffer?
               z-buffer-p
               ;; exclusion reg-exp  matched?,
               (not (delq nil (mapcar (lambda (reg)
                                        (string-match reg (buffer-name b)))
                                      zotelo--ignore-files)))
               ;; collection is set?,
               (assoc 'zotero-collection (buffer-local-value 'file-local-variables-alist b))
               ;; auto-update-all?, auto-update?
               (let ((auto-update
                      (assoc 'zotelo-auto-update (buffer-local-value 'file-local-variables-alist b))))
                 (if (and zotelo-auto-update-all (null auto-update))
                     (setq auto-update '(t . t)))
                 (cdr auto-update))
               )
          (with-current-buffer b
            (ignore-errors
              (setq id (zotelo-update-database t))))
          (when id
            (setq out
                  (append (list (buffer-name b)) out))
            )))
      (if (> (length out) 0)
          (message "Bibliography updated in %s buffers: %s." (length out) out))
      (when (and (not any-z-buffer-p)
                 (timerp zotelo--check-timer))
        ;; stop timer if no more zotelo buffers
        (cancel-timer zotelo--check-timer)
        (setq zotelo--check-timer nil)
        (delete-process (zotelo--moz-process))
        (kill-buffer zotelo--moz-buffer)
        )
      )))


;;;###autoload
(defun zotelo-export-secondary ()
  "Export zotero collection into  secondary BibTeX database.

Before export, ask for a secondary database and zotero collection
to be exported into the database. Secondary databases are those
in \\bibliography{file1, file2, ...}, except the first one.

Error ocures if there is only one (primary) file listed in 
\\bibliography{...}.

Error if zotero collection is not found by MozRepl"
  (interactive)
  (let* ((files (zotelo--locate-bibliography-files))
	 (bibfile (cond
		   ((< (length files) 2)
                    (error "No secondary databases (\\bibliography{...} lists contain less than 2 files)."))
		   ((= (length files) 2)
		    (cadr files))
		   (t (zotelo--read (cdr files) "File to update: "))))
	 (collection (zotelo-set-collection
		      (format "Export into '%s': " (file-name-nondirectory bibfile))
		      'no-update 'no-set)))
    (zotelo-update-database nil bibfile (get-text-property 0 'zotero-id collection))))


(defun zotelo-set-translator ()
  "Ask to choose from available translators and set `zotelo-default-translator'."
  (interactive)
  (let ((tnames (mapcar (lambda (el) (symbol-name (car el)))
                        zotelo-translators)))
    (setq zotelo-default-translator
          (intern (zotelo--read tnames "Choose translator: "
                                (symbol-name zotelo-default-translator))))
    (message "Translator set to %s" zotelo-default-translator)))


;;;###autoload
(defun zotelo-update-database (&optional check-zotero-change bibfile id)
  "Update the primary BibTeX database associated with the current buffer.

Primary database is the first file in \\bibliography{file1, file2,
...}, list. If you want to export into a different file use
`zotelo-update-database-secondary'.

If BIBFILE is supplied, don't infer from \\bibliography{...} statement.

If ID is supplied, don't infer collection id from file local variables.

Through an error if zotero collection has not been found by MozRepl"
  (interactive)
  (let ((bibfile (or bibfile
		     (car (zotelo--locate-bibliography-files))))
        (proc  (zotelo--moz-process))
        (id (or id
		(zotelo--get-local-collection-id)))
        (file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
        (translator (assoc zotelo-default-translator zotelo-translators))
        all-colls-p cstr bib-last-change zotero-last-change com com1)
    (unless translator
      (error "Cannot find translator %s in `zotelo-translators' alist" zotelo-default-translator))
    
    (unless bibfile
      ;; (setq file-name (concat file-name "."))
      (setq bibfile file-name)
      (message "Using '%s' filename for %s export." file-name zotelo-default-translator)
      )
    
    (setq bibfile (concat (expand-file-name bibfile) "." (nth 2 translator)))
    (setq bib-last-change (nth 5 (file-attributes bibfile))) ;; nil if bibfile does not exist
    (setq bibfile (replace-regexp-in-string "\\\\" "\\\\"
					    (convert-standard-filename bibfile) nil 'literal))
    (when (and (called-interactively-p 'any) (null id))
      (zotelo-set-collection "Zotero collection is not set. Choose one: " 'no-update)
      (setq id (zotelo--get-local-collection-id)))
    
    (unless (file-exists-p (file-name-directory bibfile))
      (error "Directory '%s' does not exist; create it first." (file-name-directory bibfile)))
    (when check-zotero-change
      (set-time-zone-rule t)
      (with-current-buffer (moz-command (format zotelo--dateModified-js id))
        (goto-char (point-min))
        (when (re-search-forward ":MozOK:" nil t) ;; ingore the error it is  cought latter
          (setq zotero-last-change (date-to-time
                                    (buffer-substring-no-properties (point) (point-max))))
          )))
    (when (and id
               (or (null check-zotero-change)
                   (null bib-last-change)
                   (time-less-p bib-last-change zotero-last-change)))
      (setq cstr (format zotelo--export-collection-js bibfile id (cadr translator)))
      (zotelo--message (format "Executing command: \n\n (moz-command (format zotelo--export-collection-js '%s' %s))\n\n translated as:\n %s\n"
			       bibfile id cstr))
      (message "Updating '%s' ..." (file-name-nondirectory bibfile))
      (setq com (split-string cstr "//split" t))
      (while (setq com1 (pop com))
	(when com ;; append to all except the last one
	  (setq com1 (concat com1 "\":MozOk:\"")))
	(with-current-buffer (moz-command com1)
	  (goto-char (point-min))
	  (unless (re-search-forward ":MozOK:" nil t)
	    (error "MozError: \n%s" (buffer-substring-no-properties (point) (point-max))))
          ))
      (let ((buf (get-file-buffer bibfile)))
        (when buf (with-current-buffer buf (revert-buffer 'no-auto 'no-conf))))
      (message "'%s' updated successfully (%s)" (file-name-nondirectory bibfile) zotelo-default-translator)
      id)
    )
  )

(defcustom zotelo-bibliography-commands '("bibliography" "nobibliography" "zotelo")
  "List of commands which specify databases to use.

For example \\bibliography{file1,file2} or \\zotelo{file1,file2}
both specify that file1 is a primary database and file2 is the
secondary one. 
"
  :group 'zotelo
  :type 'list)


(defun zotelo--locate-bibliography-files ()
  ;; Scan buffer for bibliography macro and return as a list.
  ;; Modeled after the corresponding reftex function

  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         (concat
                                        ;           "\\(\\`\\|[\n\r]\\)[^%]*\\\\\\("
          "\\(^\\)[^%\n\r]*\\\\\\("
          (mapconcat 'identity zotelo-bibliography-commands "\\|")
          "\\){[ \t]*\\([^}]+\\)") nil t)
        (split-string   (when (match-beginning 3)
                          (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
                        "[ \t\n\r]*,[ \t\n\r]*"))))

;;;###autoload
(defun zotelo-set-collection (&optional prompt no-update no-file-local)
  "Ask for a zotero collection.
Ido interface is used by default. If you don't like it set `zotelo-use-ido' to nil.


In `ido-mode' use \"C-s\" and \"C-r\" for navigation. See
ido-mode emacs wiki for many more details.

If no-update is t, don't update after setting the collecton.

If no-file-local is non-nill don't set file-local variable.

Return the properized collection name.
"
  (interactive)
  (let ((buf (get-buffer-create "*moz-command-output*"))
	colls name id)
    ;; set up the collection list
    (moz-command (format zotelo--render-collection-js
			 (process-get (zotelo--moz-process) 'moz-prompt)))
    (moz-command "zotelo_render_collection()" buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (zotelo--message (format "Collections:\n %s" 
			       (buffer-substring-no-properties (point-min) (min 500 (point-max)))))
      (while (re-search-forward "^\\([0-9]+\\) /\\(.*\\)$" nil t)
	(setq id (match-string-no-properties 1)
	      name (match-string-no-properties 2))
	(setq colls (cons
		     (propertize name 'zotero-id id)
		     colls))))

    (if (null colls)
	(error "No collections found or error occured see *moz-command-output* buffer for clues.")
      ;; (setq colls (mapcar 'remove-text-properties colls))
      (setq name (zotelo--read (cons (propertize "*ALL*" 'zotero-id "0") (nreverse colls))
			       prompt))
      (unless no-file-local
	(save-excursion
	  (add-file-local-variable 'zotero-collection
				   (propertize (get-text-property 0 'zotero-id name)
					       'name (substring-no-properties name)))
	  (hack-local-variables))
	(unless no-update
	  (zotelo-update-database)))
      name
      )))



(defun zotelo-mark-for-auto-update (&optional unmark)
  "Mark current file for auto-update.

If the file is marked for auto-update zotelo runs
`zotelo-update-database' on it whenever the zotero data-base is
updated.

File is marked by adding file local variable
'zotelo-auto-update'. To un-mark the file call this function with
an argument or just delete or set to nil the local variable at
the end of the file.
"
  (interactive "P")
  (save-excursion
    (if unmark
        (progn
          (delete-file-local-variable 'zotelo-auto-update)
          (setq file-local-variables-alist
                (assq-delete-all 'zotelo-auto-update file-local-variables-alist)))
      (add-file-local-variable 'zotelo-auto-update t)
      (hack-local-variables)
      )
    )
  )

;;;###autoload
(defun zotelo-reset ()
  "Reset zotelo."
  (interactive)
  (delete-process (zotelo--moz-process))
  (kill-buffer zotelo--moz-buffer)
  (message "Killed moz process")
  )


(defun zotelo-toggle-auto-update ()
  "Togles auto-updating in all buffers.
Note that once toggled in your firefox and MozRepl must be
started, otherwise you will start getting error screens. "
  (interactive)
  (setq zotelo--auto-update-is-on (not zotelo--auto-update-is-on))
  )


(defun zotelo--get-local-collection-id ()
  (cdr (assoc 'zotero-collection file-local-variables-alist)))

(defun zotelo--read (collections &optional prompt default)
  "Read a choice from zotero collections via Ido."
  (let (reset-ido)
    (when  (and (require 'ido)
		(not ido-mode))
      ;; ido initialization
      (setq reset-ido t)
      (ido-init-completion-maps)
      (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
      (add-hook 'choose-completion-string-functions 'ido-choose-completion-string)
      (add-hook 'kill-emacs-hook 'ido-kill-emacs-hook))
    (unwind-protect
	(ido-completing-read (or prompt "Collection: ") collections
			     nil t nil nil default)
      (when reset-ido
	(remove-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
	(remove-hook 'choose-completion-string-functions 'ido-choose-completion-string)
	(remove-hook 'kill-emacs-hook 'ido-kill-emacs-hook)
	))))




;;;; Moz utilities

(defvar zotelo--moz-host "localhost")
(defvar zotelo--moz-port 4242)
(defvar zotelo--moz-buffer nil)
(defvar zotelo--startup-error-count 0)
(defvar zotelo--max-errors 10)

(defun zotelo--moz-process ()
  "Return inferior MozRepl process.  Start it if necessary."
  (or (if (buffer-live-p zotelo--moz-buffer)
          (get-buffer-process zotelo--moz-buffer))
      (progn
        (zotelo--moz-start-process)
        (zotelo--moz-process))))

(defun zotelo--moz-start-process ()
  "Start mozrepl process and connect to Firefox.
Note that you have to start the MozRepl server from Firefox."
  (interactive)
  (setq zotelo--moz-buffer (get-buffer-create "*zoteloMozRepl*"))
  (condition-case err
      (let ((proc (make-network-process :name "zoteloMozRepl" :buffer zotelo--moz-buffer
					:host zotelo--moz-host :service zotelo--moz-port
					:filter 'moz-ordinary-insertion-filter)))
        (sleep-for 0 100)
	(set-process-query-on-exit-flag proc nil)
        (with-current-buffer zotelo--moz-buffer
          (set-marker (process-mark proc) (point-max)))
        (setq zotelo--startup-error-count 0))
    (file-error
     (let ((buf (get-buffer-create "*MozRepl Error*")))
       (setq zotelo--startup-error-count (1+ zotelo--startup-error-count))
       (with-current-buffer buf
         (erase-buffer)
         (insert "Can't start MozRepl, the error message was:\n\n     "
                 (error-message-string err)
                 "\n"
                 "\nA possible reason is that you have not installed"
                 "\nthe MozRepl add-on to Firefox or that you have not"
                 "\nstarted it.  You start it from the menus in Firefox:"
                 "\n\n     Tools / MozRepl / Start"
                 "\n"
                 "\nSee ")
         (insert-text-button
          "MozRepl home page"
          'action (lambda (button)
                    (browse-url
                     "http://hyperstruct.net/projects/mozrepl")
                    )
          'face 'button)
         (insert
          " for more information."
          "\n"
          "\nMozRepl is also available directly from Firefox add-on"
          "\npages, but is updated less frequently there.\n\n"
          (format "zotelo Error Count: %s\n\n%s"
                  zotelo--startup-error-count
                  (if (not (and (>= zotelo--startup-error-count 10)
                                zotelo--auto-update-is-on))
                      "If zotelo auto-update is on, press \"C-c z t\" to turn it off."
                    (setq zotelo--auto-update-is-on nil)
                    (setq zotelo--startup-error-count 0)
                    "Too many errors. zotelo auto-update was turned off!\nUse [C-c z t] to switch it on.")))
         )
       (kill-buffer "*zoteloMozRepl*")
       (display-buffer buf t)
       (error "zotelo cannot start MozRepl")
       ))
    ))

(defun moz-ordinary-insertion-filter (proc string)
  "simple filter for command execution"
  (with-current-buffer (process-buffer proc)
    (let ((ready (string-match "\\(\\w+\\)> \\'" string))
	  moving)
      (when ready
	(process-put proc 'moz-prompt (match-string-no-properties 1 string)))
      (process-put proc 'busy (not ready))
      (setq moving (= (point) (process-mark proc)))
      (save-excursion
        ;; Insert the text, moving the process-marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))
    ))

(defvar moz-verbose nil
  "If t print informative statements.")

;;;###autoload
(defun moz-command (com &optional buf)
  "Send the moz-repl  process command COM and delete the output
from the MozRepl process buffer.  If an optional second argument BUF
exists, it must be a string or an existing buffer object. The
output is inserted in that buffer. BUF is erased before use.

New line is automatically appended.
"
  (if buf
      (setq buf (get-buffer-create buf))
    (setq buf (get-buffer-create "*moz-command-output*")))
  (let ((proc (zotelo--moz-process))
        oldpb oldpf oldpm)
    (save-excursion
      ;; (set-buffer sbuffer)
      (when (process-get proc 'busy)
        (process-send-string proc ";\n") ;; clean up unfinished
        (sleep-for 0 100)
        (when (process-get proc 'busy)
          (error
           "MozRepl process is not ready. Try latter or reset.")))
      (setq oldpf (process-filter proc))
      (setq oldpb (process-buffer proc))
      (setq oldpm (marker-position (process-mark proc)))
      ;; need the buffer-local values in result buffer "buf":
      (unwind-protect
	  (progn
	    (set-process-buffer proc buf)
	    (set-process-filter proc 'moz-ordinary-insertion-filter)
	    ;; Output is now going to BUF:
            (with-current-buffer buf
	      (erase-buffer)
	      (set-marker (process-mark proc) (point-min))
	      (process-put proc 'busy t)
	      (process-send-string proc (concat com "\n"))
	      (moz-wait-for-process proc)
	      ;;(delete-region (point-at-bol) (point-max))
	      )
	    (zotelo--message "Moz-command finished"))
	;; Restore old values for process filter
	(set-process-buffer proc oldpb)
	(set-process-filter proc oldpf)
	(set-marker (process-mark proc) oldpm oldpb) ;; need oldpb here!!! otherwise it is not set for some reason
	)
      ))
  buf)


(defun moz-wait-for-process (proc &optional wait)
  "Wait for 'busy property of the process to become nil.
If SEC-PROMPT is non-nil return if secondary prompt is detected
regardless of whether primary prompt was detected or not.  If
WAIT is non-nil wait for WAIT seconds for process output before
the prompt check, default 0.01s. "
  ;; (unless (eq (process-status proc) 'run)
  ;;   (error "MozRepl process has died unexpectedly."))
  (setq wait (or wait 0.01))
  (save-excursion
    (while (or (accept-process-output proc wait)
	       (process-get proc 'busy)))))


;; (defun inferior-moz-track-proc-busy (comint-output)
;;   "track if process returned the '>' prompt and mark it as busy if not."
;;   (if (string-match "\\(\\w+\\)> \\'" comint-output)
;;       (process-put (get-buffer-process (current-buffer)) 'busy nil)
;;     (process-put (get-buffer-process (current-buffer)) 'busy t)))

;; (defun zotelo-insert-busy-hook ()
;;   "Add `inferior-moz-track-proc-busy' to comint-outbut-filter hook "
;;   (add-hook 'comint-output-filter-functions 'inferior-moz-track-proc-busy nil t)
;;   )

;; (add-hook 'inferior-moz-hook 'zotelo-insert-busy-hook)

(provide 'zotelo)
;;; zotelo.el ends here.
