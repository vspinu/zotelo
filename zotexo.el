;; zotexo.el --- synchronize zotero collections in emacs.
;;
;; Filename: zotero.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2011-2011, Spinu Vitalie, all rights reserved.
;; Created: Oct 2 2011
;; URL: http://code.google.com/p/zotexo/
;; Keywords: zotero, emacs, reftex, bibtex, MozRepl
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
;; Features that might be required by this library:
;; reftex
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Commentary:
;; See http://code.google.com/p/zotexo/ and `zotexo-minor-mode' for more info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Change log:
;;;; Code:

;; (require 'moz)
;; (require 'reftex)

(defvar zotexo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-czu" 'zotexo-update-database)
    (define-key map "\C-cze" 'zotexo-export-secondary)
    (define-key map "\C-czs" 'zotexo-set-collection)
    (define-key map "\C-czc" 'zotexo-set-collection)
    (define-key map "\C-czm" 'zotexo-mark-for-auto-update)
    (define-key map "\C-czr" 'zotexo-reset)
    (define-key map "\C-czt" 'zotexo-set-translator)
    (define-key map "\C-czT" 'zotexo-toggle-auto-update)
    map))

(defvar zotexo--check-timer nil
  "Global timer executed at `zotexo-check-interval' seconds. ")

(defvar zotexo-check-interval 5
  "Seconds between checks for zotero database changes.
Note that zotexo uses idle timer. Yeach time emacs is idle for
this number of seconds zotexo checks for an update.")

;; (defvar zotexo-use-ido t
;;   "If t will try to use ido interface")

(defvar zotexo-auto-update-all nil
  "If t zotexo checks for the change in zotero database
every `zotexo-check-interval' seconds and auto updates all
buffers with active `zotexo-minor-mode'.

If nil the only updated files are those with non-nil file local
variable `zotexo-auto-update'. See
`zotexo-mark-for-auto-update'. ")

(defgroup zotexo nil "Customization for Zotexo")

(defcustom zotexo-default-translator 'BibTeX
  "The name of the default zotero-translator to use (a symbol).

Must correspond to one of the keys in `zotexo-translators' alist.

You can set this varialbe interactively with
`zotexo-set-translator'.
"
  :type 'symbol
  :group 'zotexo)

(defcustom zotexo-translators
  '((BibTeX . "9cb70025-a888-4a29-a210-93ec52da40d4")
    (BibLaTeX . "ba4cd274-f24e-42cf-8ff2-ccfc603aacf3"))
  "An alist of zotero translators ids.
Each cons cell consists of an user frendly key and an unique
identifier used by zotero.

Not all of the listed translatros are the default zotero
translators. You have to search and download them yourself.

Standard BibTeX (zotero): '9cb70025-a888-4a29-a210-93ec52da40d4'
BibLaTeX (downloaded from https://code.google.com/p/zotero-biblatex-export/): 'ba4cd274-f24e-42cf-8ff2-ccfc603aacf3'
"
  :group 'zotexo
  :type 'alist
)

(defvar zotexo--get-zotero-database-js
  "var zotexo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
zotexo_zotero.getZoteroDatabase().path;")

(defvar zotexo--get-zotero-storage-js
  "var zotexo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
zotexo_zotero.getStorageDirectory().path;")

(defvar zotexo--auto-update-is-on nil
  "If t zotexo monitors changes in zotero database and reexports
  collections if needed.
  You can toggle it with  'C-c z T'
")

(defvar zotexo--ignore-files (list "_region_.tex"))


(defvar zotexo--verbose nil)
(defun zotexo-verbose ()
  "Toggle zotexo debug messages (all printed in *message* buffer)"
  (interactive)
  (message "zotexo verbose '%s'" (setq zotexo--verbose (not zotexo--verbose))))

(defun zotexo--message (str)
  (when zotexo--verbose
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (insert (format "\n zotexo message on [%s]\n %s\n" (current-time-string) str)))))

(defvar zotexo--render-collection-js
  "var zotexo_render_collection = function() {
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
(defvar zotexo--export-collection-js
  "
var zotexo_filename=('%s');
var zotexo_id = %s;
var zotexo_translator_id = '%s';
var zotexo_prefs = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefService).getBranch('extensions.zotero.');
var zotexo_file = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
var zotexo_recColl = zotexo_prefs.getBoolPref('recursiveCollections');
zotexo_file.initWithPath(zotexo_filename);
//split
var zotexo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
var zotexo_collection = true;
var zotexo_translator = new zotexo_zotero.Translate('export');
if (zotexo_id != 0){ //not all collections
    zotexo_collection = zotexo_zotero.Collections.get(zotexo_id);
    zotexo_translator.setCollection(zotexo_collection);
};
//split
if(zotexo_collection){
    zotexo_translator.setLocation(zotexo_file);
    zotexo_translator.setTranslator(zotexo_translator_id);
    zotexo_prefs.setBoolPref('recursiveCollections', true);
    zotexo_translator.translate();
    zotexo_prefs.setBoolPref('recursiveCollections', zotexo_recColl);
    zotexo_out=':MozOK:';
}else{
    zotexo_out='Collection with the id ' + zotexo_id + ' does not exist.';
};
//split
zotexo_out;
"
  "Command to be sent to zotero request export."
  )

(defvar zotexo--dateModified-js
  "var zotexo_zotero = Components.classes['@zotero.org/Zotero;1'].getService(Components.interfaces.nsISupports).wrappedJSObject;
var zotexo_id = %s;
var zotexo_collection = zotexo_zotero.Collections.get(zotexo_id);
if(zotexo_collection){
   ':MozOK:' + zotexo_collection.dateModified;
}else{
   'Collection with the id ' + zotexo_id + ' does not exist.';
}"

  "Command to get last modification date of the collection.")

(define-minor-mode zotexo-minor-mode
  "zotexo minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, `zotexo-set-collection' prompts
for zotero collection and stores it as file local variable . To
manually update the BibTeX data base call
`zotexo-update-database'. The \"file_name.bib\" file will be
created with the exported zotero items. To specify the file_name
just insert insert \\bibliography{file_name} anywhere in the
buffer.

This mode is designed mainly for latex modes and works in
conjunction with RefTex, but it can be used in any other mode
such as org-mode.

The following keys are bound in this minor mode:

\\{zotexo-minor-mode-map}"
  nil
  (zotexo--auto-update-is-on " ZX" " zx")
  :keymap zotexo-minor-mode-map
  :group 'zotexo
  (if zotexo-minor-mode
      (progn
        (unless (timerp zotexo--check-timer)
          (setq zotexo--check-timer
                (run-with-idle-timer 5 zotexo-check-interval 'zotexo--check-and-update-all)))
        )
    (unless
        (loop for b in (buffer-list)
              for is-zotexo-mode = (buffer-local-value 'zotexo-minor-mode b)
              until is-zotexo-mode
              finally return is-zotexo-mode)
      ;; if no more active zotexo mode, cancel the timer and kill the process
      (when (timerp zotexo--check-timer)
        (cancel-timer zotexo--check-timer)
        (setq zotexo--check-timer nil)
        (delete-process (zotexo--moz-process))
        (kill-buffer zotexo--moz-buffer)
        )
      )
    )
  )


(defun zotexo--check-and-update-all ()
  "Function run with `zotexo--check-timer'."
  (when zotexo--auto-update-is-on
    (let ( out id any-z-buffer-p z-buffer-p)
      (zotexo--message  "Zotexo checking for updates.")
      (dolist (b  (buffer-list)) ;iterate through zotexo buffers
        (setq z-buffer-p (buffer-local-value 'zotexo-minor-mode b))
        (when z-buffer-p
          (setq any-z-buffer-p t))
        (when (and
               ;; zotexo buffer?
               z-buffer-p
               ;; exclusion reg-exp  matched?,
               (not (delq nil (mapcar (lambda (reg)
                                        (string-match reg (buffer-name b)))
                                      zotexo--ignore-files)))
               ;; collection is set?,
               (assoc 'zotero-collection (buffer-local-value 'file-local-variables-alist b))
               ;; auto-update-all?, auto-update?
               (let ((auto-update
                      (assoc 'zotexo-auto-update (buffer-local-value 'file-local-variables-alist b))))
                 (if (and zotexo-auto-update-all (null auto-update))
                     (setq auto-update '(t . t)))
                 (cdr auto-update))
               )
          (with-current-buffer b
            (ignore-errors
              (setq id (zotexo-update-database t))))
          (when id
            (setq out
                  (append (list (buffer-name b)) out))
          )))
      (if (> (length out) 0)
          (message "Bibliography updated in %s buffers: %s." (length out) out))
      (when (and (not any-z-buffer-p)
                 (timerp zotexo--check-timer))
        ;; stop timer if no more zotexo buffers
        (cancel-timer zotexo--check-timer)
        (setq zotexo--check-timer nil)
        (delete-process (zotexo--moz-process))
        (kill-buffer zotexo--moz-buffer)
        )
      )))



(defun zotexo-export-secondary ()
  "Export zotero collection into  secondary BibTeX database.

Before export, ask for a secondary BibTeX database and zotero
collection to be exported into the database. Secondary databases
are those in \\bibliography{file1, file2, ...}, except the first
one.

Error ocures if there is only one (primary) BibTeX database in
\\bibliography{...} listing.

Error if zotero collection is not found by MozRepl"
  (interactive)
  (let* ((files (zotexo--locate-bibliography-files default-directory))
	 (bibfile (cond
		   ((< (length files) 2)
		   (error "No secondary databases (\\bibliography{...} lists contain less than 2 files)."))
		   ((= (length files) 2)
		    (cadr files))
		   (t (zotexo--read (cdr files) "File to update: "))))
	 (collection (zotexo-set-collection
		      (format "Export into '%s': " (file-name-nondirectory bibfile))
		      'no-update 'no-set)))
    (zotexo-update-database nil bibfile (get-text-property 0 'zotero-id collection))))
  

(defun zotexo-set-translator ()
  "Ask to choose from available translators and set `zotexo-default-translator'."
  (interactive)
  (let ((tnames (mapcar (lambda (el) (symbol-name (car el)))
                        zotexo-translators)))
    (setq zotexo-default-translator
          (intern (zotexo--read tnames "Choose translator: ")))))
    
  
  

(defun zotexo-update-database (&optional check-zotero-change bibfile id)
  "Update the primary BibTeX database associated with the current buffer.

Primary database is the first file in \\bibliography{file1, file2,
...}, list. If you want to export into a different file use
`zotexo-update-database-secondary'.

If BIBFILE is supplied, don't infer from \\bibliography{...} statement.

If ID is supplied, don't infer collection id from file local variables.

Through error if zotero collection is not found by MozRepl"
  (interactive)
  (let ((bibfile (or bibfile
		     (car (zotexo--locate-bibliography-files default-directory))))
        (proc  (zotexo--moz-process))
        (id (or id
		(zotexo--get-local-collection-id)))
        (file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
        (trans-id (cdr (assoc zotexo-default-translator zotexo-translators)))
        all-colls-p cstr bib-last-change zotero-last-change com1)
    (unless trans-id
      (error "Cannot find translator %s in `zotexo-translators' alist" zotexo-default-translator))
    (when (null bibfile)
      (setq file-name (concat file-name "_zotexo_.bib"))
      (setq bibfile (concat default-directory file-name ))
      (message "Cannot find bibliography reference in the file '%s'.\n  Ussing '%s' for BibTeX export." (buffer-name) file-name)
      )
    (setq bib-last-change (nth 5 (file-attributes bibfile))) ;; nil if bibfile does not exist yet
    (setq bibfile (replace-regexp-in-string "\\\\" "\\\\"
					    (convert-standard-filename bibfile) nil 'literal))
    (when (and (called-interactively-p) (null id))
      (zotexo-set-collection "Zotero collection is not set. Choose one: " 'no-update)
      (setq id (zotexo--get-local-collection-id)))
    (unless (file-exists-p (file-name-directory bibfile))
      (error "Directory '%s' does not exist; create it first." (file-name-directory bibfile)))
    (when check-zotero-change
      (set-time-zone-rule t)
      (with-current-buffer (moz-command (format zotexo--dateModified-js id))
        (goto-char (point-min))
        (when (re-search-forward ":MozOK:" nil t) ;; ingore the error it is  cought latter
          (setq zotero-last-change (date-to-time
                                    (buffer-substring-no-properties (point) (point-max))))
          )))
    (when (and id
               (or (null check-zotero-change)
                   (null bib-last-change)
                   (time-less-p bib-last-change zotero-last-change)))
      (setq cstr (format zotexo--export-collection-js bibfile id trans-id))
      (zotexo--message (format "Executing command: \n\n (moz-command (format zotexo--export-collection-js '%s' %s))\n\n translated as:\n %s\n"
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
      (message "'%s' updated successfully" (file-name-nondirectory bibfile))
      id)
    )
  )

(defcustom zotexo-bibliography-commands '("bibliography" "nobibliography" "zotexo")
  "List of commands which specify databases to use.

For example \\bibliography{file1,file2} or \\zotexo{file1,file2}
both specify that file1 is a primary database and file2 is the
secondary one. 
")
  

(defun zotexo--locate-bibliography-files ()
  ;; Scan buffer for bibliography macro and return as a list.
  ;; Modeled after the corresponding reftex function

  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         (concat
                                        ;           "\\(\\`\\|[\n\r]\\)[^%]*\\\\\\("
          "\\(^\\)[^%\n\r]*\\\\\\("
          (mapconcat 'identity zotexo-bibliography-commands "\\|")
          "\\){[ \t]*\\([^}]+\\)") nil t)
        (split-string   (when (match-beginning 3)
                          (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
                        "[ \t\n\r]*,[ \t\n\r]*"))))


(defun zotexo-set-collection (&optional prompt no-update no-file-local)
  "Ask for a zotero collection.
Ido interface is used by default. If you don't like it set `zotexo-use-ido' to nil.


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
    (moz-command (format zotexo--render-collection-js
			 (process-get (zotexo--moz-process) 'moz-prompt)))
    (moz-command "zotexo_render_collection()" buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (zotexo--message (format "Collections:\n %s" 
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
      (setq name (zotexo--read (cons (propertize "*ALL*" 'zotero-id "0") (nreverse colls))
			       prompt))
      (unless no-file-local
	(save-excursion
	  (add-file-local-variable 'zotero-collection
				   (propertize (get-text-property 0 'zotero-id name)
					       'name (substring-no-properties name)))
	  (hack-local-variables))
	(unless no-update
	  (zotexo-update-database)))
      name
      )))



(defun zotexo-mark-for-auto-update (&optional unmark)
  "Mark current file for auto-update.

If the file is marked for auto-update zotexo runs
`zotexo-update-database' on it whenever the zotero data-base is
updated.

File is marked by adding file local variable
'zotexo-auto-update'. To un-mark the file call this function with
an argument or just delete or set to nil the local variable at
the end of the file.
"
  (interactive "P")
  (save-excursion
    (if unmark
        (progn
          (delete-file-local-variable 'zotexo-auto-update)
          (setq file-local-variables-alist
                (assq-delete-all 'zotexo-auto-update file-local-variables-alist)))
      (add-file-local-variable 'zotexo-auto-update t)
      (hack-local-variables)
      )
    )
  )


(defun zotexo-reset ()
  "Reset zotexo."
  (interactive)
  (delete-process (zotexo--moz-process))
  (kill-buffer zotexo--moz-buffer)
  (message "Killed moz process")
  )


(defun zotexo-toggle-auto-update ()
  "Togles auto-updating in all buffers.
Note that once toggled in your firefox and MozRepl must be
started, otherwise you will start getting error screens. "
  (interactive)
  (setq zotexo--auto-update-is-on (not zotexo--auto-update-is-on))
  )


(defun zotexo--get-local-collection-id ()
  (cdr (assoc 'zotero-collection file-local-variables-alist)))

(defun zotexo--read (collections &optional prompt)
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
			     nil t nil nil)
      (when reset-ido
	(remove-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
	(remove-hook 'choose-completion-string-functions 'ido-choose-completion-string)
	(remove-hook 'kill-emacs-hook 'ido-kill-emacs-hook)
	))))




;;;; Moz utilities

(defvar zotexo--moz-host "localhost")
(defvar zotexo--moz-port 4242)
(defvar zotexo--moz-buffer nil)
(defvar zotexo--startup-error-count 0)
(defvar zotexo--max-errors 10)

(defun zotexo--moz-process ()
  "Return inferior MozRepl process.  Start it if necessary."
  (or (if (buffer-live-p zotexo--moz-buffer)
          (get-buffer-process zotexo--moz-buffer))
      (progn
        (zotexo--moz-start-process)
        (zotexo--moz-process))))

(defun zotexo--moz-start-process ()
  "Start mozrepl process and connect to Firefox.
Note that you have to start the MozRepl server from Firefox."
  (interactive)
  (setq zotexo--moz-buffer (get-buffer-create "*ZotexoMozRepl*"))
  (condition-case err
      (let ((proc (make-network-process :name "ZotexoMozRepl" :buffer zotexo--moz-buffer
					:host zotexo--moz-host :service zotexo--moz-port
					:filter 'moz-ordinary-insertion-filter)))
        (sleep-for 0 100)
	(set-process-query-on-exit-flag proc nil)
        (with-current-buffer zotexo--moz-buffer
          (set-marker (process-mark proc) (point-max)))
        (setq zotexo--startup-error-count 0))
    (file-error
     (let ((buf (get-buffer-create "*MozRepl Error*")))
       (setq zotexo--startup-error-count (1+ zotexo--startup-error-count))
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
          (format "Zotexo Error Count: %s\n\n%s"
                  zotexo--startup-error-count
                  (if (not (and (>= zotexo--startup-error-count 10)
                                zotexo--auto-update-is-on))
                      "If zotexo auto-update is on, press \"C-c z t\" to turn it off."
                    (setq zotexo--auto-update-is-on nil)
                    (setq zotexo--startup-error-count 0)
                    "Too many errors. Zotexo auto-update was turned off!\nUse [C-c z t] to switch it on.")))
         )
       (kill-buffer "*ZotexoMozRepl*")
       (display-buffer buf t)
       (error "Zotexo cannot start MozRepl")
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
  (let ((proc (zotexo--moz-process)))
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
	    (save-excursion
	      (set-buffer buf)
	      (erase-buffer)
	      (set-marker (process-mark proc) (point-min))
	      (process-put proc 'busy t)
	      (process-send-string proc (concat com "\n"))
	      (moz-wait-for-process proc)
	      ;;(delete-region (point-at-bol) (point-max))
	      )
	    (zotexo--message "Moz-command finished"))
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

;; (defun zotexo-insert-busy-hook ()
;;   "Add `inferior-moz-track-proc-busy' to comint-outbut-filter hook "
;;   (add-hook 'comint-output-filter-functions 'inferior-moz-track-proc-busy nil t)
;;   )

;; (add-hook 'inferior-moz-hook 'zotexo-insert-busy-hook)

(provide 'zotexo)
;;; zotexo.el ends here.
