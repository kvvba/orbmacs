(setq bongo-default-directory "~/Music")
(setq bongo-prefer-library-buffers nil)
(setq bongo-insert-whole-directory-trees t)
(setq bongo-logo nil)
(setq bongo-display-track-icons nil)
(setq bongo-display-track-lengths t)
(setq bongo-display-header-icons nil)
(setq bongo-display-playback-mode-indicator t)
(setq bongo-display-inline-playback-progress t)
(setq bongo-join-inserted-tracks nil)
(setq bongo-field-separator (propertize " · " 'face 'shadow))
(setq bongo-mark-played-tracks t)
(setq bongo-header-line-mode nil)
(setq bongo-mode-line-indicator-mode nil)
(setq bongo-enabled-backends '(vlc mpv))
(setq bongo-vlc-program-name "cvlc")

;;; Bongo playlist buffer
(defvar prot/bongo-playlist-delimiter
  "\n******************************\n\n"
  "Delimiter for inserted items in `bongo' playlist buffers.")

(defun prot/bongo-playlist-section ()
  (bongo-insert-comment-text
   prot/bongo-playlist-delimiter))

(defun prot/bongo-paylist-section-next ()
  "Move to next `bongo' playlist custom section delimiter."
  (interactive)
  (let ((section "^\\*+$"))
    (if (save-excursion (re-search-forward section nil t))
        (progn
          (goto-char (point-at-eol))
          (re-search-forward section nil t))
      (goto-char (point-max)))))

(defun prot/bongo-paylist-section-previous ()
  "Move to previous `bongo' playlist custom section delimiter."
  (interactive)
  (let ((section "^\\*+$"))
    (if (save-excursion (re-search-backward section nil t))
        (progn
          (goto-char (point-at-bol))
          (re-search-backward section nil t))
      (goto-char (point-min)))))

(defun prot/bongo-playlist-mark-section ()
  "Mark `bongo' playlist section, delimited by custom markers.
The marker is `prot/bongo-playlist-delimiter'."
  (interactive)
  (let ((section "^\\*+$"))
    (search-forward-regexp section nil t)
    (push-mark nil t)
    (forward-line -1)
    ;; REVIEW any predicate to replace this `save-excursion'?
    (if (save-excursion (re-search-backward section nil t))
        (progn
          (search-backward-regexp section nil t)
          (forward-line 1))
      (goto-char (point-min)))
    (activate-mark)))

(defun prot/bongo-playlist-kill-section ()
  "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`prot/bongo-playlist-kill-section'."
  (interactive)
  (prot/bongo-playlist-mark-section)
  (bongo-kill))

(defun jakub/bongo-playlist-play ()
  "Play `bongo' track and determine further conditions."
  (interactive)
  (unless (bongo-playlist-buffer)
    (bongo-playlist-buffer))
  (when (or (bongo-playlist-buffer-p)
            (bongo-library-buffer-p))
    (unless (bongo-playing-p)
      (with-current-buffer (bongo-playlist-buffer)
				(bongo-play-line (bongo-point-at-first-line-satisfying 'bongo-first-line-p))
        (bongo-recenter)))))

(defun prot/bongo-playlist-random-toggle ()
  "Toggle `bongo-random-playback-mode' in playlist buffers."
  (interactive)
  (if (eq bongo-next-action 'bongo-play-random-or-stop)
      (bongo-progressive-playback-mode)
    (bongo-random-playback-mode)))

(defun prot/bongo-playlist-reset ()
  "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
  (interactive)
  (when (bongo-playlist-buffer-p)
    (bongo-stop)
    (bongo-reset-playlist)))

(defun prot/bongo-playlist-terminate ()
  "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
  (interactive)
  (when (bongo-playlist-buffer-p)
    (bongo-stop)
    (bongo-erase-buffer)))

(defun prot/bongo-playlist-insert-playlist-file ()
  "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`jakub/bongo-play-playlist'.

The available options at the completion prompt point to files
that hold filesystem paths of media items.  Think of them as
'directories of directories' that mix manually selected media
items.

Also see `prot/bongo-dired-make-playlist-file'."
  (interactive)
  (let* ((path "~/Music/playlists/")
         (dotless directory-files-no-dot-files-regexp)
         (playlists (mapcar
                     'abbreviate-file-name
                     (directory-files path nil dotless)))
         (choice (completing-read "Insert playlist: " playlists nil t)))
    (if (bongo-playlist-buffer-p)
        (progn
          (save-excursion
            (goto-char (point-max))
            (bongo-insert-playlist-contents
             (format "%s%s" path choice))
            (prot/bongo-playlist-section))
          (jakub/bongo-playlist-play))
      (user-error "Not in a `bongo' playlist buffer"))))

;;; Bongo + Dired (bongo library buffer)

(add-hook 'dired-mode-hook
					(lambda ()
						(cond ((string-match-p "^~\/Music\/.*$" dired-directory)
									 (bongo-dired-library-mode))
									(t "default"))))

(defun prot/bongo-dired-insert-files ()
  "Add files in a `dired' buffer to the `bongo' playlist."
  (let ((media (dired-get-marked-files)))
    (with-current-buffer (bongo-playlist-buffer)
      (goto-char (point-max))
      (mapc 'bongo-insert-file media)
      (prot/bongo-playlist-section))
    (with-current-buffer (bongo-library-buffer)
      (dired-next-line 1))))

(defun prot/bongo-dired-insert ()
  "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `prot/bongo-dired-insert-files' as well as
`prot/bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `prot/bongo-dired-library')."
  (interactive)
  (when (bongo-library-buffer-p)
    (unless (bongo-playlist-buffer-p)
      (bongo-playlist-buffer))
    (prot/bongo-dired-insert-files)
		(jakub/bongo-playlist-play)))

(defun prot/bongo-dired-make-playlist-file ()
  "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `prot/bongo-playlist-insert-playlist-file'."
  (interactive)
  (let* ((dotless directory-files-no-dot-files-regexp)
         (pldir "~/Music/playlists")
         (playlists (mapcar
                     'abbreviate-file-name
                     (directory-files pldir nil dotless)))
         (plname (completing-read "Select playlist: " playlists nil nil))
         (plfile (format "%s/%s" pldir plname))
         (media-paths
          (if (derived-mode-p 'dired-mode)
              ;; TODO more efficient way to do ensure newline ending?
              ;;
              ;; The issue is that we need to have a newline at the
              ;; end of the file, so that when we append again we
              ;; start on an empty line.
              (concat
               (mapconcat #'identity
                          (dired-get-marked-files)
                          "\n")
               "\n")
            (user-error "Not in a `dired' buffer"))))
    ;; The following `when' just checks for an empty string.  If we
    ;; wanted to make this more robust we should also check for names
    ;; that contain only spaces and/or invalid characters…  This is
    ;; good enough for me.
    (when (string-empty-p plname)
      (user-error "No playlist file has been specified"))
    (unless (file-directory-p pldir)
      (make-directory pldir))
    (unless (and (file-exists-p plfile)
                 (file-readable-p plfile)
                 (not (file-directory-p plfile)))
      (make-empty-file plfile))
    (append-to-file media-paths nil plfile)
    (with-current-buffer (find-file-noselect plfile)
      (delete-duplicate-lines (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (save-buffer)
      (kill-buffer))))

(provide 'music)
;;; music.el ends here
