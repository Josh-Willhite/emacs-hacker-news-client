(defun hacker-news ()
  "display HackerNews in buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "hacker news"))
  (render-rows (parse-rows (hn-get-dom *hn-url*)))
  (org-mode)
)

(defun hn-get-comments ()
  "display comments for the current headline"
  (interactive)
  (hn-render-comments (hn-parse-comments))
  )


(defun hn-render-comments (comments)
  ;; Clear potential existing comments
  ;; (org-toggle-narrow-to-subtree)
  ;; (org-show-subtree)
  ;; (forward-line)
  ;; (delete-region (point) (end-of-buffer))
  (let (comment-stack headline-point)
    (setq headline-point (point))
    (end-of-line)
    (insert "\n")
    (dolist (comment (reverse comments))
      (end-of-line)
      (if comment-stack
          (insert (format "%s** " (pop comment-stack)))
        (progn
          (insert "** ")
        )
      )
      (open-line 1)
      (forward-line)
      (if (alist-get `text comment)
          (progn
            (insert (alist-get `text comment))
            (fill-paragraph)
          )
      )
      (open-line 1)
      (forward-line)
      (setq comment-stack (hn-add-comment-indents-to-stack comment-stack
                (string-to-number (alist-get `indent-following comment))))
    )
    (goto-char headline-point)
    (outline-hide-subtree)
  )
)

(defun hn-add-comment-indents-to-stack (comment-stack indents)
  (let (comment-stack-out count)
    (setq count 0)
    (while (> (1- indents) count)
      (if comment-stack
          (push (format "%s*" (pop comment-stack)) comment-stack-out)
        (push "*" comment-stack-out)
      )
      (setq count (1+ count))
    )

    (setq comment-stack-out (append (reverse comment-stack-out) comment-stack))
    comment-stack-out
  )
)

(defun hn-parse-comments ()
  (let (comments id text indent-following comments-dom)
    (setq comments-dom (hn-get-dom (org-element-property :raw-link (org-element-context))))
    (dolist (item (dom-by-class comments-dom "athing comtr"))
              (setq text (dom-texts (dom-by-class item "commtext c00")))
              (setq indent-following (alist-get `n (dom-attributes (dom-by-class item "togg"))))
              (setq id (alist-get `id (dom-attributes item)))
              (push `((id . ,id) (indent-following . ,indent-following) (text . ,text)) comments)
      )
    comments
  )
)

(defvar *hn-url* "https://news.ycombinator.com")

(defun render-rows (rows)
  (dolist (row rows)
    (insert (format "* [[%s][%s]] [[%s/item?id=%s][Comments]]\n"
                    (alist-get `href row) (car (alist-get `title row)) *hn-url*  (alist-get `id row)))
    )
  )

;; (defun parse-rows (hn-dom)
;;   (let (rows id story hrek title rank row)
;; (pp (dom-by-class hn-dom "itemlist") (current-buffer))
;;   (dolist (item (dom-by-class hn-dom "athing"))
;;       (setq id (alist-get `id (dom-attributes item)))
;;       (setq story-link (dom-by-class item "storylink"))
;;       (setq href (alist-get `href (dom-attributes story-link)))
;;       (setq title (dom-children story-link))
;;       (setq rank (car (dom-children (dom-by-class item "rank"))))
;;       (setq row `((id . ,id) (rank . ,rank) (title . ,title) (href . ,href)))
;;       (push row rows)
;;     )
;;   rows
;;   )
;;   )


(defun parse-rows (hn-dom)
(let (row rows subitem)
        (
                dolist (item (dom-by-class hn-dom "\\(subtext\\|athing\\)"))
                (if (equal (alist-get `class (dom-attributes item)) "athing")
                        ;; Content of submission
                        (progn
                        (setq subitem (dom-by-class item "athing"))
                        (setq row
                                `(
                                (id . ,(alist-get `id (dom-attributes subitem)))
                                (title . ,(dom-children (dom-by-class subitem "storylink")))
                                (href . ,(alist-get `href (dom-attributes (dom-by-class subitem "storylink"))))
                                )
                        )
                        )
                        ;; Meta data for submission
                        (progn
                        (setq subitem (dom-by-class item "subtext"))
                        (setq row
                                (append
                                        `(
                                        (score . ,(dom-children (dom-by-class subitem "score")))
                                        (user . ,(dom-children (dom-by-class subitem "hnuser")))
                                        (age . ,(dom-children (dom-children (dom-by-class subitem "age"))))
                                        (comments . ,(dom-children (reverse (dom-by-tag (car subitem) `a))))
                                        )
                                row
                                )
                        )
                        (push row rows)
                        )
                )
        )
        rows
)
)


(defun hn-get-dom (hn-url)
  (save-excursion
  (with-current-buffer  (url-retrieve-synchronously hn-url)
    (url-retrieve-synchronously hn-url)
    (libxml-parse-html-region (point-min) (point-max))
    )
  )
)
