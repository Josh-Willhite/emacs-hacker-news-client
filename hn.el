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

;; how deep is the current comment
;; how many comment should we indent
;; literally user a list
;; 
(defun hn-render-comments (comments)
  (let (comment-stack `("*"))
  (end-of-line)
  (insert "\n")
  (dolist (comment (reverse comments))
    (end-of-line)
    (insert (format "%s*" (pop comment-stack)))
    (open-line 1)
    (forward-line)
    ;; (insert (format "INDENT: %s" (alist-get `indent-following comment)))
    (insert (alist-get `text comment))
    (fill-paragraph)
    (open-line 1)
    (forward-line)

    (setq count 0)
    (while (>= (1- (string-to-number (alist-get `indent-following comment))) count)
      (if ((length))
      (setq count (1+ count))
      )
  )
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
    (insert (format "* [[%s][%s]] [[%s/item?id=%s][C]]\n"
                    (alist-get `href row) (car (alist-get `title row)) *hn-url*  (alist-get `id row)))
    )
  )

(defun parse-rows (hn-dom)
  (let (rows id story hrek title rank row)
  (dolist (item (dom-by-class hn-dom "athing"))
      (setq id (alist-get `id (dom-attributes item)))
      (setq story-link (dom-by-class item "storylink"))
      (setq href (alist-get `href (dom-attributes story-link)))
      (setq title (dom-children story-link))
      (setq rank (car (dom-children (dom-by-class item "rank"))))
      (setq row `((id . ,id) (rank . ,rank) (title . ,title) (href . ,href)))
      (push row rows)
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
