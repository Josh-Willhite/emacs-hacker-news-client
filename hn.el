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
  (hn-render-comments (hn-get-comments))
  )

(defun hn-render-comments (comments)
  (let (comment-point)
  (dolist (comment comments)
    ;; if next line doesn't have a * then insert a newline and add a star
    ;; else go to next line an add a star
    ;; while indent-following > 1
    ;; do above and decrement indent-following
    (setq comment-point (point))
    (while )
    (insert (format "* [[%s][%s]] [[%s/item?id=%s][C]]\n"
                    (alist-get `href row) (car (alist-get `title row)) *hn-url*  (alist-get `id row)))
     )
  )
)


(defun hn-get-comments ()
    ;; (print (format "THE URL: %s"(org-element-property :raw-link (org-element-context))) (current-buffer))
    ;; (class . "comment-tree"))
    ;; ((class . "athing comtr ")
    ;; each comment has
    ;; - comment text
    ;; - number following to indent
    ;; - id (for later features)
    ;; ((comment . "the comment") (id . 1234) (children . (another node)))
    ;; 
  ;; (switch-to-buffer (get-buffer-create "the comments")
  ;;                   (pp (dom-by-class comments-dom  "athing comtr") (get-buffer "the comments"))
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
