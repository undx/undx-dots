(setq sql-postgres-login-params
      '((user :default "undx")
        (database :default "undx")
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
;;
(setq sql-connection-alist
      '((server1 (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "undx")
                 ; (sql-password "password")
                  (sql-database "undx"))
        (server2 (sql-product 'mysql)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "user")
                  (sql-password "password")
                  (sql-database "undx"))))


(defun my-sql-server1 ()
  (interactive)
  (my-sql-connect 'postgres 'server1))

(defun my-sql-server2 ()
  (interactive)
  (my-sql-connect 'mysql 'server2))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))
;; ;; connect to database
;; (setq sql-product product)
;; (if current-prefix-arg
;; 		(sql-connect connection connection)
;; 	(sql-connect connection)))
(defvar sql-servers-list
	'(("undx local Dev [pg]" my-sql-server1)
		("undx local Dev [my]" my-sql-server2))
	"Alist of server name and the function to connect")


(defun sql-connect-server (func)
	"Connect to the input server using tmtxt/sql-servers-list"
	(interactive
	 (helm-comp-read "Select server: " sql-servers-list))
	(funcall func))

(provide 'sql-setup)
