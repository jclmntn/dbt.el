;; Dependencies
(require 'transient)

;; Custom Variables
(defcustom dbtel-global-prefix
  `("")
  "Global DBT prefixes.
The prefixes set here are used every time the DBT process is executed as a subprocess. This is useful for DBT projects which use tools like poetry or uv.
"
  :package-version '(dbtel . "0.1.0")
  :group 'dbtel-commands)

;; Functions
(defun dbtel--project-directory ()
  "Returns the name of the directory it it detecs a dbt_project.yml file."
    (locate-dominating-file default-directory "dbt_project.yml"))

(defun dbtel-process-dbt-arguments (&rest args)
  "Prepares DBT arguments to spawn a process."
  (string-join (append dbtel-global-prefix (flatten-list (list "dbt" args))) " "))

;;;###autoload
(defun dbtel-debug ()
  "Runs dbt debug in a compilation buffer. Uses `dbtel-global-prefix' if available."
  (interactive)
  (when (dbtel--project-directory)
    (let ((default-directory (dbtel--project-directory)))
      (compile (dbtel-process-dbt-arguments "debug")))))

;;;###autoload
(defun dbtel-run ()
  "Runs dbt run in a compilation buffer. Uses `dbtel-global-prefix' if available."
  (interactive)
  (when (dbtel--project-directory)
    (let ((default-directory (dbtel--project-directory)))
      (compile (dbtel-process-dbt-arguments "run")))))

(transient-define-prefix dbtel-menu ()
  "Transient Menu for DBT"
  ["Commands"
   ("d" "debug" dbtel-debug)
   ("r" "run" dbtel-run)])

(provide 'dbt.el)
