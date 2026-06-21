(defcustom dbtel-global-prefix
  `("")
  "Global DBT prefixes.
The prefixes set here are used every time the DBT process is executed as a subprocess. This is useful for DBT projects which use tools like poetry or uv.
"
  :package-version '(dbtel . "0.1.0")
  :group 'dbtel-commands)

(defun dbtel--project-directory ()
  "Returns the name of the directory it it detecs a dbt_project.yml file."
    (locate-dominating-file default-directory "dbt_project.yml"))

(defun dbtel-process-dbt-arguments (&rest args)
  "Prepares DBT arguments to spawn a process."
  (string-join (append dbtel-global-prefix (flatten-list (list "dbt" args))) " "))

(defun dbtel-debug ()
  (compile (dbtel-process-dbt-arguments "debug")))

(defun dbtel-run ()
  (compile (dbtel-process-dbt-arguments "run")))

(provide 'dbt.el)
