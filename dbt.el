;; Dependencies
(require 'transient)
(require 'yaml)
(require 'cl-lib)

;; Variables
(defvar dbtel-project-file "dbt_project.yml" "DBT project configuration file.")

;; Custom Variables
(defcustom dbtel-global-prefix-option nil
  "Global DBT prefixes.  The prefixes set here are used every time the DBT process is executed as a
subprocess. This is useful for DBT projects which use tools like poetry or uv."
  :type '(choice (const :tag "No global prefix" nil)
                 (string :tag "User override"))
  :package-version '(dbtel . "0.1.0")
  :group 'dbtel-commands)

(defun dbtel--project-marker (dir)
  "Returns the name of the directory if it detects a DBT project configuration file."
  (let ((project-directory (locate-dominating-file dir dbtel-project-file)))
  (when project-directory
      (cons 'dbt-project project-directory))))

(cl-defmethod project-root ((project (head dbt-project)))
  "Extract the root path from a project matching 'dbt-project'."
  (cdr project))

(cl-defmethod project-files ((project (head dbt-project)) &optional dirs)
  (if-let ((vc-proj (project-try-vc (cdr project))))
      (project-files vc-proj dirs)
    (cl-call-next-method)))

;; TODO: Since we have overridden project-files, project-ignores is redundant. Check that.
(cl-defmethod project-ignores ((project (head dbt-project)) _dir)
  '("dbt_packages/" "logs/" ".venv/" ".git/" "target"))

;;;###autoload
(add-hook 'project-find-functions #'dbtel--project-marker)

(defun dbtel--global-prefix ()
  "Returns the user override or an empty string."
  (or dbtel-global-prefix-option ""))

(defun dbtel--get-model-name ()
  "Return the DBT model name for the current buffer (filename without extension)."
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun dbtel--file-refs-model-p (file model-name)
  "Return non-nil if FILE contains a ref to MODEL-NAME."
  (let ((regexp (concat "ref([[:space:]]*['\"]"
                        (regexp-quote model-name)
                        "['\"][[:space:]]*)")))
    (with-temp-buffer
      (insert-file-contents file)
      (re-search-forward regexp nil t))))

(defun dbtel--open-candidates (candidates empty-list)
  "Opens CANDIDATES with `project-find-file', or show EMPTY-MESSAGE if none."
  (if (null candidates)
      (message "parsed parents %s, but couldn't find them on disk." empty-list)
    (let ((original (symbol-function 'project-files)))
      (unwind-protect
          (progn
            (fset 'project-files (lambda (&rest _) candidates))
            (call-interactively #'project-find-file))
        (fset 'project-files original)))))

;; Parent node functions
(defun dbtel--get-model-parents ()
  "Collects in a list all the parents of a DBT model."
    (let ((str (buffer-substring-no-properties (point-min) (point-max)))
        (regexp "ref([[:space:]]*['\"]\\([^'\"]+\\)['\"][[:space:]]*)")
        matches)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t) (push (match-string 1) matches)))
      (seq-map (lambda (x) (concat x ".sql")) (nreverse matches))))

(defun dbtel-list-parents ()
  "Lists parents for the current DBT node."
  (interactive)
  (let ((parent-names (dbtel--get-model-parents)))
    (if (null parent-names)
        (message "No model dependencies found in this buffer.")
      (let* ((project (or (project-current t) (user-error "Not in a DBT project.")))
             (root (project-root project))
             (candidates
              (seq-filter (lambda (file)
                            (dbtel--source-file-p file root parent-names))
                          (project-files project))))
        (dbtel--open-candidates
         candidates
         parent-names)))))

;; Child node functions
(defun dbtel--get-model-children (project-files)
  "Return the subset of PROJECT-FILES that ref the current buffer's model."
  (let ((model-name (dbtel--get-model-name)))
    (seq-filter
     (lambda (file) (dbtel--file-refs-model-p file model-name))
     project-files)))

(defun dbtel-list-children ()
  "List children of the current DBT node."
  (interactive)
  (let* ((project  (or (project-current t) (user-error "Not in a project")))
         (root     (project-root project))
         (children (dbtel--get-model-children (project-files project)))
         (candidates (seq-filter
                      (lambda (file) (not (dbtel--artifact-p file root)))
                      children)))
    (dbtel--open-candidates
         candidates
         (dbtel--get-model-name))))

(defun dbtel--artifact-p (file root)
  "Returns non-nil if FILE is a build artifact or nested repo copy."
  (let ((rel-path (file-relative-name file root))
        (root-name (file-name-nondirectory (directory-file-name root))))
    (or (string-match-p "target/\\(run\\|compiled\\)/" rel-path)
        (string-prefix-p root-name rel-path))))  

(defun dbtel--source-file-p (file root parent-names)
  "Returns non-nil if FILE is a clean source file matching one of PARENT-NAMES.
Rejects DBT build artifacts under target/run or target/compiled,
and nested repository copies under DBT-Analytics/."
  (let ((rel-path (file-relative-name file root))
        (root-name (file-name-nondirectory (directory-file-name root))))
    (and (member (file-name-nondirectory file) parent-names)
         (not (dbtel--artifact-p file root)))))

(defun dbtel--current-model-name ()
  "Return the DBT model name if the current buffer is a valid project SQL file.
Returns nil if the buffer is not visiting a file, is not a SQL file,
or sits outside the current DBT project."
  (let ((file (buffer-file-name)))
    (when (and file 
               (string= (file-name-extension file) "sql")
               (dbtel--project-marker default-directory))
      (file-name-base file))))

(defun dbtel--process-dbt-arguments (&rest args)
  "Prepares DBT arguments to spawn a process."
  (let* ((flat-args (flatten-list args))
         (upstream (member "--upstream" flat-args))
         (downstream (member "--downstream" flat-args))
         model
         cleaned-args)
    (while flat-args
      (let ((item (pop flat-args)))
        (cond
         ((eq item :model)
          (setq model (pop flat-args)))
         ((member item '("--upstream" "--downstream"))
          nil)
         (t
          (push item cleaned-args)))))
    (setq cleaned-args (nreverse cleaned-args))

    (when model
      (let ((qualified-model (concat (when upstream "+")
                                     model
                                     (when downstream "+"))))
        (setq cleaned-args (append cleaned-args (list "-s" qualified-model)))))
    (string-join (append (dbtel--global-prefix) (cons "dbt" cleaned-args)) " ")))

(defun dbtel--get-model-list ()
  "Lists DBT models. Uses `dbtel-global-prefix-option'."
  (when-let* ((proj (project-current))
              (all-files (project-files proj))
              (root (project-root proj))
              (models-dir (expand-file-name "models/" root)))
    (seq-keep (lambda (file) (when (and (string= (file-name-extension file) "sql")
                                        (file-in-directory-p file models-dir))
                               (file-name-base file))) all-files)))

(defun dbtel--compile (command &rest args)
  "Runs COMMAND in a compilation buffer."
  (let ((marker (dbtel--project-marker default-directory)))
    (when marker
    (let ((default-directory (cdr marker)))
      (compile (dbtel--process-dbt-arguments (list command args)))))))

;;;###autoload
(defun dbtel-debug (&rest args)
  "Runs 'dbt debug' in a compilation buffer. Uses `dbtel-global-prefix-option'."
  (interactive)
  (dbtel--compile "debug" args))

;;;###autoload
(defun dbtel-run-all (args)
  "Runs the entire project in a compilation buffer using 'dbt run'. Uses `dbtel-global-prefix-option'."
  (interactive (list (transient-args 'dbtel-run-dispatch)))
  (dbtel--compile "run" args))

(defun dbtel-run-this-model (args)
  "Runs the model on the current buffer file in a compilation buffer using 'dbt run'. Uses `dbtel-global-prefix-option'."
  (interactive (list (transient-args 'dbtel-run-dispatch)))
  (if-let ((model (dbtel--current-model-name))) (dbtel--compile "run" args :model model)
    (user-error "Current buffer is not a valid DBT model file")))

(defun dbtel-run-prompted (args)
  "Prompts for a DBT model name and runs it ."
  (interactive (list (transient-args 'dbtel-run-dispatch)))
  (let ((model (completing-read "Select DBT Model: " (dbtel--get-model-list))))
    (dbtel--compile "run" args :model model)))

(transient-define-prefix dbtel-goto ()
  "Goes to node of `relationship'. Pops up a list of candidates based on the project configuration."
  ["Node type"
   ("p" "parent" dbtel-list-parents)
   ("c" "child" dbtel-list-children)])

(transient-define-prefix dbtel-run-dispatch ()
  "Transient menu for running DBT models."
  ["Arguments & Modifiers"
   ("-F" "Full refresh" "--full-refresh")
   ("-u" "Include Upstream" "--upstream")
   ("-d" "Include Downstream" "--downstream")]
  ["Commands"
   ("a" "Run entire project" dbtel-run-all)
   ("t" "Run this model" dbtel-run-this-model)
   ("p" "Run prompted model" dbtel-run-prompted)])

(transient-define-prefix dbtel-dispatch ()
  "Transient Menu for DBT"
  ["Commands"
   ("d" "debug" dbtel-debug)
   ("r" "run" dbtel-run-dispatch)
   ("g" "go to" dbtel-goto)])



(provide 'dbt.el)
