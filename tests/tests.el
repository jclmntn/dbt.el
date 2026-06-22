(ert-deftest dbtel-test-process-arguments-as-run ()
  "A test to assert a dbt run command is generated as expected."
  (should (equal
           (dbtel-process-dbt-arguments "run")
           (string-join (append dbtel-global-prefix '("dbt run")) " "))))

(ert-deftest dbtel-test-process-arguments-as-debug ()
  "A test to assert a dbt debug command is generated as expected."
  (should (equal
           (dbtel-process-dbt-arguments "debug")
           (string-join (append dbtel-global-prefix '("dbt debug")) " "))))
