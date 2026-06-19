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

(ert-deftest dbtel-test-run ()
  "A test to assert `dbtel-run' is generated as a `dbt run` command."
  (should (equal (dbtel-run) (string-join (append dbtel-global-prefix '("dbt run")) " ")))) 

(ert-deftest dbtel-test-debug ()
  "A test to assert `dbtel-debug' is generated as a `dbt debug` command."
  (should (equal (dbtel-debug) (string-join (append dbtel-global-prefix '("dbt debug")) " ")))) 
