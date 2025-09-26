;;; test-spark-sender.el --- Unit tests for spark-sender.el

(require 'ert)

(ert-deftest test-spark-sender-wrap-val-basic ()
  "Test basic val wrapping without implicit keyword."
  (let ((input "val x = 42")
        (expected "val x = { 42 }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-implicit ()
  "Test val wrapping with implicit keyword."
  (let ((input "implicit val spark = SparkSession.builder()")
        (expected "implicit val spark = { SparkSession.builder() }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-with-type ()
  "Test val wrapping with type annotation."
  (let ((input "val name: String = \"John\"")
        (expected "val name: String = { \"John\" }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-implicit-with-type ()
  "Test implicit val wrapping with type annotation."
  (let ((input "implicit val count: Int = 100")
        (expected "implicit val count: Int = { 100 }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-leading-whitespace ()
  "Test val wrapping with leading whitespace."
  (let ((input "  val result = calculation()")
        (expected "val result = { calculation() }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-multiline ()
  "Test the original failing case with multiline SparkSession."
  (let ((input "  implicit val spark: SparkSession = SparkSession
    .builder()
    .master(\"local\")
    .config(\"spark.hadoop.fs.s3a.aws.credentials.provider\", \"com.amazonaws.auth.profile.ProfileCredentialsProvider\")
    .config(\"spark.sql.mapKeyDedupPolicy\", \"LAST_WIN\")
    .config(\"spark.ui.enabled\", false)
    .getOrCreate()")
        (expected "implicit val spark: SparkSession = { SparkSession
    .builder()
    .master(\"local\")
    .config(\"spark.hadoop.fs.s3a.aws.credentials.provider\", \"com.amazonaws.auth.profile.ProfileCredentialsProvider\")
    .config(\"spark.sql.mapKeyDedupPolicy\", \"LAST_WIN\")
    .config(\"spark.ui.enabled\", false)
    .getOrCreate() }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-wrap-val-spaces-around-equals ()
  "Test val wrapping with spaces around equals sign."
  (let ((input "val data : List[Int]  =  List(1, 2, 3)")
        (expected "val data : List[Int]  = { List(1, 2, 3) }"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) expected))))

(ert-deftest test-spark-sender-no-match-var ()
  "Test that var declarations are not wrapped."
  (let ((input "var mutable = 42"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) input))))

(ert-deftest test-spark-sender-no-match-def ()
  "Test that def declarations are not wrapped."
  (let ((input "def function = () => 42"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) input))))

(ert-deftest test-spark-sender-no-match-no-equals ()
  "Test that val without equals is not wrapped."
  (let ((input "val someIdentifier"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) input))))

(ert-deftest test-spark-sender-no-match-random-text ()
  "Test that random text is not wrapped."
  (let ((input "This is just some random text with val in it"))
    (should (string= (spark-sender--wrap-val-in-curly-braces input) input))))

(provide 'test-spark-sender)
;;; test-spark-sender.el ends here
