(ns clj-cbor.header-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.header :as header]
    [clj-cbor.test-utils :refer :all]))


(defn check-header-int
  [value hex]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [data-out (java.io.DataOutputStream. baos)]
      (header/write data-out :unsigned-integer value))
    (is (= hex (bin->hex (.toByteArray baos))))))


(deftest header-int-representation
  (is (thrown? clojure.lang.ExceptionInfo
        (check-header-int -1 ""))
      "error on negative values")
  (check-header-int          0 "00")
  (check-header-int         23 "17")
  (check-header-int         24 "1818")
  (check-header-int        255 "18FF")
  (check-header-int        256 "190100")
  (check-header-int      65535 "19FFFF")
  (check-header-int      65536 "1A00010000")
  (check-header-int 2147483647 "1A7FFFFFFF")
  (check-header-int 2147483648 "1A80000000")
  (check-header-int 4294967295 "1AFFFFFFFF")
  (check-header-int 4294967296 "1B0000000100000000")
  (check-header-int 9223372036854775807 "1B7FFFFFFFFFFFFFFF")
  (check-header-int 9223372036854775808 "1B8000000000000000")
  (is (thrown? clojure.lang.ExceptionInfo
        (check-header-int 18446744073709551617N "1BFFFFFFFFFFFFFFFF"))
      "error on values over 8 bytes"))


(deftest header-int-reading
  (testing "values 0 - 23"
    (dotimes [i 24]
      (is (= i (header/read-code nil i))
          "should be represented directly")))
  (testing "reserved values"
    (is (thrown? clojure.lang.ExceptionInfo
          (header/read-code nil 28)))
    (is (thrown? clojure.lang.ExceptionInfo
          (header/read-code nil 29)))
    (is (thrown? clojure.lang.ExceptionInfo
          (header/read-code nil 30))))
  (testing "indefinite length"
    (is (= :indefinite (header/read-code nil 31))))
  (testing "invalid value"
    (is (thrown? IllegalArgumentException
          (header/read-code nil 32)))))
