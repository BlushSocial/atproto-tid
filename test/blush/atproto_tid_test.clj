(ns blush.atproto-tid-test
  (:require [clojure.test :refer :all]
            [blush.atproto-tid :refer :all]))


(deftest create-test
  (testing "Testing tid/create; used as rkey"
    (is (= "3kmtfck6kq22s"
           (create 1709512159544000 24)))))

(deftest parse-test
  (testing "Testing timestamp parser"
    (is (= {:timestamp 1709512113158000, :clockid 10}
           (parse "3kmtfb5wxvk2e")))))
