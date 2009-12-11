(ns tests-clj-pi-calculus
  (:use [clojure.test]
        [clj-pi-calculus]))



(deftest make-url-1
  (is (= "http://google.es"
         (. (make-url "google.es") toString))))

(deftest make-url-2
  (is (= "http://google.es:8080"
         (. (make-url "google.es:8080") toString))))

(deftest make-url-3
  (is (= "http://google.es:8080?a=%20a&b=%20b"
         (. (make-url "google.es:8080" {:a " a" :b " b"}) toString))))