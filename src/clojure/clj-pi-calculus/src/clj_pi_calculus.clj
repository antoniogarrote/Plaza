;; @doc
;; A rough implementation of a Process Calculus for RESTful
;; Semantic Web Services

;; @date 10.12.2009

;; @author Antonio Garrote

(ns clj-pi-calculus
  (:require clojure.set)
  (:import (java.net HttpURLConnection URI URL)
           (com.hp.hpl.jena.rdf.model Model ModelFactory Resource Property RDFNode ResourceFactory)
           (com.hp.hpl.jena.graph.query Query)
           (com.hp.hpl.jena.query QueryFactory QueryExecutionFactory)))


;; auxiliary functions


(defn key-to-str [kw]
  (let [s (str kw)]
    (. s (substring 1 (. s length)))))


(defn http-params-from-map [map]
  (reduce
   (fn [acum k]
     (let [key (key-to-str k)
           value (k map)
           joiner (if (= acum "") "" "&")]
       (str acum joiner key "=" value)))
   ""
   (keys map)))


(defn make-url
  ([base] (make-url base {}))
  ([base params]
     (let [http-params (http-params-from-map params)
           joiner (if (= http-params "") "" "?")
           uri (new URI "http" (str "//" base joiner http-params) nil)]
       (. uri toURL))))


(defn read-buffer [buffer]
  "Reads a buffer until null is returned. The
   String read is returned as a result"
  (loop [acum ""]
    (let [next (. buffer readLine)]
      (if (nil? next)
        acum
        (recur (str acum next))))))


;; HTTP functions


(defn http-res
  "Makes a single HTTP get request and returns the
   read data and the connection object"
  ([method uri headers parameters options]
     (let [url (make-url uri parameters)
           url-object (doto
                        (. url (openConnection))
                        (.setRequestMethod method)
                        (.setDoInput true)
                        (.setDoOutput false))]
       (when (:timeout options)
         (. url-object (setReadTimeout (:timeout options))))
       (. url-object connect)
       (let [is (. url-object (getContent))
             br (new java.io.BufferedReader
                     (new java.io.InputStreamReader is))]
         [(read-buffer br)
          url-object]))))


(defn http-req
  "Makes a single HTTP get request and returns the
   read data and the connection object"
  ([method uri headers parameters options]
     (let [url (make-url uri parameters)
           url-object (doto
                        (. url (openConnection))
                        (.setRequestMethod method)
                        (.setDoInput true)
                        (.setDoOutput true))]
       (when (:timeout options)
         (. url-object (setReadTimeout (:timeout options))))
       (. url-object connect)
       (let [is (. url-object (getContent))
             br (new java.io.BufferedReader
                     (new java.io.InputStreamReader is))]
         [(read-buffer br)
          url-object]))))


;; Jena functions


(defn parse-model [xml]
     (let [model (. ModelFactory createDefaultModel)]
       (. model (read (new java.io.ByteArrayInputStream (. xml getBytes)) nil))))


(defn parse-states [vars]
  (loop [remaining vars
         statements  (set [])]
    (if (empty? remaining)
      statements
      (let [itm (first remaining)
            id (. itm (substring 1 (. itm length)))]
        (recur (rest remaining)
               (clojure.set/union statements (set [id])))))))


(defn add-to-model-solution [var solution model]
  (let [s (str "s" var)
        p (str "p" var)
        o (str "o" var)
        sol-s (. solution (get s))
        sol-p (. solution (get p))
        sol-o (. solution (get o))
        stm (. model (createStatement
                      sol-s
                      (. ResourceFactory (createProperty (.toString sol-p)))
                      (if (. sol-o isLiteral)
                        (. model (createLiteral (.toString sol-o)))
                        sol-o)))]
    (. model (add stm))))


(defn model-from-result [result]
  (let [vars (. (. result getResultVars) toArray)
        states (parse-states vars)
        model (. ModelFactory createDefaultModel)]
    (loop [continue (. result hasNext)]
      (if continue
        (let [sol (. result next)]
          (loop [remaining states]
            (if (empty? remaining)
              model
              (do
                (add-to-model-solution (first remaining)
                                       sol
                                       model)
                (recur (rest remaining))))))
        model))))


(defn add-to-bindings-solution [vars solution]
  (loop [remaining vars
         acum      {}]
    (if (empty? remaining)
      acum
      (recur (rest remaining)
             (conj acum {(keyword (first remaining)) (. solution (get (first remaining)))})))))


(defn bindings-from-result [result]
  (let [vars (. result getResultVars)]
    (loop [continue (. result hasNext)
           bindings []]
      (if continue
        (let [sol (. result next)]
          (recur (. result hasNext)
                 (conj bindings (add-to-bindings-solution vars sol))))
        bindings))))


;; Calculus functions


(def *ts* (ref {}))


(defn pi-declare-ts [name]
  (dosync
   (alter *ts* (fn [ts] (conj ts {name (ref (. ModelFactory createDefaultModel))})))))


(defn pi-ts [name]
  (name @*ts*))


(defn pi-ts-write [name model]
  (dosync
   (alter (pi-ts name)
          (fn [old-model]
            (. old-model (union model))))))


(defn pi-get [resource params]
  (let [triples (nth (http-res "GET"  resource {} params {}) 0)]
    (parse-model triples)))


(defn pi-get-block [resource params secs]
  (try
   (let [triples (nth (http-res "GET" resource {} (conj {:xblocking "true"} params) {:timeout (* secs 1000)}) 0)]
     (parse-model triples))
   (catch Exception e (. ModelFactory createDefaultModel))))


(defn pi-get-subscribe [resource params]
  (let [triples (nth (http-res "GET" resource {} (conj {:xsubscribe "true"} params) {}) 0)]
    (parse-model triples)))


(defn pi-post [resource params]
  (let [triples (nth (http-req "POST"  resource {} params {}) 0)]
    (parse-model triples)))


(defmulti pi-bind (fn [query model] (class model)))

(defmethod pi-bind  Model [query model]
  (let [q-obj (. QueryFactory (create query))
        q-eng (. QueryExecutionFactory (create query model))
        results (. q-eng execSelect)]
    (bindings-from-result results)))


(defmulti pi-match (fn [query model] (class model)))

(defmethod pi-match  Model [query model]
  (let [q-obj (. QueryFactory (create query))
        q-eng (. QueryExecutionFactory (create query model))
        results (. q-eng execConstruct)]
    results))


(defn pi-ts-in [name query]
  (dosync
   (let [read-model (pi-match query @(pi-ts name))]
     (alter (pi-ts name)
            (fn [old-model]
              (loop [model old-model
                     remaining (. (. read-model listStatements) toList)]
                (if (empty? remaining)
                  model
                  (recur (. model (remove (first remaining)))
                         (rest remaining))))))
     read-model)))


(defn pi-ts-read [name query]
  (pi-match query @(pi-ts name)))


;; Calculus declarations


(defmacro pi-declare-process [ & equation]
  `(fn []
     (do
       ~@equation)))