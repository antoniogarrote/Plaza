(ns tail
  (:use clj-pi-calculus)
  (:import (java.io BufferedInputStream IOException FileInputStream File)
           (java.net HttpURLConnection URI URL URLEncoder)
           (com.hp.hpl.jena.rdf.model Model ModelFactory Resource Property RDFNode ResourceFactory)
           (java.util UUID)
           (com.hp.hpl.jena.graph.query Query)
           (com.hp.hpl.jena.query QueryFactory QueryExecutionFactory)))


(defn run [filename application language ts-name]
  (let [fl (. (new File filename) length)
        fis (new FileInputStream filename)
        _   (. fis (skip fl))
        bis (new BufferedInputStream fis)
        formater (new java.text.SimpleDateFormat "E, dd MMM yyyy HH:mm:ss")]
    (loop [offset 0]
      (let [l (. bis available)]
        (if (= l -1)
          (throw (new IOException "file ended"))
          (if (= l 0)
            (recur offset)
            (let [data (do (println (str "."))
                           (byte-array l))]
              (if (= (. bis (read data 0 l)) -1)
                (do ;;(println "EOF??")
                    (recur (+ offset l)))
                (do
                  (loop [lines (. java.util.Arrays (asList (. (new String data) (split "\n"))))
                         m (. ModelFactory createDefaultModel)]
                    (if (empty? lines)
                      (when (> (. m size) 0)
                        (pi-ts-write ts-name m))
                      (do
                         (when (re-find #"(ERROR|error|Error)" (first lines))
                          (println "!")
                          (let [r (. m (createResource (str "http://anonymous.exception.com#" (. (. UUID randomUUID) toString))))
                                dt (re-find #"\w\w\w, \d\d \w\w\w \d\d\d\d \d\d:\d\d:\d\d" (first lines))]
                            (if (nil? dt)
                              (. r (addProperty (. m (createProperty "http://anonymous.exception.com#" "createdAt")) (. (new java.util.Date) toString)))
                              (. r (addProperty (. m (createProperty "http://anonymous.exception.com#" "createdAt")) (. (. formater (parse dt)) toString))))
                            (. r (addProperty (. m (createProperty "http://anonymous.exception.com#" "language")) (. m (createTypedLiteral language))))
                            (. r (addProperty (. m (createProperty "http://anonymous.exception.com#" "level")) (. m (createTypedLiteral "ERROR"))))
                            (. r (addProperty (. m (createProperty "http://anonymous.exception.com#" "message")) (. m (createTypedLiteral (first lines)))))
                            (. m (add (. r listProperties)))))
                        (recur (rest lines)
                               m))))
                  (. Thread (sleep 10000))
                  (recur offset))))))))))

(defn exception-bind-pattern [] "prefix ex: <http://anonymous.exception.com#> select ?s ?c ?lg ?lv ?m where { ?s ex:createdAt ?c . ?s ex:language ?lg . ?s ex:level ?lv . ?s ex:message ?m } LIMIT 1")
(defn exception-match-pattern [] "prefix ex: <http://anonymous.exception.com#> construct { ?s ex:createdAt ?c . ?s ex:language ?lg . ?s ex:level ?lv . ?s ex:message ?m } where { ?s ex:createdAt ?c . ?s ex:language ?lg . ?s ex:level ?lv . ?s ex:message ?m } LIMIT 1")

(defn encode-predicate [pred model]
  (. URLEncoder (encode (. model (shortForm (. pred getURI))))))

(defn subject-to-params [res model]
  (let [stmts (.(. res listProperties) toList)
        props (map (fn [st]  (str (encode-predicate (. st getPredicate) model)  "=" (. URLEncoder (encode (.(. st getObject) toString))) )) stmts)
        propquery (reduce (fn [acum i] (str acum "&" i)) "" props)]
    (. propquery (substring 1 (. propquery length)))))

(defn declare-namespaces [ns]
  (loop [ks (keys ns)
         m  (. ModelFactory createDefaultModel)]
    (if (empty? ks)
      m
      (recur (rest ks)
             (. m (setNsPrefix (first ks) (get ns (first ks))))))))