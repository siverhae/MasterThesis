(ns pdgCreator
  (:require [clojure.java.io :as io])
  (:import [yoshikihigo.tinypdg.ast.TinyPDGASTVisitor]))

;;/Users/Silke/Downloads/JHotDraw51

(defn get-java-files [dir]
  (filter #(.endsWith (.getPath %) ".java") (file-seq (io/file dir))))

(defn get-ast [path]
  (let [methods (new java.util.ArrayList)
        java-files (get-java-files path)]
    (letfn [(process-file [file]
              (let [unit (yoshikihigo.tinypdg.ast.TinyPDGASTVisitor/createAST file)
                    filepath (.getAbsolutePath file)
                    visitor (new yoshikihigo.tinypdg.ast.TinyPDGASTVisitor
                                 filepath unit methods)]
                (.accept unit visitor)))]
      (doall (map process-file java-files))
      methods)))

(defn getMethodSignature [method]
  (let* [text  (.name method)
         text2 (str text " <")
         text3 (str text2 (.startLine method))
         text4 (str text3 "...")
         text5 (str text4 (.endLine method))
         text6 (str text5 ">")]
    text6))

(defn buildPDG [methods]
  (with-open [writer (clojure.java.io/writer "/Users/Silke/Documents/thesis/PDG" :append true)]
    (let [createdGraphNumber (atom 0) ]
      (letfn [(writePDG [pdg])     
              (create-pdg [method]
                (let [ PDGNodeFactory (new yoshikihigo.tinypdg.pdg.node.PDGNodeFactory)
                      CFGNodeFactory (new yoshikihigo.tinypdg.cfg.node.CFGNodeFactory)
                      pdg (new yoshikihigo.tinypdg.pdg.PDG method PDGNodeFactory CFGNodeFactory true true false)]
                  (.build pdg)
                  (reset! createdGraphNumber (+ 1 @createdGraphNumber))
                  (let* [method (.unit pdg )
                         signature (getMethodSignature method)
                         nodeLabels (new java.util.HashMap)]
                    (letfn [(putLabels [node]
                              (.put nodeLabels node (.size nodeLabels)))
                            ( putEntry [entry] 
                              (let* [key (.getKey entry)
                                     keytext (.getText key)
                                     replacedOne (clojure.string/replace keytext   "\"" "\\\"")
                                     replacedTwo (clojure.string/replace replacedOne "\\\\\""  "\\\\\\\"")
                                     node (.getKey entry)]
                                (.write writer (str @createdGraphNumber))
                                (.write writer ".")
                                (.write writer (str (.getValue entry)))
                                (.write writer " [style = filled, label = \"")
                                (.write writer replacedTwo)
                                (.write writer"\"")
                                
                                (cond
                                  (instance?  yoshikihigo.tinypdg.pdg.node.PDGMethodEnterNode node) (.write writer ", fillcolor = aquamarine")
                                  (.contains (.getExitNodes pdg) node) (.write writer ", fillcolor = deeppink")
                                  (instance?  yoshikihigo.tinypdg.pdg.node.PDGParameterNode node)  (.write writer ", fillcolor = tomato")
                                  :else (.write writer ", fillcolor = white"))
                                
                                (cond 
                                  (instance?  yoshikihigo.tinypdg.pdg.node.PDGControlNode node) (.write writer ", shape = diamond")
                                  (instance?  yoshikihigo.tinypdg.pdg.node.PDGParameterNode node) (.write writer ", shape = box")
                                  :else (.write writer ", shape = ellipse"))
                                
                                (.write writer "];")
                                (.newLine writer)))
                            (edges [edge] 
                              (.write writer (str @createdGraphNumber))
                              (.write writer ".")
                              (.write writer  (str (.get nodeLabels(.fromNode edge))))
                              (.write writer " -> ")
                              (.write writer (str @createdGraphNumber))
                              (.write writer ".")
                              (.write writer (str (.get nodeLabels (.toNode edge))))
                              (cond 
                                (instance?  yoshikihigo.tinypdg.pdg.edge.PDGDataDependenceEdge edge) (.write writer (str " [style = solid, label=\""  (.getDependenceString edge)  "\"]"))
                                (instance?  yoshikihigo.tinypdg.pdg.edge.PDGControlDependenceEdge edge) (.write writer (str " [style = dotted, label=\"" (.getDependenceString edge)  "\"]"))
                                (instance?  yoshikihigo.tinypdg.pdg.edge.PDGExecutionDependenceEdge edge) (.write writer (str " [style = bold, label=\"" (.getDependenceString edge)  "\"]")))
                              (.write writer ";")
                              (.newLine writer))]
                      (.write writer "subgraph cluster")
                      (.write writer (str @createdGraphNumber))
                      (.write writer " {")
                      (.newLine writer)
                      (.write writer "label = \"")
                      (.write  writer signature)
                      (.write writer "\";")
                      (.newLine writer)
                      
                      (doall (map putLabels (.getAllNodes pdg)))
                      (doall (map putEntry (.entrySet nodeLabels)))
                      (doall (map edges (.getAllEdges pdg)))
                      
                      (.write writer "}")
                      (.newLine writer))))
                
                )]
        (print "building and outputting PDGs ..") 
        (.write writer "digraph {")
        (.newLine writer)
        (doall (map create-pdg methods))
        (.write writer "}")
        (.close writer))
      (print "Sucessfully finished"))))







