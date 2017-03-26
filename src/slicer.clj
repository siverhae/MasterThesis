(ns slicer
  (require [pdgCreator :as pdg] )
  (:import [yoshikihigo.tinypdg.ast.TinyPDGASTVisitor]
           [yoshikihigo.tinypdg.scorpio.PDGGenerationThread]))

(def myast (pdg/get-java-files  "/Users/Silke/Downloads/JHotDraw51"))

;;"/Users/Silke/Documents/thesis/PDG"
;/Users/Silke/Downloads/JHotDraw51
;;outputfile is the file we want to output to.

(defn slicer[inputfile outputfile SIZE_THRESHOLD NUMBER_OF_THREADS] 
  (let* [useOfControl true
         useOfData true
         useOfExecution false
         useOfMerging false
         methods (pdg/get-ast inputfile)
         treeset (new java.util.TreeSet)
         pdgs (java.util.Collections/synchronizedSortedSet treeset)
         cfgNodeFactory (new yoshikihigo.tinypdg.cfg.node.CFGNodeFactory)
         pdgNodeFactory (new yoshikihigo.tinypdg.pdg.node.PDGNodeFactory)
         pdgGenerationThreads (java.lang.reflect.Array/newInstance java.lang.Thread NUMBER_OF_THREADS)]
    (letfn [(threadCreator [index]
              (let [pdgGenerationThread (new yoshikihigo.tinypdg.scorpio.PDGGenerationThread  methods pdgs cfgNodeFactory pdgNodeFactory useOfControl useOfData useOfExecution useOfMerging SIZE_THRESHOLD)
                    thread (new java.lang.Thread pdgGenerationThread)]    
                (java.lang.reflect.Array/set pdgGenerationThreads index thread)      
                (.start thread))) 
            (joinThreads [index]
              (let [thread (java.lang.reflect.Array/get pdgGenerationThreads index )]
                (try
                  (.join thread)
                  (catch InterruptedException e (.printStackTrace e )))
                ))
            ]
      (println "generating PDGs .. ") 
      (doall (for [i (range NUMBER_OF_THREADS)] (threadCreator i)))
      (doall (for [i (range NUMBER_OF_THREADS)] (joinThreads i)))
      (let* [pdgArray (.toArray pdgs (java.lang.reflect.Array/newInstance yoshikihigo.tinypdg.pdg.PDG 0 ))
             treeMap1 (new java.util.TreeMap )
             mappingPDGToPDGNodes  (java.util.Collections/synchronizedSortedMap treeMap1)
             treeMap2 (new java.util.TreeMap )
             mappingPDGToPDGEdges (java.util.Collections/synchronizedSortedMap treeMap2)
             hashCalculationThreads (java.lang.reflect.Array/newInstance java.lang.Thread NUMBER_OF_THREADS)]
        (letfn [ (hashThreadCreator [index] 
                   (let* [hashCalculationThread (new yoshikihigo.tinypdg.scorpio.HashCalculationThread pdgArray  mappingPDGToPDGNodes mappingPDGToPDGEdges)
                          thread (new java.lang.Thread hashCalculationThread)]
                     (java.lang.reflect.Array/set hashCalculationThreads index thread) 
                     (.start thread)))
                
                (hashJoinThreads [index]
                  (let [thread (java.lang.reflect.Array/get hashCalculationThreads index )]
                    (try
                      (.join thread)
                      (catch InterruptedException e (.printStackTrace e )))
                    ))]
          (println "done generating PDG's")
          (newline)
        ;  (println "calculating hash values ... ")
        ;  (doall (for [i (range NUMBER_OF_THREADS)] (hashThreadCreator i)))
        ;  (doall (for [i (range NUMBER_OF_THREADS)] (hashJoinThreads i)))
        ;  (println "done calculating hash values")
          (count pdgs) 
          )))))



