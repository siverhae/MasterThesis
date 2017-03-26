(ns slicerNotMultithreaded
  (require [pdgCreator :as pdg] )
  (:import [yoshikihigo.tinypdg.ast.TinyPDGASTVisitor]
           [yoshikihigo.tinypdg.pdg.node]))



(defn namesOfNodes[nodes]
  (letfn [(printname[node]
            (print (.getText node)))]
    (doall (map printname nodes))))

(defn createpdg [method]
  (let [ PDGNodeFactory (new yoshikihigo.tinypdg.pdg.node.PDGNodeFactory)
        CFGNodeFactory (new yoshikihigo.tinypdg.cfg.node.CFGNodeFactory)
        ;the last false is so we no longer have execution dependency edges
        pdg (new yoshikihigo.tinypdg.pdg.PDG method PDGNodeFactory CFGNodeFactory true true false)]
    (.build pdg)
    pdg)) 


;pdg is your program dependence graph 
;nodesUnderInvestigation is a list of all the nodes that we want to uses as a criterium to slice

(defn  markVerticesOfSlices [pdg nodesUnderInvestigation]
  (letfn [     
          ( getAllMarkedNodes [pdg]
            (let [nodes (.getAllNodes pdg)]
              (letfn [(marked? [node]
                        (.isMarked node))]
                (doall (filter marked? nodes)))))     
          
          (deleteAllMarks [pdg]
            (let [nodes (.getAllNodes pdg)]
              (doall (map (fn [x] (.setNOTMarked x)) nodes))))
          (setOtherNode [pdg  nodeUnderInvestigation]
            (let [nodes (.getAllNodes pdg)]
              (letfn [(comparison [nodeOfpdg]
                        (if (.compareTo  nodeUnderInvestigation nodeOfpdg)
                          (.setMarked nodeOfpdg)
                          nil))]
                (map comparison nodes))))]
    
    (loop [workList nodesUnderInvestigation]  
      
      (when (empty? workList)
        (print (getAllMarkedNodes pdg))
        (deleteAllMarks pdg))
      
      (when (not (empty? workList))   
        (let [nodeUnderInvestigation (first workList)]
          (letfn [ (addEdgesAndFilterMarked [nodesUnderInvestigation nodeUnderInvestigation]
                     (let [edges (.getBackwardEdges nodeUnderInvestigation)]
                       (letfn [(toNode [edge]
                                 (.fromNode edge))
                               (notMarked? [node]
                                 (not(.isMarked node)))
                               ]
                         (if (not (nil? edges))
                           (doall (filter notMarked?  (into workList (doall (map toNode edges)))))
                           (doall (filter notMarked? workList)))
                         )))]
            ;we mark the node 
            (.setMarked nodeUnderInvestigation)
            (setOtherNode pdg nodeUnderInvestigation)
            
            ;now we need to find unmarked nodes such that edge w -> v 
            ;since we filter on marked node we know that the first node will be deleted too.
            (recur 
              (addEdgesAndFilterMarked workList nodeUnderInvestigation ))
            )))
      )))





(def myfiles (pdg/get-java-files  "/Users/Silke/Downloads/JHotDraw51"))
(def myMethods (pdg/get-ast  "/Users/Silke/Downloads/JHotDraw51"))
(def mypdg (createpdg (second myMethods)))
(def nodes (.getAllNodes mypdg))




