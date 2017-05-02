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

(defn  markVerticesOfSlices [pdg nodesUnderInvestigation typeOfSlicing]
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
                        (if (=  (.getText  nodeUnderInvestigation) (.getText nodeOfpdg))
                        (.setMarked nodeOfpdg)
                        nil))]
               (doall (map comparison nodes)))))]
    
    (loop [workList nodesUnderInvestigation]  
      

      (if (not (empty? workList))   
        (let [nodeUnderInvestigation (first workList)]
          (letfn [ (addEdgesAndFilterMarked [nodesUnderInvestigation nodeUnderInvestigation]
                     (let [edges (.getBackwardEdges nodeUnderInvestigation)
                           forwardEdges (.getForwardEdges nodeUnderInvestigation)]
                       (letfn [(fromNode [edge]
                                 (.fromNode edge))
                               (notMarked? [node]
                                 (not(.isMarked node)))
                               (toNode [edge]
                                 (.toNode edge))]
                         (if (not (nil? edges))
                          (if (= typeOfSlicing "forward")
                            (doall (doall (filter notMarked?  (into workList (doall (map toNode forwardEdges))))))
                            (doall (doall (filter notMarked?  (into workList (doall (map fromNode edges)))))))
                           (doall (filter notMarked? workList)))
                         )))]
            ;we mark the node 
            (.setMarked nodeUnderInvestigation)
            (setOtherNode pdg nodeUnderInvestigation)
            
            ;now we need to find unmarked nodes such that edge w -> v 
            ;since we filter on marked node we know that the first node will be deleted too.
            (recur 
              (addEdgesAndFilterMarked workList nodeUnderInvestigation ))
            ))
            
        (let [nodes (getAllMarkedNodes pdg)]
        (deleteAllMarks pdg)
       nodes))
      )
      ))


(defn backwardsAndForwardsSlicing [pdg nodesUnderInvestigation]
 (print (namesOfNodes (distinct (into (markVerticesOfSlices pdg nodesUnderInvestigation "forward")
(markVerticesOfSlices pdg nodesUnderInvestigation "backwards"))))))

(defn nth-treeset [coll index]
  (com.google.common.collect.Iterables/get coll index))


(def myfiles (pdg/get-java-files  "/Users/Silke/Downloads/JHotDraw51"))
(def myMethods (pdg/get-ast  "/Users/Silke/Downloads/JHotDraw51"))
(def mypdg (createpdg (second myMethods)))
(def nodes (.getAllNodes mypdg))
(def pdg8 (createpdg (nth-treeset myMethods 8)))
(def nodes8 (.getAllNodes pdg8))





