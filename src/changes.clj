(ns changes
  (:require [slicerNotMultithreaded :as slicer] )
  (:require [pdgCreator :as pdg] )
  (:require [clojure.core.logic :as logic])
  (:require [damp.ekeko.jdt.astnode :as astnode])
  (:require [damp.ekeko.logic :as el])
  (:require [qwalkeko.clj.logic :as l])
  (:require [qwalkeko.clj.ast :as ast])
  (:require [damp.qwal :as qwal])
  (:require [qwalkeko.clj.graph :as graph])
  (:require [qwalkeko.clj.functionalnodes :as changes])
  (:require [qwalkeko.clj.changenavigation :as nav])
  (:require [damp.ekeko.jdt
             [ast :as jdt]])
  (:require [clojure.java.io :as io])
  (:import [yoshikihigo.tinypdg.ast.TinyPDGASTVisitor])
  (:import [yoshikihigo.tinypdg.pe.MethodInfo]))


;Hierboven imports
;-------------------------------------------------------------------------------------

(defn a-model [ ] (first (filter #(instance? qwalkeko.HistoryProjectModel %)
                           (damp.ekeko.ekekomodel/all-project-models))))

(defn a-graph [a-model] (graph/convert-model-to-graph a-model))

(def model (a-model))
(def graph (a-graph model))

;gives us back the version belonging to version-Number
;example of VersionNumber = "b6d6656db556ab1d9f7a212a30200765f7653ae5"
(defn find-version [versionNumber a-graph] 
  (first 
    (filter 
      #(= (graph/revision-number %) 
         versionNumber) (:versions a-graph))))


;We want to find the predecesor of a version in order to figure out what changes happened.
(defn get-pred-version  [version] 
  (first (graph/predecessors version)))


; function that returns the absolutePath of a version
(defn getAbsolutePath [version wantedFile] 
  (let [eclipseVersion (graph/eclipse-project version)
        ;(first (graph/file-infos version)) = file
        filename (:file  wantedFile)
        file (.getFile eclipseVersion filename)
        fullPath (.getFullPath file)
        javaFile (.toFile fullPath)]
    (.getName javaFile)))


(defn createMethods [unit path] 
  (let [methods (new java.util.ArrayList)
        visitor  (new yoshikihigo.tinypdg.ast.TinyPDGASTVisitor path unit methods)]
    (.accept unit visitor)
    methods))

;(defn findMethod [methods]
;  (map (fn[method] (print (.getName method))) methods))


(defn method-name [method]
  (ast/has-clj-unwrapped :identifier (ast/has-clj-unwrapped :name method)))

(defn findMethod [methods method]
  (if (not (nil? method))
    (let [methodName (method-name method)]
      (loop [methods methods]
        (if (empty? methods)
          nil
          (let [currentMethod (first methods)]
            (if (= (.getName currentMethod) methodName)
              currentMethod
              (recur (next methods)))))))
    false))

(defn createpdg [method]
  (when (not (nil? method))
    (slicer/createpdg method)))


(defn encompassing-method [change]
  (logic/run 1 [?parent]
    (logic/fresh [?node]
      (changes/change-original change ?node)
      (jdt/ast-parent+ ?node ?parent)
      (jdt/ast :MethodDeclaration ?parent))))


(defn findEncompassingMethod  [change]
  (list change (first (encompassing-method change ))))

(defn encompassing-type [change]
  (logic/run 1 [?parent]
    (logic/fresh [?node]
      (changes/change-original change ?node)
      (jdt/ast-parent+ ?node ?parent)
      (jdt/ast :TypeDeclaration ?parent))))


(defn findEncompassingType  [change]
  (list change (first (encompassing-type change ))))


(defn getOriginalChangeDeclarative [change]
  (logic/run 1 [?original]
    (changes/change-original change ?original)))


(defn getOriginalChange [change]
  (:original change))


(defn groupMethodRelatedChanges [listOfChangesAndEncompassingMethods]
  (loop [methodRelatedChanges (hash-map)
         listOfChangesAndEncompassingMethod listOfChangesAndEncompassingMethods]
    (if (empty? listOfChangesAndEncompassingMethod)
      methodRelatedChanges
      (let [changeAndEncompassingMethod (first listOfChangesAndEncompassingMethod)
            newMethodRelatedChanges 
            (if (empty? methodRelatedChanges)
              (assoc methodRelatedChanges (second changeAndEncompassingMethod) (list(first changeAndEncompassingMethod)))
              (if (contains? methodRelatedChanges (second changeAndEncompassingMethod))
                (assoc methodRelatedChanges (second changeAndEncompassingMethod) (cons (first changeAndEncompassingMethod) (get methodRelatedChanges (second changeAndEncompassingMethod)))) 
                (assoc methodRelatedChanges (second changeAndEncompassingMethod) (list (first changeAndEncompassingMethod)))))]
        (recur newMethodRelatedChanges
          (next listOfChangesAndEncompassingMethod))))))


(defn createPairCouples [listOfElems]
  (loop 
    [firstElem 0 
     resList (list)]
    (if (>= firstElem (count listOfElems))
      resList
      (recur (+ firstElem 1)
        (into resList
          (loop [secondElem (+ 1 firstElem )
                 resListTemp (list)]
            (if (>= secondElem (count listOfElems))
              resListTemp
              (recur (+ 1 secondElem)
                (conj resListTemp (list (nth listOfElems firstElem) (nth  listOfElems secondElem)))))))))))


(defn hashmapPair [groupedChanges]
  (let [methods (keys groupedChanges)]
    (loop [methods (keys groupedChanges)
           hashmapPaired (hash-map)]
      (if (empty? methods)
        hashmapPaired
        (let [method (first methods)
              value (get groupedChanges method)]
          (recur (next methods)
            (assoc hashmapPaired method (createPairCouples value))))))))


(defn node-is-comment? [node]  
  (when (and (not (nil? node)) (instance? org.eclipse.jdt.core.dom.ASTNode node)) 
    (let [id (.getNodeType node)]
      (or 
        (= id org.eclipse.jdt.core.dom.ASTNode/BLOCK_COMMENT)
        (= id org.eclipse.jdt.core.dom.ASTNode/JAVADOC)
        (= id org.eclipse.jdt.core.dom.ASTNode/LINE_COMMENT)
        (= id org.eclipse.jdt.core.dom.ASTNode/TEXT_ELEMENT)
        (= id org.eclipse.jdt.core.dom.ASTNode/TAG_ELEMENT)
        (recur (.getParent node))))))

(defn change-is-comment? [change]
  (node-is-comment? (:original change)))

(defn node-is-not-comment? [node]
  (not (node-is-comment? node)))


(defn change-is-not-comment? [change]
  (node-is-not-comment? (:original change)))

(defn notKeyNil [pair] 
  (not (nil? (key pair))))

(defn loopOverSlicerTillWeFindCorrectNode [pdg originalfirst]
  (loop [nodeUnderInvestigation originalfirst]
    (let [ resultSlicer (slicer/backwardsAndForwardsSlicing pdg (list nodeUnderInvestigation))]
    (if (not (empty? resultSlicer))
      resultSlicer
      (recur (.getParent nodeUnderInvestigation))))))
  

(defn partOf [node markedNodes]
  (loop [workList markedNodes
         bool false] 
    (if (empty? workList)
      bool
      (let [nodeUnderInvestigation (first workList)
            originalNode (clojure.string/trim-newline(.toString node))
            textNodeUnderInvestigation (.getText nodeUnderInvestigation)]
        (if (.contains textNodeUnderInvestigation originalNode )
         (do (println textNodeUnderInvestigation) (println originalNode)(println true))
         (do (println  textNodeUnderInvestigation) (println originalNode)(println "gy")))
        (recur (next workList)
          bool)))))

;We want to see if changes (represented by a versionNumber) is an atomic commit or not
(defn changesSlicer [versionNumber a-model a-graph]
  (let  [version (find-version versionNumber a-graph)
         ;and it's predecessor
         pred-version (get-pred-version version)
         ; then the next step is for each of the files that are changed we check the changes.
         units (l/qwalkeko* [?left ?right ?file]
                 (qwal/qwal a-graph version pred-version []
                   (l/in-source-code [curr]
                     (l/fileinfo|edit ?file curr)
                     (l/fileinfo|compilationunit ?file ?right curr))
                   qwal/q<=
                   (l/in-source-code [curr]
                     (l/fileinfo|compilationunit ?file ?left curr))))]
    (letfn [(unitWork [unit]
              (let [pred (first unit)
                    current (second unit)
                    file (last unit)
                    pathOfPred (getAbsolutePath pred-version file)
                    pathOfCurrent (getAbsolutePath version file)
                    changes (changes/get-ast-changes pred current)
                    listOfChanges (filter change-is-not-comment? (:changes changes))
                    listOfChangesAndEncompassingMethod  (doall (map findEncompassingMethod listOfChanges))
                    groupedChanges  (groupMethodRelatedChanges listOfChangesAndEncompassingMethod)
                    groupedPairedChanges (hashmapPair groupedChanges)
                    methods (createMethods pred pathOfPred)]
                (letfn[(partOfSlice [key]
                         (let [vals (get groupedPairedChanges key)
                               method (findMethod methods key)
                               pdg (if method (createpdg method) false)
                               ]
                           (letfn [(forEachPairOfChanges[pairOfChanges]
                                     (let [firstChange (first pairOfChanges)
                                           originalfirst (getOriginalChange firstChange)
                                           secondChange (second pairOfChanges)
                                           originalsecond (getOriginalChange secondChange)
                                           markedNodesFirst (loopOverSlicerTillWeFindCorrectNode pdg originalfirst)
                                           markedNodesSecond (loopOverSlicerTillWeFindCorrectNode pdg originalsecond)
                                           secondPartOfFirst (partOf originalsecond markedNodesFirst)
                                    
                                           ]

                                       ))]
                             (doall (map forEachPairOfChanges vals))
                             ;   (pdg/buildPDG (list method))
                             )))] 
                  (doall (map partOfSlice (keys groupedPairedChanges)))
                  
                  
                  )))
            ]
      (doall (map unitWork units)))))


(defn forEachPairOfChanges[pairOfChanges]
  (let [firstChange (first pairOfChanges)
        originalfirst (getOriginalChange firstChange)
        secondChange (second pairOfChanges)
        originalsecond (getOriginalChange secondChange)]
    (println "new")
    (println originalfirst)
    (println originalsecond)
    pairOfChanges))

