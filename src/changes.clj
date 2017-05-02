(ns changes
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
             [ast :as jdt]]))

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
    javaFile))


;we have to give a compilationUnit as parameter
(defn create-methodsArray [unit]
  (let [methods (new java.util.ArrayList)
        filepath (getAbsolutePath unit)
        visitor (new yoshikihigo.tinypdg.ast.TinyPDGASTVisitor
                  filepath unit methods)]
    (.accept unit visitor)
    methods))

(defn encompassing-method [change]
  (logic/run 1 [?parent]
    (logic/fresh [?node]
      (changes/change-original change ?node)
      (jdt/ast-parent+ ?node ?parent)
      (jdt/ast :MethodDeclaration ?parent))))

(defn encompassing-type [change]
  (logic/run 1 [?parent]
    (logic/fresh [?node]
      (changes/change-original change ?node)
      (jdt/ast-parent+ ?node ?parent)
      (jdt/ast :TypeDeclaration ?parent))))

(defn findEncompassingType  [change]
  (list change (first (encompassing-type change ))))

(defn findEncompassingMethod  [change]
  (println "change")
  (println change)
  (println "method")
  (println (first (encompassing-method change )))
  (list change (first (encompassing-method change ))))




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


(defn createpairseCouples [listOfElems]
  (let [listOfCouples (list)]
  (for [firstElem (range 0 (- (count listOfElems) 1))]
    for [secondElem (range firstElem (- (count listOfElems) 1))]
    
  (* x x))

  (defn createpairseCouples [listOfElems]
    (loop 
      [firstElem 0 
      resList (list)]
      (if (>= firstElem (count listOfElems))
        resList
     (recur (+ firstElem 1)
            (conj resList
            (loop [secondElem firstElem 
                   resListTemp (list)]
              (if (>= secondElem (count listOfElems))
                resListTemp
                (recur (+ 1 secondElem)
                        (conj resListTemp (cons (nth listOfElems firstElem) (nth  listOfElems secondElem)))))))))))
        

 
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
                    listOfChanges (:changes changes)
                    listOfChangesAndEncompassingMethod  (doall (map findEncompassingMethod listOfChanges))
                    groupedChanges (groupMethodRelatedChanges listOfChangesAndEncompassingMethod)]
                
           ; (println groupedChanges)
           ; (println (first (keys groupedChanges)))
           
            (list changes)
                ))
            ]
      (doall (map unitWork units)))))





