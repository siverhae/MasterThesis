
(def a-model (first (damp.ekeko.ekekomodel/all-project-models)))
(def a-graph (graph/convert-model-to-graph a-model))
(def a-root (first (:roots a-graph)))

(defn le-version (first 
                   (filter 
                     #(= (graph/revision-number %) 
                        "b6d6656db556ab1d9f7a212a30200765f7653ae5") (:versions a-graph))))

(def le-pred (first (graph/predecessors le-version)))

(graph/file-infos le-version)

;;geeft compilation units terug
(l/qwalkeko 1 [?left ?right]
  (qwal/qwal a-graph le-pred le-version [?file ]
    (l/in-source-code [curr]
      (l/fileinfo|edit ?file curr)
      (l/fileinfo|compilationunit ?file ?left curr))
    qwal/q=>
    (l/in-source-code [curr]
      (l/fileinfo|compilationunit ?file ?right curr))))


(l/qwalkeko 1 [?file]
  (qwal/qwal a-graph le-pred le-version []
    (l/in-source-code [curr]
      (l/fileinfo|edit ?file curr))
     qwal/q=>
    (l/in-source-code [curr]
      )))


         pred (first units)
         current (second units)
         file (last units)
         ;we get the compulationUnit of our predecessor
        
         methods (create-methodsArray compilationUnit)
         methodsPrev (create-methodsArray compilationUnitPred)]
    ;we  now have a list of all the methods of that complilation unit
    ;we could create pdg's of all the methods 
    
       methods)
  )
    



#<Metaversion-b6d6656db556ab1d9f7a212a30200765f7653ae5>
=> (graph/file-infos le-version)
({:status :edit, :file "src_new/org/argouml/uml/ui/ActionRemoveFromModel.java"})
=> (graph/eclipse-project le-version)
#<Project P/repo_17_06_2010.git-b6d6656db556ab1d9f7a212a30200765f7653ae5>
=> (type *1)
org.eclipse.core.internal.resources.Project
=> (.getFile (graph/eclipse-project le-version) (:file (first (graph/file-infos le-version))))
#<File L/repo_17_06_2010.git-b6d6656db556ab1d9f7a212a30200765f7653ae5/src_new/org/argouml/uml/ui/ActionRemoveFromModel.java>
=> (.getFullPath *1)
#<Path /repo_17_06_2010.git-b6d6656db556ab1d9f7a212a30200765f7653ae5/src_new/org/argouml/uml/ui/ActionRemoveFromModel.java>
=> (.toFile *1)
#<File /repo_17_06_2010.git-b6d6656db556ab1d9f7a212a30200765f7653ae5/src_new/org/argouml/uml/ui/ActionRemoveFromModel.java>
=> (type *1)
java.io.File


  (let [eclipseVersion (do (print (graph/eclipse-project version)) (graph/eclipse-project version))
        ;(first (graph/file-infos version)) = file
        filename (:file  wantedFile)
        file (.getFile eclipseVersion filename)
        fullPath (.getFullPath file)
        javaFile (.toFile fullPath)]
    javaFile))
  
  
  
  (defn encompassing-method [change]
  (logic/run 1 [?parent]
    (logic/fresh [?node]
        (changes/change-original change ?node)
        (jdt/ast-parent+ ?node ?parent)
        (jdt/ast :MethodDeclaration ?parent))))
(map
  (fn [change]
    {change
      (first (logic/run 1 [?parent]
               (encompassing-method change ?parent)))))
  changes)
  
  
  (defn groupMethodRelatedChanges [listOfChangesAndEncompassingMethods]
  (loop [methodRelatedChanges (hash-map)
         listOfChangesAndEncompassingMethod listOfChangesAndEncompassingMethods]
    (if (empty? listOfChangesAndEncompassingMethod)
      methodRelatedChanges
      (let [changeAndEncompassingMethod (first listOfChangesAndEncompassingMethod)
            newMethodRelatedChanges 
            (if (empty? methodRelatedChanges)
              (do (println "assoc1") (println (second changeAndEncompassingMethod) )(assoc methodRelatedChanges (second changeAndEncompassingMethod) (list(first changeAndEncompassingMethod))))
              (if (contains? methodRelatedChanges (second changeAndEncompassingMethod))
                (do (println "assoc2") (println (second changeAndEncompassingMethod)) (assoc methodRelatedChanges (second changeAndEncompassingMethod) (cons (first changeAndEncompassingMethod) (get methodRelatedChanges (second changeAndEncompassingMethod))))) 
                (do (println "assoc3") (println (second changeAndEncompassingMethod)) (assoc methodRelatedChanges (second changeAndEncompassingMethod) (list (first changeAndEncompassingMethod))))))]
        (recur newMethodRelatedChanges
          (next listOfChangesAndEncompassingMethod))))))
  
  
 markedNode (while 
                                                        (empty? (slicer/backwardsAndForwardsSlicing pdg (list originalfirst)))
                                                        (print "slicer")
                                                        (print (slicer/backwardsAndForwardsSlicing pdg (list originalfirst)) nil)
                                                        (slicer/backwardsAndForwardsSlicing pdg (list (.getParent originalfirst))))
                                           ]
                        
                        


